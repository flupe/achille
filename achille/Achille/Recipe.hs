{-# LANGUAGE GADTs, ApplicativeDo, RecordWildCards #-}
module Achille.Recipe
  ( Context(..)
  , Recipe(Id)
  , Embedded(..)
  , embed
  , exl
  , exr
  , void
  , runRecipe
  , readText
  , readByteString
  , write
  )
  where

import Prelude hiding ((.), id, log)
import Control.Monad (when)
import Control.Category
import Control.Arrow
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.FilePath ((</>))

import Achille.Cache
import Achille.Diffable
import Achille.IO as AIO
import Achille.Writable (Writable)
import Achille.Writable qualified as Writable


-- | Context in which tasks and recipes are run.
data Context = Context
  { lastTime   :: UTCTime  -- ^ Time of the last run.
  , currentDir :: FilePath -- ^ Current directory used for glob patterns
  , inputRoot  :: FilePath
  , outputRoot :: FilePath
  }

-- | A recipe is a glorified Kleisli arrow, computing a value of type @b@ in some monad @m@
--   given some input of type @a@ and a context. The recipe has access to a local cache,
--   preserved between runs.
data Recipe m a b where
  Id      :: Recipe m a a
  Comp    :: Recipe m b c -> Recipe m a b -> Recipe m a c
  (:***:) :: Recipe m a b -> Recipe m c d -> Recipe m (a, c) (b, d)
  (:&&&:) :: Recipe m a b -> Recipe m a c -> Recipe m a (b, c)
  Exl     :: Recipe m (a, b) a
  Exr     :: Recipe m (a, b) b
  Void    :: Recipe m a ()
  Embed   :: {-# UNPACK #-} !(Embedded m a b) -> Recipe m a b


instance Show (Recipe m a b) where
  show Id = "Id"
  show (Comp g f) = "Comp (" <> show g <> ") (" <> show f <> ")"
  show (f :***: g) = "(" <> show f <> ") :***: (" <> show g <> ")"
  show (f :&&&: g) = "(" <> show f <> ") :&&&: (" <> show g <> ")"
  show Exl = "Exl"
  show Exr = "Exr"
  show Void = "Void"
  show (Embed e) = "Embed " <> show (rName e)


exl :: Recipe m (a, b) a
exl = Exl
{-# INLINE exl #-}

exr :: Recipe m (a, b) b
exr = Exr
{-# INLINE exr #-}

void :: Recipe m a ()
void = Void
{-# INLINE void #-}

-- TODO(flupe): find a better name for this?
data Embedded m a b = Embedded
  { rName    :: !String
  , runEmbed :: Context -> Cache -> Value a -> m (Value b, Cache) 
  }


embed :: Embedded m a b -> Recipe m a b
embed = Embed
{-# INLINE embed #-}


-- TODO(flupe): maybe do the simplification step earlier? (at compile-time?)
runRecipe
  :: Monad m 
  => Context -> Cache -> Value a -> Recipe m a b -> m (Value b, Cache)
runRecipe ctx cache v r = case r of
  Id -> pure (v, cache)

  Comp g f -> do
    let (cf, cg) = splitCache cache
    (y, cf) <- runRecipe ctx cf v f
    (z, cg) <- runRecipe ctx cg y g
    pure (z, joinCache cf cg)

  f :***: g -> do -- TODO(flupe): parallelism
    let (cf, cg) = splitCache cache
        (x, y) = splitPair v
    (z, cf) <- runRecipe ctx cf x f
    (w, cg) <- runRecipe ctx cg y g
    pure (joinPair z w, joinCache cf cg)

  f :&&&: g -> do -- TODO(flupe): parallelism
    let (cf, cg) = splitCache cache
    (z, cf) <- runRecipe ctx cf v f
    (w, cg) <- runRecipe ctx cg v g
    pure (joinPair z w, joinCache cf cg)

  Exl  -> pure (fst $ splitPair v, cache)
  Exr  -> pure (snd $ splitPair v, cache)
  Void -> pure (unit, cache)

  Embed r -> runEmbed r ctx cache v


instance Category (Recipe m) where
  id = Id
  {-# INLINE id #-}

  -- simplify morphisms using category laws
  Comp g f    . h           = g . (f . h) -- right-nested composition chain
  Id          . f           = f
  g           . Id          = g
  (p :***: q) . (f :***: g) = (p . f) :***: (q . g)
  (p :***: q) . (f :&&&: g) = (p . f) :&&&: (q . g)
  -- Exl         . (f :&&&: g) = f
  -- Exr         . (f :&&&: g) = g
  -- Exl         . (f :***: g) = f . Exl
  -- Exr         . (f :***: g) = g . Exr
  g           . f           = Comp g f
  {-# INLINE (.) #-}


instance Applicative m => Arrow (Recipe m) where
  arr f = embed Embedded
    { rName = ""
    , runEmbed = \ctx cache v@(x, _) -> pure (value (f x) (hasChanged v), cache)
    } -- TODO
  {-# INLINE arr #-}
  first f = f :***: id
  {-# INLINE first #-}
  second f = id :***: f
  {-# INLINE second #-}
  (***) = (:***:)
  {-# INLINE (***) #-}
  (&&&) = (:&&&:)
  {-# INLINE (&&&) #-}

-- | Read text from file.
readText :: (Applicative m, AchilleIO m) => Recipe m FilePath Text
readText = Embed $ Embedded
  { rName = "readText"
  , runEmbed = \Context{..} cache v@(src, _) -> do
      let path = inputRoot </> src
      mtime <- getModificationTime path
      txt   <- decodeUtf8 <$> AIO.readFile path
      pure (value txt (hasChanged v || mtime > lastTime), cache)
  }

-- | Read a bytestring from file.
readByteString :: (Applicative m, AchilleIO m) => Recipe m FilePath ByteString
readByteString = Embed $ Embedded
  { rName = "readText"
  , runEmbed = \Context{..} cache v@(src, _) -> do
      let path = inputRoot </> src
      mtime <- getModificationTime path
      txt   <- AIO.readFile path
      pure (value txt (hasChanged v || mtime > lastTime), cache)
  }

-- | Write something to file, /iff/ this thing has changed since the last run.
write
  :: (Monad m, AchilleIO m, Writable m a)
  => Recipe m (FilePath, a) FilePath
write = Embed $ Embedded
  { rName = "write"
  , runEmbed = \Context{..} cache v@((src, x), _) -> do
      let path = outputRoot </> src
      let vsrc = fst (splitPair v)
      when (hasChanged v) $ log ("Writing " <> path) *> Writable.write path x
      pure (vsrc, cache)
  }
