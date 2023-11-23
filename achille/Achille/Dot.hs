{-# LANGUAGE OverloadedStrings #-}
-- | Components for outputing a program as a Graphviz graph
module Achille.Dot where

import Prelude hiding ((.), id, lookup)
import Data.Text (unpack)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.IO qualified as Text
import Data.Map.Strict (Map)

import Data.Text.Lazy.Builder qualified as Builder
import Data.Map.Strict qualified as Map

import Achille.Core.Program hiding (Env, lookupEnv, bindEnv)
import Achille.Core.Recipe

-- NOTE(flupe):
-- This graph rendering was hacked together and should be rewritten at some point
-- Also, once we actually care about recursive tasks, we will have to prevent looping here
-- (With StableNames or something)

outputGraph :: Maybe FilePath -> Program m a -> IO ()
outputGraph output p = do
  print p
  let dot = toLazyText $ build p
  case output of
    Just file -> Text.writeFile file dot
    Nothing   -> Text.putStrLn dot

data Env = Env
  { theEnv :: Map Int Int
  , size   :: Int
  }

lookupEnv :: Int -> Env -> Int
lookupEnv k (Env env _) = env Map.! k

bindEnv :: Env -> Int -> Env
bindEnv (Env env s) id = Env (Map.insert s id env) (s + 1)

build :: Program m a -> Builder
build p =
  let ((b, _), _) = renderP (buildGraph p) (Env Map.empty 0) 1
   in "digraph {" <> b <> "}"

newtype ProgramRenderer b = PR
  { renderP :: Env -> Int -> (b, Int)
  } deriving (Functor)

instance Applicative ProgramRenderer where
  pure x = PR \_ nextId -> (x, nextId)
  mf <*> mx = PR \env nextId ->
    let (f, nextId')  = renderP mf env nextId
        (x, nextId'') = renderP mx env nextId'
    in (f x, nextId'')

instance Monad ProgramRenderer where
  mx >>= f = PR \env nextId ->
    let (x, nextId') = renderP mx env nextId
    in renderP (f x) env nextId'

lookup :: Int -> ProgramRenderer Int
lookup k = PR \env nextId -> (lookupEnv k env, nextId)

bind :: Int -> ProgramRenderer a -> ProgramRenderer a
bind v p = PR \env nextId -> renderP p (bindEnv env v) nextId

newNode :: ProgramRenderer Int
newNode = PR \_ nextId -> (nextId, nextId + 1)

buildGraph :: Program m a
           -> ProgramRenderer (Builder, Int)
buildGraph p = case p of
  Var k -> ("",) <$> lookup k

  Seq x y -> do
    (bx,  _) <- buildGraph x
    (by, oy) <- buildGraph y
    pure (bx <> by, oy)

  Fail _ -> ("",) <$> newNode

  Bind x y -> do
    (bx, ox) <- buildGraph x
    (by, oy) <- bind ox $ buildGraph y
    pure (bx <> by, oy)

  Val _ -> do
    out <- newNode
    pure (Builder.fromString $ show out <> "[label=\"value\"];", out)

  Apply r x -> do
    (bx, outx) <- buildGraph x
    (bf, outf) <- buildRecipe outx r
    pure (bx <> bf, outf)

  Pair x y -> do
    (bx, ox) <- buildGraph x
    (by, oy) <- buildGraph y
    out      <- newNode
    pure ( bx <>
           by <> Builder.fromString 
             ( show ox <> "->" <> show out <> " [arrowhead=none];" <>
               show oy <> "->" <> show out <> " [arrowhead=none];" <>
               show out <> "[shape=\"point\"];")
         , out)

  _ -> ("",) <$> newNode

buildRecipe :: Int -> Recipe m a b -> ProgramRenderer (Builder, Int)
buildRecipe input r = case r of
  Id -> pure ("", input)
  Embed name _ -> do
    box <- newNode
    pure (Builder.fromString $ show input <> "->" <> show box <> ";" <>
                               show box <> "[shape=\"box\"label=\"" <> unpack name <> "\"];"
         , box)
  f :***: g -> do
    splitInput <- newNode
    joinOutput <- newNode
    (bf, outf) <- buildRecipe splitInput f
    (bg, outg) <- buildRecipe splitInput g
    let binput = Builder.fromString  $
                     show input <> "->" <> show splitInput <> " [arrowhead=none];"
                  <> show outf  <> "->" <> show joinOutput <> " [arrowhead=none];"
                  <> show outg  <> "->" <> show joinOutput <> " [arrowhead=none];"
                  <> show splitInput <> "[shape=\"point\"];"
                  <> show joinOutput <> "[shape=\"point\"];"
    pure (binput <> bf <> bg, joinOutput)
  f :&&&: g -> do
    joinOutput <- newNode
    (bf, outf) <- buildRecipe input f
    (bg, outg) <- buildRecipe input g
    let binput = Builder.fromString  $
                     show outf  <> "->" <> show joinOutput <> " [arrowhead=none];"
                  <> show outg  <> "->" <> show joinOutput <> " [arrowhead=none];"
                  <> show joinOutput <> "[shape=\"point\"];"
    pure (binput <> bf <> bg, joinOutput)
  Comp g f -> do
    (bf, outf) <- buildRecipe input f
    (bg, outg) <- buildRecipe outf g
    pure (bf <> bg, outg)
  _        -> pure ("", input)
