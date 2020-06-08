{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Achille.Thumbnail
    ( FitType(..)
    , Thumbnail(..)
    , saveThumbnailTo
    , downscaleToFit
    ) where


import Data.Binary         (Binary, put, get)
import Codec.Picture
import Codec.Picture.Extra (scaleBilinear)

import Achille.Recipe


data FitType
    = FitWidth  Int
    | FitHeight Int
    -- | FitBoth Int Int

data Thumbnail = Thumbnail
    { thumbWidth  :: Int
    , thumbHeight :: Int
    , thumbPath   :: FilePath
    } deriving (Eq, Ord)

instance Binary Thumbnail where
    put (Thumbnail w h p) = put w >> put h >> put p
    get                   = Thumbnail <$> get <*> get <*> get


saveThumbnailTo :: PngSavable pixel
                => (FilePath -> FilePath)
                -> Image pixel
                -> Recipe FilePath Thumbnail
saveThumbnailTo mod img@Image{..} =
    Thumbnail imageWidth imageHeight <$> saveTo mod img


downscaleToFit :: ( Pixel a
                  , Bounded  (PixelBaseComponent a)
                  , Integral (PixelBaseComponent a)
                  )
               => FitType
               -> Image a
               -> Image a
downscaleToFit fit img@(Image{..}) =
    let width  = imageWidth
        height = imageHeight
        (width', height') = case fit of
            FitWidth  wbound | wbound < width ->
                (wbound, round $ (fi height) * (fi wbound) / (fi width))
            FitHeight hbound | hbound < height ->
                (round $ (fi width) * (fi hbound) / (fi height), hbound)
    in scaleBilinear width' height' img
    where fi = fromIntegral
