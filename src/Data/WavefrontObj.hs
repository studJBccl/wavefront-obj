{-|
Module      : Data.WavefrontObj
Description : Wavefront .obj 3D model loading microlibrary.
Copyright   : (c) Alexis Williams 2016
License     : BSD3
Maintainer  : sasinestro@gmail.com
Stability   : experimental
A (very) minimal microlibrary to load 3D geometry from Wavefront .obj files.
-}
module Data.WavefrontObj (
    -- * Types
      WavefrontVertex(..)
    , WavefrontFace'(..), WavefrontFace
    , WavefrontModel'(..), WavefrontModel
    -- * Interface
    , parseWavefrontObj, loadWavefrontObj
    ) where

import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Data.WavefrontObj.Parsers
import           Data.WavefrontObj.Types

{-|
    Takes a 'Text' and gives you either an error message or a 'WavefrontModel' containing the geometry data from the input.
-}
parseWavefrontObj :: T.Text -> (Either String WavefrontModel)   
parseWavefrontObj = runWavefrontParser objFileParser

{-|
    A convienence function for the common case of loading a model from a text file on disk.
-}
loadWavefrontObj :: FilePath -> IO (Either String WavefrontModel)
loadWavefrontObj = fmap parseWavefrontObj . T.readFile
