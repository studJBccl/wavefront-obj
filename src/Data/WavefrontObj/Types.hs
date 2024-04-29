{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.WavefrontObj.Types (
    WavefrontParser
  , runWavefrontParser
  , debugWavefrontParser
  , WavefrontVertex(..)
  , WavefrontFace'(..)
  , WavefrontFace
  , WavefrontModel'(..)
  , WavefrontModel
  , getVertexPoint
  , appendVertexPoint
  , getTextureCoordinate
  , appendTextureCoordinate
  , getVertexNormal
  , appendVertexNormal
  ) where

import           Control.Applicative
import           Control.Monad.Trans.State.Strict
import           Data.Attoparsec.Text
import           Data.Sequence                    ((|>))
import qualified Data.Sequence                    as S
import qualified Data.Text                        as T
import           Linear.Affine
import           Linear.V2
import           Linear.V3

type WavefrontParser a = StateT WavefrontState Parser a

runWavefrontParser :: WavefrontParser a -> T.Text -> Either String a
runWavefrontParser p = parseOnly (evalStateT p emptyWavefrontState)

debugWavefrontParser :: WavefrontParser a -> T.Text -> Either String (a, WavefrontState)
debugWavefrontParser p = parseOnly (runStateT p emptyWavefrontState)

data WavefrontState = WavefrontState {
                      _vertexPoints       :: !(S.Seq (Point V3 Double))
                    , _textureCoordinates :: !(S.Seq (Point V2 Double))
                    , _vertexNormals      :: !(S.Seq (V3 Double))
                    }
                    deriving (Show, Eq)

emptyWavefrontState :: WavefrontState
emptyWavefrontState = WavefrontState (S.empty) (S.empty) (S.empty)

{-|
    Contains the data for an individual vertex, with the world-space coordinate, (optional) texture-space coordinate, and the (optional) precalculated normal vector.

    TODO: Add an option to add normals for models without them(?).
-}
data WavefrontVertex = WavefrontVertex {
                      _vertexPoint       :: !(Point V3 Double)
                    , _textureCoordinate :: !(Maybe (Point V2 Double))
                    , _vertexNormal      :: !(Maybe (V3 Double))
                    }
                    deriving (Show, Eq)

-- | The underlying datatype to store the vertices for a face.
newtype WavefrontFace' a = WavefrontFace [a]
                         deriving (Show, Eq, Functor, Foldable)

-- | An alias to make the the type signatures look nicer while still allowing the instances that you would want (without pulling in mono-traversable).
type WavefrontFace = WavefrontFace' WavefrontVertex

-- | Models are not exactly complicated in this system, just a list of faces.
newtype WavefrontModel' a = WavefrontModel [a]
                          deriving (Show, Eq, Functor, Foldable)

-- | Another alias to beautify type signatures.
type WavefrontModel = WavefrontModel' WavefrontFace

--

getVertexPoint :: (MonadFail m) => Int -> StateT WavefrontState m (Point V3 Double)
getVertexPoint idx' = do
    vertexPoints <- _vertexPoints <$> get
    let idx = if idx' > 0
        then idx'
        else idx' + (S.length vertexPoints) + 1
    if (idx > S.length vertexPoints) || (idx <= 0)
        then fail   $ "Invalid model! Vertex point at index " ++ show idx' ++ " doesn't exist."
        else return $ S.index vertexPoints (idx - 1)

appendVertexPoint :: (Monad m) => Point V3 Double -> StateT WavefrontState m ()
appendVertexPoint p = do
    curState <- get
    put $ curState { _vertexPoints = (_vertexPoints curState) |> p }

--

getTextureCoordinate :: (MonadFail m) => Int -> StateT WavefrontState m (Point V2 Double)
getTextureCoordinate idx' = do
    textureCoordinates <- _textureCoordinates <$> get
    let idx = if idx' > 0
        then idx'
        else idx' + (S.length textureCoordinates) + 1
    if (idx > S.length textureCoordinates) || (idx <= 0)
        then fail   $ "Invalid model! Texture coordinate at index " ++ show idx' ++ " doesn't exist."
        else return $ S.index textureCoordinates (idx - 1)

appendTextureCoordinate :: (Monad m) => Point V2 Double -> StateT WavefrontState m ()
appendTextureCoordinate t = do
    curState <- get
    put $ curState { _textureCoordinates = (_textureCoordinates curState) |> t }

--

getVertexNormal :: (MonadFail m) => Int -> StateT WavefrontState m (V3 Double)
getVertexNormal idx' = do
    vertexNormals <- _vertexNormals <$> get
    let idx = if idx' > 0
        then idx'
        else idx' + (S.length vertexNormals) + 1
    if (idx > S.length vertexNormals) || (idx <= 0)
        then fail   $ "Invalid model! Vertex normal at index " ++ show idx' ++ " doesn't exist."
        else return $ S.index vertexNormals (idx - 1)

appendVertexNormal :: (Monad m) => V3 Double -> StateT WavefrontState m ()
appendVertexNormal n = do
    curState <- get
    put $ curState { _vertexNormals = (_vertexNormals curState) |> n }
