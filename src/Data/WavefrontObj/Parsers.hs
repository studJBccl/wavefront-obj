{-# LANGUAGE OverloadedStrings #-}
module Data.WavefrontObj.Parsers (vertexPointParser, textureCoordParser, vertexNormalParser, faceParser, objFileParser) where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Data.Attoparsec.Text
import           Data.Maybe
import           Data.WavefrontObj.Types
import           Linear
import           Linear.Affine

vertexPointParser :: WavefrontParser ()
vertexPointParser = do
    lift $ char 'v'
    lift skipSpace
    x <- lift double
    lift skipSpace
    y <- lift double
    lift skipSpace
    z <- lift double
    lift (skipWhile $ not . isEndOfLine)
    appendVertexPoint $ P (V3 x y z)

textureCoordParser :: WavefrontParser ()
textureCoordParser = do
    lift $ string "vt"
    lift skipSpace
    u <- lift double
    lift skipSpace
    v <- lift double
    lift (skipWhile $ not . isEndOfLine)
    appendTextureCoordinate $ P (V2 u v)

vertexNormalParser :: WavefrontParser ()
vertexNormalParser = do
    lift $ string "vn"
    lift skipSpace
    x <- lift double
    lift skipSpace
    y <- lift double
    lift skipSpace
    z <- lift double
    lift (skipWhile $ not . isEndOfLine)
    appendVertexNormal $ V3 x y z

faceParser :: WavefrontParser WavefrontFace
faceParser = do
    lift $ char 'f'
    lift $ skipSpace
    vertices <- (vertexWithPointTextureCoordNormal <|> vertexWithPointNormal <|> vertexWithPointTextureCoord <|> vertexWithPoint) `sepBy` lift space
    lift (skipWhile $ not . isEndOfLine)
    return $ WavefrontFace vertices
    where
        vertexWithPoint :: WavefrontParser WavefrontVertex
        vertexWithPoint = do
            vertexPointIdx <- lift $ signed decimal

            vertexPoint <- getVertexPoint vertexPointIdx
            return $ WavefrontVertex vertexPoint Nothing Nothing
        vertexWithPointTextureCoord :: WavefrontParser WavefrontVertex
        vertexWithPointTextureCoord = do
            vertexPointIdx <- lift (signed decimal)
            lift $ char '/'
            textureCoordinateIdx <- lift (signed decimal)

            vertexPoint <- getVertexPoint vertexPointIdx
            textureCoordinate <- getTextureCoordinate textureCoordinateIdx
            return $ WavefrontVertex vertexPoint (Just textureCoordinate) Nothing
        vertexWithPointNormal :: WavefrontParser WavefrontVertex
        vertexWithPointNormal = do
            vertexPointIdx <- lift (signed decimal)
            lift $ string "//"
            vertexNormalIdx <- lift (signed decimal)

            vertexPoint  <- getVertexPoint vertexPointIdx
            vertexNormal <- getVertexNormal vertexNormalIdx
            return $ WavefrontVertex vertexPoint Nothing (Just vertexNormal)
        vertexWithPointTextureCoordNormal :: WavefrontParser WavefrontVertex
        vertexWithPointTextureCoordNormal = do
            vertexPointIdx <- lift (signed decimal)
            lift $ char '/'
            textureCoordinateIdx <- lift (signed decimal)
            lift $ char '/'
            vertexNormalIdx <- lift (signed decimal)

            vertexPoint <- getVertexPoint vertexPointIdx
            textureCoordinate <- getTextureCoordinate textureCoordinateIdx
            vertexNormal <- getVertexNormal vertexNormalIdx
            return $ WavefrontVertex vertexPoint (Just textureCoordinate) (Just vertexNormal)

objFileParser :: WavefrontParser WavefrontModel
objFileParser = do
    faces <- ((vertexPointParser  *> pure Nothing) 
          <|> (textureCoordParser *> pure Nothing)
          <|> (vertexNormalParser *> pure Nothing)
          <|> (commentParser      *> pure Nothing)
          <|> (Just <$> faceParser))
          `sepBy` (lift $ some endOfLine)
    return $ WavefrontModel (catMaybes faces)
    where
        commentParser = lift $ char '#' *> (skipWhile $ not . isEndOfLine)
