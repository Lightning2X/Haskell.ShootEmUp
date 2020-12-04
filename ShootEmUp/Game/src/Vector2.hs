module Vector2 where

import Constants 
import Data.Fixed

--The position and vector datatype (represents a point in a coordinatesystem and the vector is a movement)
data Position = Pos Float Float
    deriving (Show, Eq, Ord, Read)
data Vector2  = Vec Float Float
    deriving (Show, Eq, Ord, Read)

--Default vector
zeroVector :: Vector2
zeroVector = Vec 0.0 0.0

--Allows vectors to work like normal numbers
instance Num Vector2 where
    negate v                  = zipVec v negate
    (Vec x1 y1) + (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)
    (Vec x1 y1) * (Vec x2 y2) = Vec (x1 * x2) (y1 * y2)
    fromInteger int           = Vec (fromIntegral int) (fromIntegral int)
    abs v                     = zipVec v abs
    signum v                  = zipVec v signum
--Appy a function to a vector
zipVec :: Vector2 -> (Float -> Float) -> Vector2
zipVec (Vec x y) f = Vec (f x) (f y)
--Normalise a vector
normalise :: Vector2 -> Vector2
normalise (Vec 0.0 0.0) = zeroVector
normalise (Vec x y)     = Vec (x / norm) (y / norm)
  where norm = sqrt (x*x + y*y)
--Multiply a vector with a certain number
mult :: Float -> Vector2 -> Vector2
mult scale (Vec x y) = Vec (x*scale) (y*scale)
--Create a unitvector
unitVector :: Float -> Vector2 -> Vector2
unitVector scale vec = mult scale $ normalise vec
vecLength :: Vector2 -> Float
vecLength (Vec x y) = sqrt (x * x + y * y)
vecDist :: Vector2 -> Vector2 -> Float
vecDist (Vec x1 y1) (Vec x2 y2)  = sqrt $ xsqr + ysqr
  where xsqr = (x2 - x1) * (x2 - x1)
        ysqr = (y2 - y1) * (y2 - y1)
--Convert an angle to a vector
degreeToVector :: Float -> Vector2
degreeToVector input = Vec vX vY
  where angle = mod' input 360.0
        vX = cos (dTr * angle)
        vY = sin (dTr * angle)


--Convert position to a vector from the origin.
toVec :: Position -> Vector2
toVec (Pos x y) = Vec x y

--Translates a position with a vector to a new position
translatePos :: Position -> Vector2 -> Position
translatePos (Pos x y) (Vec x1 y1) = Pos (x+x1) (y+y1)