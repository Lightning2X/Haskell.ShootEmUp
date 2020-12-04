module RNG where

import           System.Random

--The lowest and highest seed that are allowed
minSeed :: Int
minSeed = 1
maxSeed :: Int
maxSeed = 999999

--Default random number generator, only used as a dummy when first creating game states etc.
defaultGen :: StdGen
defaultGen = mkStdGen 777

--Creates a random integer between two values (e.g. between 1 and 2)
randomInt :: StdGen -> Int -> Int -> Int
randomInt r min max = getNum $ randomR (min,max) r
   where getNum (x, _) = x
--Creates a random float between two values (e.g. between 1.0 and 2.0)
randomFloat :: StdGen -> Float -> Float -> Float
randomFloat r min max = getNum $ randomR (min, max) r
   where getNum (x, _) = x
--Refreshes the seed of the generator
makeNewRNG :: IO StdGen
makeNewRNG = do seed <- getStdRandom (randomR (minSeed,maxSeed))
                return (mkStdGen seed)