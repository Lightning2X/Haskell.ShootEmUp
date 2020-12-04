module Main where

import           Controller
import           FileManager
import           Model
import           Renderer
import           Constants
import           Graphics.Gloss.Interface.IO.Game

{-This function starts the game by loading a tuple that contains the name of the first stage and 
the names of all the other stages-}
main :: IO ()
main = do getStages <- loadStages 
          gameLoop getStages

{-the gameloop. The -}
gameLoop:: (String, [String]) -> IO()
gameLoop getStages = playIO (InWindow "ShootEmUp" (screenWidth, screenHeight) (0, 0)) -- Sets the window with a default size
                             black                    -- Background color
                             60                       -- Frames per second
                             (beginState getStages)   -- Initial state (needs the stages)
                             view                     -- View function
                             input                    -- Event function
                             step                     -- Step function
