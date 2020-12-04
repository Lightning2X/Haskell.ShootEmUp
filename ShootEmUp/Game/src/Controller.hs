-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import           Model
import           UpdateLogic
import           Graphics.Gloss.Interface.IO.Game
import           RNG
import           GameStateHelper
import           Constants
import           Vector2
import           GameOverModule
import           BeginModule

{- Handle one iteration of the game.
To make the framerate smoother, the game is only updated after
an x amount of seconds. 
-}
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = --Create a new rng, updatethe background and update the game
    do newRNG <- makeNewRNG
       let newGstate = updateBG gstate
       let newkState = updateGame newGstate
       return $ newkState { elapsedTime = 0, rng = newRNG }
  | otherwise
  = -- Just update the elapsed time
    do newRNG <- makeNewRNG
       return $ gstate { elapsedTime = elapsedTime gstate + secs,  rng = newRNG }


-- | Handle user input
input :: Event -> GameState -> IO GameState
--This event handles when the event that the user changes the screen size. 
--The screenScale will be equal to the windowsize divided by the resolution.
input (EventResize (width, height)) gstate = return gstate{screenScale = (fromIntegral width/ fromIntegral screenWidth,
                                                                          fromIntegral height/ fromIntegral screenHeight)}
{-This handles the event when the user moves the mouse.
The mouse coordinates are translated so that they can be used by the game. 
The coordinates are translated because the coordinatesystem in the game, starts in the bottomleft and 
not in the centrum. Since the screen can also be resized, the mousepositions needs to be translated as well. 
Therefore the mouseposition is also "scaled".  -}
input (EventMotion (x,y)) gstate@GameState {screenScale = (_, yScale)} = return gstate{mousePos = (x,(1/yScale) * y +(fromIntegral screenHeight /2))}
input e gstate = inputKey e gstate

--Every gamestate requires a different input. The gameover gamestate unfortunately requires some IO (read, write score).
--Therefore this function already converts the gamestate to an io gamestate.
inputKey :: Event -> GameState -> IO GameState
inputKey e gstate@GameState {infoToShow = i} = case i of
    (Begin _ _)        -> handleBeginInput e gstate
    (Running _)        -> return $ handleRunningInput e gstate
    (Paused _)         -> return $ handlePauseInput e gstate
    (GameOver (w, _))  -> handleGOInput e w gstate

{- this function handels input in the menu. The player can navigate with w and s.
If the player presses <Enter> a level is selected.-}
handleBeginInput:: Event -> GameState -> IO GameState
handleBeginInput (EventKey (Char c) Down _ _) gstate = case c of
  'w' -> return $ switchLevelMenu gstate  PreviousLevel
  's' -> return $ switchLevelMenu gstate  NextLevel
  _   -> return gstate
handleBeginInput (EventKey (SpecialKey c) Down _ _) gstate = case c of
  KeyEnter -> switchToGame gstate
  _        -> return gstate
handleBeginInput _ gstate = return gstate

{-Handle the input in a stage.
The player speed is set in a certain direction when w or s is pressed.
If no directionkey is pressed, the direction is reset to (0,0).
The player can switch to mouse controls with m and can shoot with q(hold q to shoot)
or by holding the left mouse button.
The player switches to pause with <SPACE> or <ENTER>.  
 -}
handleRunningInput :: Event -> GameState -> GameState
handleRunningInput (EventKey (Char c) Down _ _) gstate@GameState {useMouse = m} = case c of
             's' -> setPlayerSpeed (Vec 0.0 (-1.0)) gstate
             'w' -> setPlayerSpeed (Vec 0.0 1.0 ) gstate
             'q' -> setPlayerShooting True gstate
             'm' -> gstate{useMouse = not m}
             _   -> gstate
handleRunningInput (EventKey (Char c) Up _ _) gstate = case c of
             's' -> setPlayerSpeed zeroVector gstate
             'w' -> setPlayerSpeed zeroVector gstate
             'q' -> setPlayerShooting False gstate
             _   -> gstate
handleRunningInput (EventKey (SpecialKey c) Down _ _) gstate = case c of
             KeySpace -> switchToPause gstate
             KeyEnter -> switchToPause gstate
             _        -> gstate
handleRunningInput (EventKey (MouseButton LeftButton) Down _ _) gstate =
             setPlayerShooting True gstate
handleRunningInput (EventKey (MouseButton LeftButton) Up _ _) gstate =
             setPlayerShooting False gstate
handleRunningInput _ gstate = gstate-- Otherwise reset player speed so that he doesnt hover

--Toggle to not pause with <ENTER>, <PAUSE> or leftmousebutton
handlePauseInput:: Event -> GameState -> GameState
handlePauseInput (EventKey (SpecialKey c) Down _ _) gstate = case c of
             KeySpace -> switchToRun gstate
             KeyEnter -> switchToRun gstate
             _        ->  gstate
handlePauseInput (EventKey (MouseButton LeftButton) Down _ _) gstate =
             switchToRun gstate
handlePauseInput _ gstate =  gstate-- Otherwise reset player speed so that he doesnt handlePauseInput

{-This is the input fpr the gameover state
If the player has lost, he can start again with <Enter> or leftmousebutton.
If the player has won, there are two menu's with different input.
First the player has to pick a name. This can be done by typing characters.
A space adds a space, tab is a backspace and enter confirms the name. 
Once the player has selected a name, the player can press <ENTER> to go 
to the next level. -}
handleGOInput:: Event -> Won -> GameState -> IO GameState
handleGOInput (EventKey (SpecialKey c) Down _ _) (HasWon Inserted _) gstate = case c of
             KeyEnter -> switchToNextLevel gstate
             _        -> return gstate
handleGOInput (EventKey (MouseButton LeftButton) Down _ _) (HasWon Inserted _) gstate =
             switchToNextLevel gstate
handleGOInput (EventKey (SpecialKey c) Down _ _) (HasWon NotInserted n) gstate = case c of
             KeySpace -> return $ changeName (n++"_") gstate
             KeyEnter -> case n of
              "" -> return gstate
              _  -> handleGameOver (getName newState) newState
                where newState = setToInserted gstate
             KeyTab -> return $ changeName (decreaseString n) gstate
             _      -> return gstate
handleGOInput (EventKey (MouseButton LeftButton) Down _ _) (HasWon NotInserted n) gstate =
             case n of
              "" -> return gstate
              _  -> handleGameOver (getName newState) newState
                where newState = setToInserted gstate
handleGOInput (EventKey (Char c) Down _ _) (HasWon NotInserted n) gstate =
                         return $ changeName (n++[c]) gstate
handleGOInput (EventKey (SpecialKey c) Down _ _) HasLost gstate = case c of
             KeyEnter -> reloadLevel gstate
             _        -> return gstate
handleGOInput (EventKey (MouseButton LeftButton) Down _ _) HasLost gstate =
             reloadLevel gstate
handleGOInput _ _ gstate = return gstate-- Otherwise reset player speed so that he doesnt handlePauseInput



