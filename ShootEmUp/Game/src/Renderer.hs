-- | This module defines how to turn
--   the game state into a picture
module Renderer where

import           Graphics.Gloss
import           Model
import           Objects
import           Constants

--Draws the game
view :: GameState -> IO Picture
view = return . viewPure

{-This function checks the gamestate and then draws that gamestate. 
 Then the picture is scaled according to the windowsize-}
viewPure :: GameState -> Picture
viewPure gstate@GameState {screenScale = (x, y), mousePos = (_, ym)} = scaleToScreenSize $ case infoToShow gstate of
   Begin  i is     -> drawBegin i is
   Running i       -> drawGameInfo i
   Paused i        -> pictures [drawGameInfo i, drawPause]
   GameOver (w, i) -> drawGameOver w (map generateDrawScore $ scores $ stage i)
  where scaleToScreenSize p | renderDebug = scale x y  $ pictures [ p, translate 0 200 $ drawText (show ym)]
                            | otherwise   = scale x y p


--------------------------------------------BEGIN Draw methods
{-This function draws the beginning state. That is the menu. It draws the list of all the stages and an arrow at the selected stage. 
The picture is then translated to the topleft.-}
drawBegin:: String -> [String] -> Picture
drawBegin selected st = translate (- fromIntegral screenWidth/2.0) (fromIntegral screenHeight/2.0 -20) $ 
                            pictures [translate 55 0 $ drawTextSpaced st, translate 0 (arrowPos st 10) drawArrow]
  where arrowPos [] yPos = yPos + 20 --Translates arrow to the position of the selected item, 20 is the fontheight
        arrowPos (x:xs) yPos | x == selected = yPos
                             | otherwise     = arrowPos xs yPos-20

--This function draws a green arrow (is used in the menu)
drawArrow:: Picture
drawArrow = color green $ Line [(0,0),(50,0), (35,10), (50,0), (35, -10)]


-------------------------------------------RUN Draw methods
{-Draws the game in the running and pausestate.
First the picture is determined. This picture contains the background, objects and ui in order.
Then the picture is translated from a centred coordinatesystem to a coordinatesystem in the
bottomleftcorner. -}
drawGameInfo:: GameInfo -> Picture
drawGameInfo GameInfo {stage = s, score = sc, player = p, enemies = e, bullets = b, powerups = pow} 
  =  translate (- fromIntegral screenWidth/2.0) (-fromIntegral screenHeight/2.0) $ 
              pictures ((render $ backGround s) : drawWorld e b pow p  ++ drawUi s p sc)

{-Draws all enemies, bullets and the player.-}
drawWorld:: [Enemy] -> [Bullet] -> [PowerUp] -> Player -> [Picture]
drawWorld e b pow p = render p : (map render e ++ map render b ++ map render pow)

{-Draws the ui. This contains the StageName, playerlifes, currentweapon and the score.
These things are converted to a nice looking string. This string is translated because
otherwise the string would be above the screen (0,0). This string is drawn on top of a bar.
This bar is drawn with a border and is as wide as the screen and 26 pixels in height.-}
drawUi:: Stage -> Player -> Score ->[Picture]
drawUi Stage{sName = stageName} p s = [drawScreenBorder] ++ drawBar ++ [pictureText]
    where pictureText = translate 0 (fromIntegral screenHeight -20) $ drawText finalText
          finalText = setTextSize 20 stageName ++  " | Lifes = " ++ show (pLifes p) ++ " | " ++
                       setTextSize 20 (show (pWeapon p)) ++ " | " ++ "WeaponLevel = " ++ show (pWeaponLevel p) ++
                        " | " ++ "Score = " ++ show s

--Draws a border around the entire screen.
drawScreenBorder::Picture
drawScreenBorder = color (greyN 0.3) $ Line borderPoints
  where borderPoints = boxPoints 0 1 (screenWidth-1) (screenHeight-1)

--Draws the bar at the top of the screen. 
drawBar::[Picture]
drawBar = [bar, border]
  where border = color (greyN 0.3) $ Line barPoints
        bar = color (greyN 0.5) $ Polygon barPoints
        barPoints = boxPoints 0 (screenHeight-1) (screenWidth-1) (screenHeight-topBorder)

-------------------------------------PAUSE Draw methods
--This function draws a box that shows that the game is paused. 
drawPause:: Picture
drawPause = pictures [pauseBox, pauseText]
  where pauseText = translate (-250) 0 $ scale 0.3 0.3 $ color green (text "Game is PAUSED")
        pauseBox = color (greyN 0.3) $ Polygon (boxPoints (-250) (-200) 250 200)


------------------------------------GAMEOVER Draw methods
{-This function is called when the state is Gameover. 
It shows a congratulations message if you have won and asks you to enter your name.
It also shows you the name that you are typing. 
If you have lost, it asks you to try again. 
If you have won and entered your name, the scorelist is shown.
It also translates the screen to the topleft-}
drawGameOver:: Won -> [String] -> Picture
drawGameOver w st = translate (- fromIntegral screenWidth/2.0) (fromIntegral screenHeight/2.0 -20) $  drawGameOver' w st 
  where drawGameOver' (HasWon Inserted name) xs    = drawTextSpaced $ ("You have won! "++name):xs ++ ["Press <ENTER> to go to the next stage"]
        drawGameOver' (HasWon NotInserted name) _  = drawTextSpaced ["You have won!", "Press <ENTER> your name " ++ name]
        drawGameOver' HasLost _                    = drawTextSpaced ["You have lost...", "Better luck next time", "Try again with <ENTER>"]

--Converts the score to a string that can be used. 
generateDrawScore:: (String, Score) -> String
generateDrawScore (name, sc) = name ++ ": " ++ show sc


-----------------------------------HELPER Draw methods

--This helper function, creates a list of boxpoints. This function can be used by a function that wants to draw a box.
boxPoints:: Int -> Int -> Int -> Int -> [(Float, Float)]
boxPoints x1 y1 x2 y2 = [(fromIntegral x1,fromIntegral y1), (fromIntegral x2,fromIntegral y1),
                         (fromIntegral x2,fromIntegral y2), (fromIntegral x1,fromIntegral y2),
                         (fromIntegral x1,fromIntegral y1)]

{-Draw a text with a fixed length. If the text is shorter than the requested length, spaces are added.
If the text is longer than the requestened length it is shortened.-}
setTextSize:: Int -> String -> String
setTextSize x s | sizeChange > 0 = s ++ replicate sizeChange ' '
                | otherwise = take x s
  where sizeChange = x - length s

{-Draws text. This changes the color of the text to green.
 The text is also scaled because otherwise it would be to large.-}
drawText:: String-> Picture
drawText s =  scale 0.15 0.15 $ color green (text s)

{-Converts a list of strings to a paragraph.-}
drawTextSpaced:: [String] -> Picture
drawTextSpaced [] = Blank
drawTextSpaced st = pictures $ drawTextSpaced' st 0
  where drawTextSpaced' [] _= []
        drawTextSpaced' (x:xs) dist = translate 0 dist (drawText x) : drawTextSpaced' xs (dist-20) -- (-20) works in this case, but needs to be changed if drawtext is changed




