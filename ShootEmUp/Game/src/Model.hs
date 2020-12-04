-- | This module contains the data types
--   which represent the state of the game
module Model where

import Objects
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss
import System.Random
import RNG
import Constants
import Vector2

--Determines the time that needs to be passed between updates, before the game is updated. 
nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1/60

--Gets background from the stage
retrieveBG:: Stage -> BackGround
retrieveBG Stage {backGround = bg} = bg

--The state that the game starts in. This is the beginstage by default. 
beginState :: (String, [String]) -> GameState
beginState (seStage, stages) = GameState (Begin seStage stages) 0 defaultGen (1.0, 1.0) (0.0,0.0) True

--Defaults that can be used, if an object is loaded incorrectly or simply for debugging
debugPlayer = Player "K" (Sp (color red (text "P"))) (Pos 0.0 2.0) (Vec 0.0 0.0) 5.0 1 Alive (Hb 200.0 300.0)  debugWeapon 1 False
debugWeapon = Weapon "pikant" debugBullet 0.3 0.0 1 (NormalPattern [])
debugBullet = BulletType "!" (Sp (color green (text ['b']))) 5.0 2.0 debugHitbox 
debugBG = BG (Sp Blank) 0 0 0 0 0
debugStage = Stage "ERROR" [] debugBG (GaEv []) 0.0
debugHitbox =  Hb 90.0 90.0


{-The background datatype. This datatype has a sprite, a spritesize and a speed.
The background has two sprites, widthleft is the width of the left picture. -}
data BackGround = BG {
                    bgSprite :: Sprite
                    , spriteWidth :: Float
                    , spriteHeight :: Float
                    , bgSpeed:: Float
                    , widthLeft  :: Float
                    , widthRight :: Float
}

{-Renders a background. It renders the sprite twice, but alligns them, so that they seem like they are one sprite.
The sprites are also wrapped. This combined with a speed creates the illusion of an infinite background -}
instance Renderable BackGround where
    render BG {bgSprite = (Sp (Bitmap bmpData)), widthLeft = wL, widthRight = wR, spriteHeight = h, spriteWidth = w} =
        pictures[leftPic, rightPic]
      where leftPic  = translate (wL/2) (h/2) $ render' (w-wL) wL
            rightPic = translate (w - wR/2) (h/2) $ render' 0 wR
            render' x width = BitmapSection (Rectangle (round x,0) (round width, round h)) bmpData
    updateRender gt bg = bg

--Alll gamestates. Begin is the menu, running is the game, paused pauses the game and gameover is between stages
data InfoToShow = Begin SelectedStage [String]
                | Running  GameInfo --Gameplay
                | Paused   GameInfo --Pause gameplay
                | GameOver (Won, GameInfo) --Score, unlock new stage

--Used by gameover. Has the player won and has the player entered his/her name?
data Won = HasWon InsertedName String | HasLost
data InsertedName = Inserted | NotInserted
--Used in the menu, what stage is selected?
type SelectedStage = String

{-The most import datatype. It contains the gamestate, the gametime,
a random number generate, the windowsize, mouseposition and if the player 
should use his/her mouse -}
data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , rng         :: StdGen
                 , screenScale :: (Float, Float)
                 , mousePos    :: (Float, Float)
                 , useMouse    :: Bool
                 }

{-GameInfo contains the game itself. It contains the current stage and a list of all stages.
It has information about the player on the screen, the score, the enemies on screen, 
the powerups on screen and the bullets on screen. -}
data GameInfo = GameInfo {
                 stages :: [String]
                 , stage :: Stage
                 , score :: Score
                 , player:: Player
                 , enemies:: [Enemy]
                 , bullets:: [Bullet]
                 , powerups:: [PowerUp]
}

{-The stage datatype. This datatype contains a scorelist,
a background, all of the events (the level script) and the time since the
stage started (used by gameevents) -}
data Stage = Stage{
              sName :: String
              , scores        :: [(String,Score)]
              , backGround    :: BackGround
              , gameEvents    :: GameEvents
              , time          :: Float
}


--Gameevents have a time and an event
newtype GameEvents = GaEv [(Float, GameEvent)]
--A powerupevent spawns a powerup, an enemyevent spawns an enemy, stageend ends the stage
data GameEvent = PowerUpEvent PowerUp | EnemyEvent Enemy | StageEnd
