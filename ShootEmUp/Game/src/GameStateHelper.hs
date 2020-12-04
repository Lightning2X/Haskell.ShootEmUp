module GameStateHelper where

import           Model
import           Objects
import           FileManager
import           Vector2

{- Helper functions to edit a Type in the game. 
These functions may not be called in beginningstate
-}
editGameInfo:: GameState -> (GameInfo -> GameInfo) -> GameState
editGameInfo gstate@GameState {infoToShow = (Running info)} f = gstate{infoToShow = Running (f info)}
editGameInfo gstate _ = gstate
editPlayer :: GameState -> (Player -> Player) -> GameState
editPlayer gstate@GameState {infoToShow = (Begin _ _)} _ = gstate
editPlayer gstate f = editGameInfo gstate editPlayer'
    where editPlayer' info@GameInfo {player = p} = info{player = f p}
editStage :: GameState -> (Stage -> Stage) -> GameState
editStage gstate@GameState {infoToShow = (Begin _ _)} _ = gstate
editStage gstate f = editGameInfo gstate editStage'
    where editStage' info@GameInfo {stage = s} = info{stage = f s}
{- These functions are used by the controller to control the player. SetShooting toggles if the player is shooting.
  SetSpeed sets the playerdirection.
-}
setPlayerSpeed :: Vector2 -> GameState -> GameState
setPlayerSpeed vec gstate = editPlayer gstate (`setDirection` vec)
setPlayerShooting ::  Bool -> GameState -> GameState
setPlayerShooting b gstate = editPlayer gstate (`setShooting` b)
{-If a player collides with a powerup, something has to happen. 
This function handles that. If it is the same weapon, the level is plussed.
Otherwise the weapon is changed. -}
changeWeapon:: Player -> Weapon -> Player
changeWeapon p@Player {pWeaponLevel= wL, pWeapon= w} weapon | w == weapon = p{pWeaponLevel = wL+1}
                                                            | otherwise   = p{pWeapon = weapon, pWeaponLevel = 1}

{- Add functions. These functions make it possible to add enemies or powerups to the stage.
-}
addEnemy :: GameState -> Enemy -> GameState
addEnemy gstate e = editGameInfo gstate addEnemy'
   where addEnemy' info@GameInfo {enemies = el} = info{enemies = e{eDirection = Vec (-1.0) 0.0} : el}
addPowerUp:: GameState -> PowerUp -> GameState
addPowerUp gstate pow = editGameInfo gstate addPowerUp'
   where addPowerUp' info@GameInfo {powerups = powl} = info{powerups = pow : powl}

{- Get functions that return a certain type from the gamestate.
getGameInfo returns the "info" in the gamestate. 
getPlayer returns a player.
getStage returns a stage.
All of these functions may NOT be called in the beginning state -}
getGameInfo:: GameState -> GameInfo
getGameInfo GameState {infoToShow = (Running info)}      = info
getGameInfo GameState {infoToShow = (GameOver (_,info))} = info
getGameInfo GameState {infoToShow = (Paused info)}       = info
getGameInfo _                                              = error "Gamestate is not defined in Func GetGameinfo!"
getPlayer :: GameState -> Player
getPlayer GameState {infoToShow = (Begin _ _)} = error "Gamestate is not defined in Func getPlayer!"
getPlayer gstate = getPlayer' (getGameInfo gstate)
    where getPlayer' GameInfo {player = p} = p
getStage :: GameState -> Stage
getStage GameState {infoToShow = (Begin _ _)} = error "Gamestate is not defined in Func getStage!"
getStage gstate = getStage' (getGameInfo gstate)
    where getStage' GameInfo {stage = s} = s


----------------------GameStateSwitchers

--Go from the running gamestate to the gameoverscreen
switchToGO :: GameState -> Won -> GameState
switchToGO gstate@GameState {infoToShow = Running info} w = gstate{infoToShow = GameOver (w, info) }
switchToGO gstate  _                                      = gstate

--Go from the pause gamestate to the running gamestate
switchToRun :: GameState -> GameState
switchToRun gstate@GameState {infoToShow = Paused info} = gstate{infoToShow = Running info}
switchToRun gstate                                      = gstate

--Pause the game
switchToPause :: GameState -> GameState
switchToPause gstate@GameState {infoToShow = Running info}= gstate{infoToShow = Paused info}
switchToPause gstate                                      = gstate   

--IMPURE !!!!!!! the following functions are necessary to load new stages or reload stages but are therefore unfortunately impure.

--This function loads a stage from the menu.
switchToGame:: GameState -> IO GameState
switchToGame gstate@(GameState {infoToShow = Begin s st}) = do stage' <- loadStage s
                                                               p     <- loadPlayer
                                                               return gstate{infoToShow = Running (GameInfo st stage' 0 p [] [] [])}
switchToGame gstate = return gstate

{-This function reloads a level. This function resets everything in the level. 
This function is called when the player has lost-}
reloadLevel :: GameState -> IO GameState
reloadLevel gstate@GameState {infoToShow = GameOver (_, info)} =
    do reloadedStage <- loadStage (sName $ stage info)
       p <- loadPlayer
       return gstate{infoToShow = Running info{stage = reloadedStage, enemies=[], bullets=[], player = p, score=0}}
reloadLevel gstate                       = return gstate

--This function loads the next level and switches to that level. If this is the last level, switch to menu.
switchToNextLevel:: GameState -> IO GameState
switchToNextLevel gstate@GameState {infoToShow = GameOver (_, info@GameInfo {stages = st, stage =s})} =
   do let newStage = findNextStage st
      case newStage of
        Just x  -> do st' <- loadStage x
                      p  <- loadPlayer --Reset player
                      return $ gstate{infoToShow = Running info{stage = st', enemies =[], bullets =[], player = p, score =0}}
        Nothing -> return $ gstate{infoToShow = Begin (sName s) st}
  where findNextStage (x:yss@(y:_)) | sName s == x = Just y
                                    | otherwise    = findNextStage yss
        findNextStage _                             = Nothing
switchToNextLevel gstate                                                                              = return gstate

