module UpdateLogic where

import           GameStateHelper
import           Model
import           Objects
import           Collision
import           Vector2

{-This function allows players to spawnbullets.
It checks if the player can shoot. If that is true, a weaponpattern is applied.
Then the player shoots and the bullet is added to the bulletlist-}
spawnBulletPl :: GameState -> GameState
spawnBulletPl gstate = case spawnBullets (getPlayer gstate) of
                          [] -> gstate
                          ls  -> editGameInfo gstate (addBullets ls)
    where spawnBullets p@Player {pWeapon = Weapon {currentInterval = inter} }
                                         | inter <= 0.0 = map (shoot p) shootDirs
                                         | otherwise    = []
                                         where shootDirs = dirPatternGenerator (getWeaponPattern p) (getFaceDeg p)
          addBullets pBul info@GameInfo{bullets = bl, player = p} = info{bullets = bl ++ pBul, player = setWeaponDelay p}



{-This function allows the definiton of a weaponpattern. This means that when an enemy for example shoots
, 8 bullets are shot in 8 different directions. This function generates these directions. 
-}
dirPatternGenerator :: WeaponPattern -> Float -> [Vector2]
dirPatternGenerator (CircularPattern minA maxA amount) faceDeg = generateVecs (fromIntegral amount)
   where generateVecs :: Float -> [Vector2]
         generateVecs 0 = []
         generateVecs am = degreeToVector (faceDeg + maxA - (am * difference)) : generateVecs (am - 1)
         difference = (maxA - minA) / fromIntegral amount
dirPatternGenerator (NormalPattern ls) faceDeg = map (\x -> degreeToVector (faceDeg + x)) ls



-- Deletes dead enemies from the gamestate
deleteDeadEnemies :: GameState -> GameState
deleteDeadEnemies gstate = editGameInfo gstate deleteDeadEnemies'
   where deleteDeadEnemies' info@GameInfo {enemies = el} = info{enemies = filter ffunc el}
         ffunc Enemy {eIsAlive = Dead} = False
         ffunc _                       = True

--This function removes dead bullets from the gamestate
deleteDeadBullets :: GameState -> GameState
deleteDeadBullets gstate = editGameInfo gstate deleteDeadBullets'
   where deleteDeadBullets' info@GameInfo {bullets = bl} = info{bullets = filter ffunc bl}
         ffunc Bullet{btTimeToLive = ttl} | ttl <= 0 = False
                                          | otherwise = True

{- The main update function.
This function first updates the player requests and the collision. 
Then all dead enemies are removed and score is added. After that all events are managed. -}
updateGame :: GameState -> GameState
updateGame gstate@GameState {infoToShow = (Running _)}  = (updateEvents . handleDeadEntities . updateCollision . updatePlayerRequests) gstate
updateGame gstate = gstate

{-This function allows the background to move. This function simulates movement.
That is because a background exists of two identical images that are wrapped.
The left images dissapears slowy, while the right one goes from the right to the left.
When the right image touches the border, the right and left images are reset.-}
updateBG :: GameState -> GameState
updateBG gstate@GameState {infoToShow = Running info@GameInfo {stage = st@Stage {backGround = bg}}}
 = gstate{infoToShow = Running info{stage = st{backGround = updatedBG bg}}}
   where updatedBG bg'@BG {bgSpeed = s, widthLeft = wL, widthRight = wR, spriteWidth = w}
            | wL + s <= 0 = bg'{widthLeft=w, widthRight = 0}
            | otherwise   = bg'{widthLeft= wL+s, widthRight = wR - s}
updateBG g = g


{-This function updates the score, checks if the player is gameover removes enemies.  -}
handleDeadEntities :: GameState -> GameState
handleDeadEntities gstate@GameState { elapsedTime = t, mousePos =mP, useMouse = uM} =
  checkPlDead $ deleteDeadBullets $ deleteDeadEnemies $ editGameInfo (editGameInfo gstate updateGameInfo') updateScore
   where checkPlDead gamest = checkPlDead' (getGameInfo gamest)
          where checkPlDead' GameInfo {player = Player{pLifes = lifes}} | lifes <= 0 = switchToGO gamest HasLost
                                                                        | otherwise = gamest
         updateGameInfo' info'@GameInfo {stage = s, player = p, enemies = el, bullets = bl, powerups = powl} =
          info'{stage = updateStage t s, player = newP, enemies = newEnemies, bullets = map (updateBullet t) bl ++ enemyB, powerups = map (updatePowerup t) powl}
            where newP = updatePlayer t $ updatePlayerMouseDir uM mP p
                  (newEnemies, enemyB) = updateEnemies t p el

{-A stage consists of multiple events. 
Each event has a time. If the time of an event has started, the event is called.
This event is then removed from the events list. An enemyevent, spawns a new enemy.
A powerupevent spawns a powerup. A StageEnd event sets the game to gameover.  
-}
updateEvents :: GameState -> GameState
updateEvents gstate =  updateEvents' (getGameInfo gstate)
   where  removeNextEvent :: GameInfo -> GameInfo
          removeNextEvent info@GameInfo { stage = st@Stage {gameEvents = GaEv (_ : el)}} = info{ stage = st{ gameEvents = GaEv el } }
          removeNextEvent g                                                              = g
          updateEvents' :: GameInfo -> GameState
          updateEvents' GameInfo { stage = Stage {gameEvents = GaEv []}}                                     = gstate
          updateEvents' GameInfo { stage = Stage {time = t, gameEvents = GaEv ((eTime, e) : _)}} | eTime < t = handleEvent e
                                                                                                 | otherwise = gstate
              where handleEvent :: GameEvent -> GameState
                    handleEvent ev = case ev of
                                        EnemyEvent enemy     -> nextEventHandler $ addEnemy gstate enemy
                                        PowerUpEvent powerup -> nextEventHandler $ addPowerUp gstate powerup
                                        StageEnd             -> switchToGO gstate (HasWon NotInserted "")
                    nextEventHandler ngst = updateEvents $ editGameInfo ngst removeNextEvent

--If a player wants to shoot, this function handles that request. 
updatePlayerRequests:: GameState -> GameState
updatePlayerRequests gstate@GameState {infoToShow = Running info} = playerRequests info
  where playerRequests GameInfo {player = Player {pIsShooting = pB}} | pB        = spawnBulletPl gstate
                                                                     | otherwise = gstate
updatePlayerRequests gstate = gstate

--Updates the collision of all objects. Only updates the collision if the game is running. 
updateCollision :: GameState -> GameState
updateCollision gstate@GameState {infoToShow = Running _ } = editGameInfo gstate updateCollision'
   where updateCollision' info@GameInfo {player = p, enemies = el, bullets = bl, powerups = powl} = info{player =
          updatePlayerColl el bl powl p, enemies = map (updateEnemyColl p bl) el, bullets = updateBulletColl p el bl
          , powerups = updatePowerUpColl p powl}
updateCollision gstate = gstate

-- Updates individual bullets with Gametime. Updates the bulletanimation and moves it.
-- If a bullet is old, it is removed.
updateBullet :: Float -> Bullet -> Bullet
updateBullet gt b = updateRender gt . move $ updateTTL b
   where updateTTL b'@Bullet{btTimeToLive = ttl} = b'{btTimeToLive = ttl - gt}


{-Updates all enemies. It allows all enemies to update their animations, allows them to shoot with different
weaponpatterns. Allows them to move along their path.-}
updateEnemies  :: Float -> Player -> [Enemy] -> ([Enemy], [Bullet])
updateEnemies gt p =  foldr update ([], [])
   where update e (el, bl) = let (en, eBL) = nextEnWep e
                              in ((updateRender gt . move . updateWeaponInterval . updateEnemyPath p) en : el, eBL ++ bl)
         updateWeaponInterval e@Enemy {eWeapon = w} = e{eWeapon = w{currentInterval = currentInterval w - gt}}
         nextEnWep e@Enemy {eWeapon = w@Weapon {currentInterval = inter}} | inter <= 0.0 = (e{eWeapon = w{currentInterval = interval w}}, map (shoot e) shootDirs)
                                                                          | otherwise    = (e, [])
           where shootDirs = dirPatternGenerator (getWeaponPattern e) (getFaceDeg e)


{-This function updates the player logic.
It updates the player weapon, moves the player, checks if the player is in bound and updates the animation. 
-}
updatePlayer :: Float -> Player -> Player
updatePlayer gt = updateRender gt . keepPlayerinBounds . move . updateWeaponInterval
 where updateWeaponInterval p@Player {pWeapon = w} = p{pWeapon = w{currentInterval = currentInterval w - gt}}


{-This function updates the player direction with the mouse.
This function only works if the mouse is enabled.
If this is the case, the playerYDirection is set towards the mouseposition untill 
the player is almost near the mouseposition. -}
updatePlayerMouseDir :: Bool -> (Float, Float) -> Player -> Player
updatePlayerMouseDir pUsesMouse (_, mouseyPos) p | pUsesMouse  = p
                                                 | otherwise = setDirection p newDir
  where playerY (Pos _ y) = y
        yDifference = mouseyPos  - playerY (getPos p)
        newDir | abs yDifference < 10 = Vec 0.0 0.0  --Is near the mouse (10 is an approximation)
               | otherwise            = Vec 0.0 yDifference


{-Updates the powerup with the gametime. This only updates the powerup animation-}
updatePowerup:: Float -> PowerUp -> PowerUp
updatePowerup gt = updateRender gt . move


{-This function updates the stage, which is only the gametime. -}
updateStage  :: Float -> Stage -> Stage
updateStage gt = updateTime
   where updateTime s@Stage {time = t} = s{time = t + gt}

{-This function is called before all dead enemies are removed.
If an enemy is dead, it's score value is added to the global score. -}
updateScore :: GameInfo -> GameInfo
updateScore gInfo@GameInfo {enemies = e, score = sc} = gInfo{enemies = e, score = sc + upScore e}
  where upScore:: [Enemy] -> Int
        upScore []                                                           = 0
        upScore (Enemy {eIsAlive = alive, eScore = eSc}:xs) | alive == Dead  = eSc + upScore xs
                                                            | otherwise      = upScore xs

{-This function allows an enemy to follow it's predetermined path. 
Once an enemy is near its path, the step is deleted and the next step is chosen.
If the enemy is not near yet, it's direction is set to that position-}
updateEnemyPath :: Player -> Enemy -> Enemy
updateEnemyPath _ e@Enemy {ePath = Pa []} = e
updateEnemyPath p@Player {pPosition = pPos} e@Enemy {ePath = Pa (pa : pas), ePosition = ePos@(Pos eX eY), 
                                                                     eSpeed = eSpd} = performPathAction
   where performPathAction = case pa of
                                Move x       -> followEnemyPath x  --Move to an x and y position relative of the enemy
                                ChargePlayer -> followEnemyPath pPos --Move towards player
                                MoveX x      -> followEnemyPath (Pos x eY) --Only move in the x dir
                                MoveY y      -> followEnemyPath (Pos eX y) --Only move in the y dir
                                _            -> e 
         followEnemyPath pos  | vecDist (toVec pos) ( toVec $ getPos $ move e{eDirection = newDir}) < distTresh = updateEnemyPath p e{ePath = Pa pas}
                              | otherwise =  e{eDirection = newDir }
            where newDir = mult eSpd (normalise $ toVec pos - toVec ePos)
                  distTresh = eSpd / 2
