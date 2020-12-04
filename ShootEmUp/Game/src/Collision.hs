module Collision where

import           GameStateHelper
import           Objects
import           Constants
import           Vector2

{-Checks if two boxes collide. For this the position of both objects is requested and their hitbox -}
checkCollide:: (Collision a,  Collision b) => a -> b -> Bool
checkCollide a b = checkHitboxOverlap (input (retrieveHbox a) (moveVec a) (getPos a)) (input (retrieveHbox b) (moveVec b) (getPos b))
   where input (Hb width height) (Vec vx vy) (Pos px py) = (Hb (width + abs vx) (height + abs vy), Pos (max px (px + vx)) (max py (vy + py)) )
         moveVec x = unitVector (getSpeed x) (getDirection x)

--Checks if two objects collide. 
checkHitboxOverlap :: (HitBox, Position) -> (HitBox,Position) -> Bool
checkHitboxOverlap h1 h2 = left h1 < right h2 && right h1 > left h2 && top h1 > bot h2 && bot h1 < top h2
    where right (Hb width _, Pos x _)  = x + width
          left  (_, Pos x _)           = x
          bot   (_, Pos _ y)           = y
          top   (Hb _ height, Pos _ y) = y + height

{-Updates the player collision. 
If the player collides with an enemy or bullet, a life is substracted.
If the player collides with a powerup, the weapon is changed or upgraded.
-}
updatePlayerColl :: [Enemy] -> [Bullet] -> [PowerUp] -> Player -> Player
updatePlayerColl enl bnl pnl = checkEnemies enl . checkBullets bnl . checkPowerUps pnl
   where checkBullets [] p       = p
         checkBullets (Bullet{bTeam = Good} : bl) p = checkBullets bl p
         checkBullets (b@Bullet{bTeam = Bad} : bl) p@Player {pLifes = lifes}  | checkCollide b p = checkBullets bl $ p{pLifes = lifes - 1}
                                                                              | otherwise        = checkBullets bl p
         checkEnemies [] p       = p
         checkEnemies (e : el) p@Player {pLifes = lifes}  | checkCollide e p = checkEnemies el $ p{pLifes = lifes - 1}
                                                          | otherwise        = checkEnemies el p
         checkPowerUps [] p = p
         checkPowerUps (pow@PowerUp {powWeapon = w}: powl) p | checkCollide pow p = changeWeapon p w
                                                             | otherwise          = checkPowerUps powl p

{-this function prevents the player from leaving the window. The player may also 
not go past the border at the top. The top is therefore the screenheight - the border. -}
keepPlayerinBounds:: Player -> Player
keepPlayerinBounds p@Player {pHitBox = (Hb _ height), pPosition = (Pos x y)} =
    case outOfBounds of
      Just newY -> p{pPosition = Pos x newY}
      Nothing   -> p
  where outOfBounds | y+height > getTop = Just (getTop-height)
                    | y < 0             = Just 0
                    | otherwise         = Nothing
        getTop =  fromIntegral $ screenHeight-topBorder


{-Checks the enemycollisions. If the enemy colllides with the player, die. 
Otherwise check all other collisions in a helper function-}
updateEnemyColl :: Player -> [Bullet] -> Enemy -> Enemy
updateEnemyColl p ls e | checkCollide p e = e{eIsAlive = Dead}
                       | otherwise = updateEnemyColl' ls e

{-Helper function of enemycollisions. If the player collides with a playerbullet, substract damage 
from the bullettype.  -}
updateEnemyColl':: [Bullet] -> Enemy -> Enemy
updateEnemyColl' (Bullet {bTeam = Bad} : bl) e = updateEnemyColl' bl e
updateEnemyColl' (b@Bullet {bTeam = Good} : bl) e@Enemy{eLifes = lifes, eIsAlive = Alive}
          | checkCollide b e                   = detractLife (round $ btDamage $ bBulletType b)
          | otherwise                          = updateEnemyColl' bl e
   where detractLife dam | lifes - dam <= 0 = e{eLifes = 0, eIsAlive = Dead}
                         | otherwise        =  updateEnemyColl' bl $ e{eLifes = lifes - dam }
updateEnemyColl' _ e                        = e

-- Removes all enemybullets that collide with a player and remove all playerbullets that collide with enemies.
updateBulletColl :: Player -> [Enemy] -> [Bullet] -> [Bullet]
updateBulletColl p el = filter filterHit
   where filterHit x = case bTeam x of
                        Bad  -> not $ checkCollide x p
                        Good -> all (not . checkCollide x) el

{-Check for all powerups if they collide with the player -}
updatePowerUpColl:: Player -> [PowerUp] -> [PowerUp]
updatePowerUpColl p = filter ffunc
  where ffunc pow = not $ checkCollide p pow