module Objects where

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss
import Constants
import Vector2

type Score = Int

--Determines the time a bullet is allowed to life before it is removed (for performance)
bulletTTLdef :: Float
bulletTTLdef = 12.0


--The sprite datatype. A sprite is either a picture or an animation.
--An animation has an array of pictures, the currentframe, the currenttime and the time in between frames. 
data Sprite = Sp Picture | An [Picture] CurrentFrame Float Float
type CurrentFrame = Int

--Rebders a sprite. The animation draws the currentanimation.
instance Renderable Sprite where
    render (Sp s) =  s
    render (An pics currentFrame _ _) = pics !! currentFrame
    updateRender = updateAnm

{- Updates an animation. Is the time longer than the time in between frames,
then switch to the next frame. If it is the last frame, go back to the first frame. -}
updateAnm:: Float -> Sprite -> Sprite
updateAnm time (An pics currentFrame t maxTime) | time+t>maxTime = nextSprite
                                                | otherwise      = An pics currentFrame (t+time) maxTime
  where nextSprite | currentFrame + 1 >= length pics = An pics 0 0 maxTime
                   | otherwise                       = An pics (currentFrame + 1) 0 maxTime
updateAnm _ sp = sp

type Lifes = Int

data IsAlive = Dead | Alive
  deriving (Eq)

--Hitbox datatype. A hitbox has a width and a height. 
data HitBox = Hb Float Float
  deriving (Show, Eq,Ord, Read)

--A hitbox is rendered in debugmode. The hitbox is simply a box.
instance  Renderable HitBox where
    render (Hb  width height) = Line [(0,0), (width,0), (width,height), (0,height), (0,0) ]
    updateRender _ sp = sp

--This class is used by objects that can be rendered. UpdateRender allows animations to update 
--and render simply renders the object.
class Renderable r where
    render:: r -> Picture
    updateRender:: Float -> r -> r

{-The movement class allows objects to move. An object must have a position and must be able to 
set it. The object should also have a speed and a direction. The objects in this class should 
be able to move on it's own (use the speed and direction). -}
class Movement m where
    getPos:: m -> Position
    setPos::m->Position->m
    getDirection:: m -> Vector2
    setDirection:: m -> Vector2 -> m
    getSpeed:: m -> Float
    move:: m ->  m


--A class that collides, should be able to move (needed for getPos) and should have a hitbox.
class Movement c => Collision c where
    retrieveHbox :: c -> HitBox


{-The powerup datatype. This object can spawn on the map and simply moves. 
It also contains a weapon that can be picked up by a player -}
data PowerUp = PowerUp {
                powName :: String
                , powSprite :: Sprite
                , powPosition :: Position
                , powDirection :: Vector2
                , powSpeed :: Float
                , powWeapon:: Weapon
                , powHitBox:: HitBox
}

--Render the sprite of the powerup (and a hitbox if debugmode is active)
--The sprite is also translated so that it is rendered correctly in the coordinatesystem/
instance Renderable PowerUp where
    render pow@PowerUp {powSprite = s, powPosition = (Pos x y), powHitBox = hb@(Hb  width height)} | renderDebug = pictures [spriteRender, debugRender]
                                                                                                   | otherwise   = spriteRender
      where debugRender = translate x y $ color white (render hb)
            spriteRender = translate (x+width/2) (y+height/2) $ render s
    updateRender gt pow@PowerUp {powSprite = powSp} = pow{powSprite = updateRender gt powSp}

--A powerup needs to move across the screen and has a position.
instance Movement PowerUp where
    move pow = pow{powPosition = translatePos (powPosition pow)  (unitVector (powSpeed pow) (powDirection pow)) }
    setDirection pow v = pow {powDirection = v}
    setPos pow pos = pow{powPosition = pos}
    getPos   = powPosition
    getSpeed = powSpeed
    getDirection = powDirection

--retrieve hitbox
instance Collision PowerUp where
  retrieveHbox PowerUp {powHitBox = powH} = powH

{-A weapon has a bullettype (damage, sprite etc.), an interval (how fast can it shoot),
a level (upgradeds) and a pattern (in one direction or in for example 8) -}
data Weapon = Weapon {
                wName :: String
                , wBulletType:: BulletType
                , interval:: Float
                , currentInterval:: Float
                , weaponLevel:: WeaponLevel
                , weaponPattern :: WeaponPattern
}
--If the weapon has the same name, it is equal
instance Eq Weapon where
  Weapon {wName = name1} == Weapon {wName = name2} = name1 == name2
type WeaponLevel = Int

--A weaponpattern determines how many bullets are spawned and in which direction.
data WeaponPattern = CircularPattern Float Float Int -- Max circular angle, min circular angle and the amount of shots
                   | NormalPattern [Float]     -- Angles to shoot in per shot.

--Used by the gui in render
instance Show Weapon where
    show Weapon{wName = name, weaponLevel = level} = "Weapon = " ++ name ++ " " ++ show level

{-This class is used by objects that shoot. These objects should have a weaponpattern,
should have a weapon of course, should be able to shoot and should have an angle. -}
class Movement a => Armed a where
   getWeaponPattern :: a -> WeaponPattern
   getWeapon :: a -> Weapon
   getFaceDeg :: a -> Float
   shoot :: a -> Vector2 -> Bullet

--The path that an enemy should follow
newtype EnemyPath = Pa [Action]
 deriving Show

--The actions that an enemy does along it's path.
data Action = Move Position | MoveR (Position, Position) | MoveX Float | MoveY Float | ChargePlayer
  deriving Show

{-An enemy has just one constructor. That is because enemies are similar in this game.
They all have a sprite, should be able to move, have a hitbox and lifes, a weapon and have a unique path 
and behaviour. -}
data Enemy = Enemy {
              eName :: String
             , eSprite :: Sprite
             , eScore:: Score
             , ePosition:: Position
             , eDirection:: Vector2
             , eSpeed:: Float
             , eLifes:: Lifes
             , eIsAlive:: IsAlive
             , eHitBox:: HitBox
             , eWeapon:: Weapon
             , ePath:: EnemyPath
}

--Render the enemy (and it's hitbox in debug). The enemy is also translated so that it works in the coordinatesystem..
instance Renderable Enemy where
    render e@Enemy {eSprite = s, ePosition = (Pos x y), eHitBox = hb@(Hb  width height)} | renderDebug = pictures (spriteRender: debugRender)
                                                                                         | otherwise   = spriteRender
      where debugRender = map (translate x y) [color red (render hb), color red (text (show $ eLifes e))]
            spriteRender = translate (x+width/2) (y+height/2) $ render s
    updateRender gt e@Enemy {eSprite = eSp} = e{eSprite = updateRender gt eSp}

--A player should be able to move like any other object. It uses it speed and direction to move.
instance Movement Enemy where
    move e = e{ePosition = translatePos (ePosition e)  (unitVector (eSpeed e) (eDirection e)) }
    setDirection e v = e {eDirection = v}
    setPos e pos = e{ePosition = pos}
    getPos   = ePosition
    getSpeed = eSpeed
    getDirection = eDirection

--An enemy can collide
instance Collision Enemy where
  retrieveHbox Enemy {eHitBox = eH} = eH

--An enemy always has a weapon and uses this weapon.
instance Armed Enemy where
   getWeaponPattern Enemy{eWeapon = Weapon{weaponPattern = p}} = p
   getWeapon Enemy{eWeapon = w} = w
   getFaceDeg e = 180
   shoot e vec = shoot' (getWeapon e)
      where shoot' Weapon {wBulletType = b}  = Bullet b vec bulletTTLdef (getPos e) Bad


{-The player datatype has a sprite and is able to move.
A player also has a set amount of lifes and a weapon. 
The variable pIsShooting is activated when the player presses 
the shoot binding and is set to false when this button is released.
This bool means that the player wants to shoot. The weaponlevel is used
to upgrade the weapon (damage is multiplied) -}
data Player = Player {
                pName::String
                , pSprite :: Sprite
                , pPosition :: Position
                , pDirection:: Vector2
                , pSpeed :: Float
                , pLifes :: Lifes
                , pIsAlive:: IsAlive
                , pHitBox :: HitBox
                , pWeapon :: Weapon
                , pWeaponLevel :: WeaponLevel
                , pIsShooting:: Bool
}
--Reset weapon so that it can not be used like a machine gun (the gun has an interval)
setWeaponDelay:: Player -> Player
setWeaponDelay p@Player {pWeapon = w} = p{pWeapon = w{currentInterval = interval w}}

--Sets if the player wants to shoot or not
setShooting::Player -> Bool -> Player
setShooting p b= p{pIsShooting = b}


--Render the player (and its hitbox and postion in debug) and translate the object.
instance Renderable Player where
    render Player {pSprite = s, pPosition = (Pos x y), pHitBox = hb@(Hb  width height)} | renderDebug = pictures [spriteRender,debugRender]
                                                                                        | otherwise   = spriteRender
      where debugRender = translate x y $ pictures [color green (render hb), scale 0.15 0.15 $color green (text (show y))]
            spriteRender = translate (x+width/2) (y+height/2) $ render s
    updateRender gt p@Player {pSprite = pSp} = p{pSprite = updateRender gt pSp}

--A player should be able to move. The player uses it's velocity and direction to move.
instance Movement Player where
    move p = p{pPosition = translatePos (pPosition p)  (unitVector (pSpeed p) (pDirection p))}
    setPos p pos = p{pPosition = pos}
    setDirection p v = p {pDirection = v}
    getPos    = pPosition
    getSpeed = pSpeed
    getDirection  = pDirection

--A player can collide with bullets and enemies. 
instance Collision Player where
  retrieveHbox Player{pHitBox = pH} = pH


--This allows the player to shoot.It makes sure that the bullet spawns before the player and in the middle (in ycoordinates).
--It also sets the damage according to the weaponlevel of the player. 
instance Armed Player where
  getWeaponPattern Player{pWeapon = Weapon{weaponPattern = p}} = p
  getWeapon Player{pWeapon = w} = w
  getFaceDeg p = 0
  shoot p@Player{pWeaponLevel = lvl} vec = shoot' (getWeapon p)
      where shoot' Weapon {wBulletType = b@BulletType {btDamage = dam}}= Bullet  b{btDamage = dam * fromIntegral lvl} vec bulletTTLdef (spawnPos (retrieveHbox p) (bHitBox b) (getPos p)) Good
            spawnPos (Hb pWidth pHeight) (Hb bWidth bHeight) (Pos x y) = Pos (x+pWidth) (y + pHeight/2 - bHeight/2)

--Bullets
{-  the bullettype determines what is shot by a weapon. This contains a fixed 
bulletspeed, a sprite, the damage the bullet gives and a hitbox -}
data BulletType = BulletType {
                   btName :: String
                   , btSprite :: Sprite
                   , btSpeed:: Float
                   , btDamage:: Float
                   , bHitBox :: HitBox
}

{-The bullet is an object that contains a bullettype. A bullettype only defines what the bullet is going to be and
the bullet spawns this bullet. The direction is the direction that the bullet is going, btTimeToLive determines how 
long a bullet may life before it dies. bTeam determines if the bullet is shot by a player or an enemy -}
data Bullet = Bullet {
                bBulletType :: BulletType
                , bDirection:: Vector2
                , btTimeToLive:: Float
                , bPosition:: Position
                , bTeam :: BulletTeam
}
--Playerbullet or enemybullet
data BulletTeam = Good | Bad

--Render the bullet and translate it (also render hitbox in debug)
instance Renderable Bullet where
    render Bullet {bPosition = (Pos x y),bBulletType = BulletType {bHitBox = hb@(Hb  width height), btSprite = s}}
             | renderDebug = pictures [spriteRender,debugRender]
             | otherwise = spriteRender
      where spriteRender = translate (x+width/2) (y+height/2) $ render s
            debugRender = translate x y $ pictures [color blue (render hb)]
    updateRender gt b@Bullet {bBulletType = bType@BulletType {btSprite = btSp}} = b{bBulletType = bType{btSprite = updateRender gt btSp}}

--A bullet can move across the screen with it's speed and direction.
instance Movement Bullet where
    move b = b{bPosition = translatePos (bPosition b)  (unitVector (btSpeed $ bBulletType b) (bDirection b))}
    setPos b pos = b{bPosition = pos}
    setDirection b v = b {bDirection = v}
    getPos  = bPosition
    getSpeed b = btSpeed $ bBulletType b
    getDirection = bDirection

--A bullet can collide
instance Collision Bullet where
  retrieveHbox b = bHitBox $ bBulletType b


