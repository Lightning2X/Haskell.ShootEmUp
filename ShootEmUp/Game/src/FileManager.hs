module FileManager where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Bitmap
import           Model
import           Objects
import           Constants
import           System.Directory
import           System.Random
import           RNG
import           Vector2


{-This function generates a list of all stages. It returns the name of the first stage, all stages -}
loadStages:: IO (String, [String])
loadStages          = do dir <-getCurrentDirectory
                         content <- readFile $ dir++ "/content/Stages/" ++ rootStage ++ ".txt"
                         case lines content of
                           xss@(x:xs) -> return (x, xss)
                           _          -> return ("ERROR", [])

{-This function loads a stage. 
First it checks if the file exists, then it loads the content. 
Then it checks the content. If it is incorrect, a debug stage is loaded.
If the stage is correct, the background, score, enemytypes, weapontypes an events 
are loaded. -}
loadStage:: String -> IO Stage
loadStage fileName = do dir <- getCurrentDirectory
                        content <- readFile $ dir++ "/content/Stages/" ++ fileName ++ ".txt"
                        case lines content of
                          (bgName: enemies: powerups: events) -> do bg <- loadBackGround bgName
                                                                    sc <- loadScore fileName
                                                                    getEnemyTypes <- loadEnemies enemies
                                                                    getPowerupTypes <- loadPowerUps powerups
                                                                    events' <- loadEvents events getEnemyTypes getPowerupTypes
                                                                    return (Stage fileName sc bg events' 0.0)
                          _                                   -> do debPic <- loadSpriteBMP "bullet"
                                                                    let debBg = BG (Sp Blank) 0 0 0 0 0
                                                                    return (Stage "EMPTY" [] debBg ( GaEv []) 0.0)

--This function loads the default player. 
loadPlayer:: IO Player
loadPlayer = do playerSprite <- loadSprite "An 0.1 7 fighter_plane" --Default sprite
                let hb = generateHitBox playerSprite --generate hitbox
                let pSpeed = 4.0 --playerspeed
                let lifes  = 3 --default lifes
                weapon <- loadWeapon "JetGun" --defaultweapon 
                return $ Player "P1" playerSprite (Pos 0.0 0.0) (Vec 0.0 0.0) pSpeed lifes Alive hb weapon 1 False

--This function loads a background. The background size is also retrieved and converted to a float
loadBackGround:: String -> IO BackGround
loadBackGround s = case words s of
                     (path:speed:xs) -> do bgSp <- loadSpriteBMP path --Create BackGroundSprite
                                           return $ uncurry  (\x y -> (BG bgSp x y (read speed) x 0)) (getSize bgSp)
                     _               -> return (BG (Sp Blank) 0 0 0 0 0) --DebugBackground
  where getSize (Sp (Bitmap bmpData)) = (\(x,y) -> (fromIntegral x, fromIntegral y)) $ bitmapSize bmpData
        getSize _                     = (0, 0)

{- This function loads a sprite.
It first checks if it is a single sprite or an animation.
If it is an animation, a list of sprites is generated. Spritegen manages this.
For example An 0.1 7 figher_plane, creates 7 images (fighter_plane0 etc.).
If it is a single sprite, a single sprite is loaded.
Otherwise a default picture is generated. 
-}
loadSprite:: String -> IO Sprite
loadSprite s = loadSprite' $ words s
  where loadSprite' (spType:interval:frames:sprite:rest)         | spType == "An" = loadSpriteAn (read interval) $ take (read frames -1) (spriteGen 1)
                                                                 | otherwise      = return $ Sp Blank
          where spriteGen n = (sprite++show n):spriteGen (n+1)
        loadSprite' [spType, sprite]                             | spType == "Sp" = loadSpriteBMP sprite
                                                                 | otherwise      = return $ Sp Blank
        loadSprite' _                                                             = return $ Sp Blank

--Loads a list of sprites and converts it to an animation.
loadSpriteAn::Float -> [String] -> IO Sprite
loadSpriteAn t s = getCurrentDirectory >>= \_ -> do sprites <- mapM loadPicture s
                                                    return $ An sprites 0 0 t
--LoadsASingleSprite
loadSpriteBMP:: String -> IO Sprite
loadSpriteBMP s = loadPicture s >>= \pic -> return $ Sp pic

{-Loads a bmp. It checks if the file exists. 
If the file exists, this picture is returned.
Otherwise a default picture is returned. -}
loadPicture:: String -> IO Picture
loadPicture s = getCurrentDirectory >>= \dir -> do exists <- doesFileExist $ dir ++ "/content/Sprites/" ++ s ++ ".bmp"
                                                   case exists of
                                                     True -> loadBMP $ dir ++ "/content/Sprites/" ++ s ++ ".bmp"
                                                     False -> do putStr $ dir ++ "/content/Sprites/" ++ s ++ ".bmp" ++ " does not exist"
                                                                 return Blank

--This function generates a hitbox from a sprite.
--If the sprite is an animation, the first sprite is used. 
generateHitBox:: Sprite -> HitBox
generateHitBox (Sp s)            = generateHitBox' s
generateHitBox (An (x:_) _ _ _)  = generateHitBox' x
generateHitBox _                 = debugHitbox

--Generate hitbox from sprite. 
generateHitBox':: Picture -> HitBox
generateHitBox' (Bitmap bmpData) =  (\(width, height) -> Hb (fromIntegral width) (fromIntegral height)) $ bitmapSize bmpData
generateHitBox' _                = debugHitbox


--Returns a scorelist. Each entry is a name + a score. 
loadScore:: String -> IO [(String, Score)]
loadScore filename = do dir <- getCurrentDirectory
                        content <- readFile $ dir++"/content/Stages/Score/" ++ filename ++ "Score.txt"
                        length content `seq`  return $ map lineToScore $ lines content
  where lineToScore s = case words s of
                          (name:sc:_) -> (name, read sc)
                          _           -> error "Something is wrong with the score layout."

--Write a scorelist to a scorefile.
writeScore:: String -> [(String, Score)] -> IO ()
writeScore filename xs = do dir <- getCurrentDirectory
                            writeFile (dir ++ "/content/Stages/Score/" ++ filename ++ "Score.txt") $ unlines $ map scoreToLine xs
  where scoreToLine (name, sc) = name ++ " " ++ show sc


--Loads all enemytypes.
loadEnemies:: String ->   IO [(String, Enemy)]
loadEnemies = mapM loadEnemy . words


{-Loads an individual enemy. It first checks if the enemy exists. 
If the enemy exists, its sprite, weapon and path are also loaded and checked.
Then the enemy is returned.-}
loadEnemy:: String -> IO (String, Enemy)
loadEnemy fileName = do dir <- getCurrentDirectory
                        content <- readFile (dir++"/content/Enemies/" ++ fileName ++ ".txt")
                        case lines content of
                          (name:spritePath:enemyScore:speed:lifes:weapon:path) -> do enPic <-loadSprite spritePath
                                                                                     let enemScore = read enemyScore
                                                                                     let position = Pos 0.0 0.0
                                                                                     let enemySpeed = read speed::Float
                                                                                     let enemyLifes = read lifes::Int
                                                                                     let hitBox = generateHitBox enPic
                                                                                     enemyWeapon <- loadWeapon weapon
                                                                                     let enemyPath = loadEnemyPath path
                                                                                     return (name, Enemy name enPic enemScore position (Vec 0.0 0.0) enemySpeed enemyLifes Alive hitBox enemyWeapon enemyPath)
                          _                                                    -> do debugWeapon' <- loadWeapon "LOL"
                                                                                     return ("?", Enemy "!WRONG!" (Sp Blank) 0 (Pos 0.0 0.0) (Vec 0.0 0.0) 0.0 0 Dead debugHitbox debugWeapon' (Pa []))

--This function loads an enemypath.
loadEnemyPath:: [String] -> EnemyPath
loadEnemyPath ls = Pa (concatMap (loadEnemyPath' . words) ls)
   where loadEnemyPath' ls' = case loadAction ls' of
            Just x  -> [x]
            Nothing -> []
         loadAction (x : xs) = case x of
            "Move" -> case xs of
                (x' : y' : _) -> Just $ Move (Pos (read x'::Float) (read y'::Float))
                _             -> Nothing
            "MoveR"-> case xs of
                (x1 : y1 : x2: y2 : _) -> Just $ MoveR (Pos (read x1::Float) (read y1::Float), Pos (read x2::Float) (read y2::Float))
                _                      -> Nothing
            "MoveX" -> case xs of
                (x':_) -> Just $ MoveX (read x')
                _      -> Nothing
            "MoveY" -> case xs of
                (y':_) -> Just $ MoveY (read y')
                _      -> Nothing
            "ChargePlayer" -> Just ChargePlayer
            _      -> Nothing
         loadAction _ = Nothing

{-This function loads a weapon. 
This function loads the bullettype as well and determines the powerup sprite. 
 -}
loadWeapon:: String -> IO Weapon
loadWeapon fileName = do dir <- getCurrentDirectory
                         content <- readFile (dir++"/content/Weapons/" ++ fileName ++ ".txt")
                         case lines content of
                          (interval':level:bulletname:spritePath:bSpeed:bDamage:bPattern:_) -> do bulletPic <- loadSprite spritePath
                                                                                                  let hb = generateHitBox bulletPic
                                                                                                  let bulletType = BulletType bulletname bulletPic (read bSpeed) (read bDamage) hb 
                                                                                                  let wP = loadwPattern bPattern
                                                                                                  return (Weapon fileName bulletType (read interval') 0.0 (read level) wP)
                          _                                                                 -> do let debugBullet' = BulletType "!" (Sp Blank) 0.0 0.0 debugHitbox 
                                                                                                  return (Weapon fileName debugBullet' 0.0 0.0 1 (NormalPattern []))
--This function loads a weaponpattern and checks if this pattern is correct.
loadwPattern:: String ->  WeaponPattern
loadwPattern =  loadwPattern' . words
    where loadwPattern' (x : xs) = case x of
             "NormalPattern" -> NormalPattern $ map read xs
             "CircularPattern" -> case xs of
                (minA : maxA : amount : []) -> CircularPattern (read minA::Float) (read maxA::Float) (read amount::Int)
                _                           -> error "Circular pattern wrongly typed!"
          loadwPattern _         = error "Something went wrong with loading the pattern"

--This function generates a map of powerups. The keys are the names and the values are powerups. 
loadPowerUps:: String -> IO [(String, PowerUp)]
loadPowerUps = mapM loadPowerUp . words

--This function loads a powerup. It also needs to load a weapon and a sprite for this. 
loadPowerUp:: String -> IO(String, PowerUp)
loadPowerUp fileName = do dir <-getCurrentDirectory
                          content <- readFile (dir++"/content/Weapons/" ++ fileName ++ ".txt")
                          weapon <- loadWeapon fileName
                          pic <- loadSprite ("Sp " ++ fileName)
                          let hb = generateHitBox pic
                          return  (fileName, PowerUp fileName pic (Pos 0.0 0.0) (Vec (-1.0) 0.0) 1 weapon hb)

{-This function loads all events. If an event is invalid it is discarded, otherwise it is added. 
An event is loaded with the function loadEvent. Enemypaths are also set (no random components) with 
setRandomEnemyPaths. 
-}
loadEvents:: [String] -> [(String, Enemy)] -> [(String, PowerUp)] -> IO GameEvents
loadEvents xs enemies powerups = GaEv <$> foldr (checkLoad . words) (return []) xs
  where checkLoad (time:ev) previous = do gen <- makeNewRNG
                                          case loadEvent ev enemies powerups gen of
                                            Just e  -> do p <- setRandomEnemyPaths e
                                                          (:) <$> return (read time, p) <*> previous
                                            Nothing -> previous
        checkLoad _ previous         = previous


--This function chooses between two paths that an enemy can take. If the value is 1, the second path is taken.
--Otherwise the first path is taken. This function only takes care of the enemyPathStep moveR. 
--Before every event, there is also a time added (when will this event take place)
setRandomEnemyPaths::GameEvent-> IO GameEvent
setRandomEnemyPaths (EnemyEvent e@(Enemy {ePath = Pa ps})) = do newPath <-setRandomEnemyPaths' ps
                                                                return $ EnemyEvent $ e{ePath = Pa newPath}
  where setRandomEnemyPaths' []                                    = return []
        setRandomEnemyPaths' ( (MoveR (Pos x1 y1, Pos x2 y2)):xs) = do gen <- makeNewRNG
                                                                       case randomInt gen 0 1 of
                                                                         0 -> (:) <$> return (Move $ Pos x1 y1) <*> setRandomEnemyPaths' xs
                                                                         1 -> (:) <$> return (Move $ Pos x2 y2) <*> setRandomEnemyPaths' xs

        setRandomEnemyPaths' (x:xs)                                = (:) <$> return x <*> setRandomEnemyPaths' xs
setRandomEnemyPaths x = return x


{-This long function loads an event. It first checks the type of the event.
If it is an enemy event, an enemy event is added. For the enemyEvent to work, 
the enemy is first checked. If it does not exist, the event is not added. 
If it exists, it's position is set relative to the screen (1.1 is 1.1 * screensize). 
After this the enemypath is also set to relative (1.1 again). 
Powerupevents are also only added if they exist. There position is also set relative.
However, a powerupevent also has a direction.
An EndStage event is added when End is reached. 
-}
loadEvent:: [String] -> [(String, Enemy)] -> [(String, PowerUp)] -> StdGen -> Maybe GameEvent
loadEvent (x:xs) enemies powerups gen | x=="En:"  = createEnEv xs
                                      | x=="Pow:" = createPowEv xs
                                      | x=="End"  = Just StageEnd
                                      | otherwise = Nothing
  where createEnEv (name:posx:posy:xs) = (\e -> EnemyEvent (relativatePath $ setPos e $ Pos (read posx *  fromIntegral screenWidth)
                                                                                             (read posy * fromIntegral screenHeight)))
                                         <$> checkIfPresent name enemies

        createEnEv (name:xs)           =  (\e -> EnemyEvent (relativatePath $ setPos e $ Pos (fromIntegral screenWidth * 0.9)
                                                                                              (randomFloat gen 0 $ fromIntegral screenHeight)))
                                          <$> checkIfPresent name enemies
        createEnEv _                   = Nothing
        createPowEv (name:posx:posy:dirx:diry:speed:xs) = (\pow -> PowerUpEvent (pow{powPosition = Pos (read posx *  fromIntegral screenWidth)
                                                                                                        (read posy * fromIntegral screenHeight),
                                                                                      powDirection = Vec (read dirx) (read diry),
                                                                                      powSpeed = read speed}))
                                                            <$> checkIfPresent name powerups
        createPowEv _                                   = Nothing
loadEvent _ _ _ _                                 = Nothing

--This function, makes a path relative (for each step, pos * screensize)
relativatePath :: Enemy -> Enemy
relativatePath e@Enemy{ePath = eP, ePosition = (Pos sX sY)} = e{ePath = newPath eP}
   where newPath (Pa ePath) = Pa $ map changePath ePath
         changePath (Move pos)           = Move $ changePos pos
         changePath (MoveR (pos1, pos2)) = MoveR (changePos pos1, changePos pos2)
         changePath (MoveX x) = MoveX (x*fromIntegral screenWidth + sX)
         changePath (MoveY y) = MoveY (y*fromIntegral screenHeight + sY)
         changePath x                    = x
         changePos  (Pos x y) = Pos(x * fromIntegral screenWidth + sX) (y * fromIntegral screenHeight + sY)

--Checks if an enemy or weapon is present in a map
checkIfPresent:: String -> [(String, a)] -> Maybe a
checkIfPresent _ [] = Nothing
checkIfPresent keySearch ((key, val):xs) | keySearch == key = Just val
                                         | otherwise        = checkIfPresent keySearch xs
