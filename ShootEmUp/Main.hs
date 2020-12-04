module Main where

import System.Directory 
import Data.List

{-This is the main function in the levelmanager. 
This function exists of a standard intro text and a loop function-}
main:: IO()
main = do putStrLn "This application can be used to add stages or delete stages. write help for help"
          putStrLn "Please use functions like addstage name path with no extra parentheses"
          putStrLn "Please use no extensions for filenames, only for paths"
          questionLoop 

{-This function waits for input. If the player presses <ENTER>, the input is evaluated.
This function is either executed or discarded (if it is invalid). Afterwards, the loop function is called again.
-}
questionLoop::IO()
questionLoop = do inst <-getLine 
                  case words inst of 
                     ("help":_)               -> help 
                     ("addStage":xs)          -> case xs of 
                        (fileName:path:_)       -> addStage fileName path
                        _                       -> putStrLn "Error in addStage syntax"
                     ("addStageInBetween":xs) -> case xs of 
                        (fileName:path:pos:_)   -> addStageInBetween fileName path (read pos)
                        _                       -> putStrLn "Error in addStageInBetween"
                     ("requestStages":_)      -> requestStages
                     ("removeStage":stage:_)  -> removeStage stage 
                     _                        -> putStrLn "Sorry I do not understand that instruction."
                  questionLoop 

--This function simply returns a list of text with all the functionalities and how to use them. 
help:: IO()
help = do putStrLn "Please use functions like addstage name path with no extra parentheses"
          putStrLn "Please use no extensions for filenames, only for paths"
          putStrLn "1. addStage fileName (e.g. StageUnknown) path. Will be the firststage. the file must not be present already."
          putStrLn "Example: addStage StageUnknown C:/map/StageUnknown.txt. "
          putStrLn "2. addStageInBetween fileName path pos (Int). "
          putStrLn "Example: addStageInBetween StageUnknown C:/map/StageUnknown.txt 2."
          putStrLn "This stage will now be 2, so the previous stage 2(if it exists) is now 3."
          putStrLn "2. removeStage fileName (e.g. StageUnknown)"
          putStrLn "Example: removeStage StageUnknown"
          putStrLn "3. requestStages. returns a list of all stages in order"

--The file that contains the list of stages. This is always this file by default. 
rootStage:: String
rootStage = "AllStages.txt"

--Look for the stagefolder in the game folder. 
stageFolder:: IO String
stageFolder =  (++ "/Game/content/Stages") <$> getCurrentDirectory

--Get the score folder from the stagefolder. 
stageScoreFolder:: IO String
stageScoreFolder = (++"/Score") <$> stageFolder

{-This function is supposed to add a stage if no order of the stage is requested (first, second, etc.).
The stage will then become the first stage by default. So an addstage function is called
with position 0.
-} 
addStage:: String -> String -> IO() 
addStage fileName path = addStageInBetween fileName path 0


{-This function adds a stage to the game. It needs a name (that will become the stagename), a path
and a position. The path is used to retrieve the file and load it into the stagefolder.
The position determines the location in the stagelist (and thefore also the order in the game) .
The stage is also added to the stagelist and a scorefile is added to the scorefolder. 
The stage is also checked on duplicacy and if it exists. 
-}
addStageInBetween:: String -> String -> Int -> IO()
addStageInBetween fileName path pos = do dir <-stageFolder
                                         exists <- doesFileExist path 
                                         case exists of
                                           True -> do content <- readFile $ dir++ "/" ++ rootStage
                                                      length content `seq` case lines content of --Force that the file is read 
                                                       xs -> case elem fileName xs of
                                                         True  -> putStrLn "This stage already exists, please rename the file."
                                                         False -> do writeFile (dir ++ "/" ++ rootStage) (unlines $ changeStageList fileName pos xs) --Add to stageList 
                                                                     addScoreFile fileName --Add a scorefile
                                                                     addToStageMap fileName path --Add to map
                                                                     putStrLn "Stage added succesfully."
                                           False -> putStrLn "This file does not exist"

{-This function adds a stagename in a stagenamelist in a certain position. 
If the position is longer than the list, the name will be last. -}
changeStageList:: String -> Int -> [String] -> [String]
changeStageList newStage pos stages = before ++ (newStage:after)
  where (before, after) = splitAt pos stages 

{-Create a scorefile for a certain stage in the scorefolder -}
addScoreFile:: String -> IO()
addScoreFile stageName = stageScoreFolder >>= (\dir -> writeFile (dir++"/"++stageName++"Score.txt") "")

{-Copy the stage to the stagemap -}
addToStageMap:: String -> String -> IO()
addToStageMap fileName path = do content <- readFile path  
                                 dir     <- stageFolder
                                 length content `seq` (writeFile (dir++"/"++fileName++".txt") content) --force that file is read

{-remove a stage from the stageslist and stagemap (if it exists)-}
removeStage:: String -> IO()
removeStage fileName = do dir <- stageFolder
                          let path = (dir++ "/" ++ fileName++".txt")
                          exists <- doesFileExist path 
                          case exists of 
                            True -> do removeFile path  
                                       contentAllStages <- readFile $ dir++ "/" ++ rootStage 
                                       length contentAllStages `seq` writeFile (dir ++ "/" ++ rootStage) 
                                                                    (unlines $ filter (/= fileName) $ lines contentAllStages) 
                                       putStrLn "The file was completely removed." 
                            False -> putStrLn "The file you are trying to delete does not exist"

{-returns a list with the names of all the stages -}                          
requestStages:: IO()
requestStages = do dir<-stageFolder
                   content <- readFile $ dir ++ "/" ++ rootStage 
                   length content `seq` putStrLn $ (intercalate "\n") $ lines content