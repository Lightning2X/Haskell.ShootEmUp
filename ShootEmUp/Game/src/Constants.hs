module Constants where

--This module contains global variables. 

--Screen resolution
screenWidth::Int
screenWidth = 1280

screenHeight::Int
screenHeight = 720

topBorder::Int --This is used by the score bar. All gameobjects should not be able to move through this bar.
topBorder = 26


--Draw the debug (mouseposition, hitboxes)
renderDebug::Bool
renderDebug = True

--The name of file that contains all stages
rootStage::String
rootStage = "AllStages"

dTr :: Float
dTr = pi / 180
