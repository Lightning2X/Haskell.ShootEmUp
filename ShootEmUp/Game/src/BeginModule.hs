module BeginModule where

import Model 



{-This datatype is used by the function switchlevelmenu.
It is used to determine if the player wants to select the next level or a previous one. -}
data LevelSwitch = NextLevel | PreviousLevel
  deriving (Eq)

{-This function allows players to select an earlier or a later level. 
This function also wraps. So if you want to select an earlier level than level 0,
  the last level is selected. -} 
switchLevelMenu:: GameState -> LevelSwitch -> GameState
switchLevelMenu gstate@GameState {infoToShow = (Begin selected stss@(st:_))}
   switch | switch == NextLevel = findNextStage $ stss++[st] --Similar to sentineldata
          | otherwise           = findNextStage (reverse $ stss++[st]) --Search in reverse
  where findNextStage (x:yss@(y:_)) | x==selected = gstate{infoToShow = Begin y stss}
                                    | otherwise   = findNextStage yss
        findNextStage _                           = gstate
switchLevelMenu gstate _ = gstate