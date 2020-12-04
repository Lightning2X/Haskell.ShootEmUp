module GameOverModule where
import Model
import Objects
import Data.List
import FileManager

--GameOver logic
--If a player has won, he has to insert this name, this name can be changed with this function. 
changeName:: String -> GameState -> GameState
changeName s gstate@GameState {infoToShow = GameOver (HasWon ins _, i)} = gstate{infoToShow = GameOver (HasWon ins s, i)}
changeName _ gstate = gstate

--If a player has won, he has to insert this name, this name can be retrieved with this function. 
getName:: GameState -> String
getName GameState {infoToShow = GameOver (HasWon _ name, _)} = name
getName _ = ""

--This function removes a character from a string
decreaseString:: String -> String
decreaseString xs = take (length xs - 1) xs

{-This function confirms that the player has inserted his name and 
sets the Inserted flag to Inserted-}
setToInserted:: GameState -> GameState
setToInserted gstate@GameState {infoToShow = GameOver (HasWon NotInserted name, i)} = gstate{infoToShow = GameOver (HasWon Inserted name,i)}
setToInserted gstate = gstate

{-This function is called when the game ends. This function, loads the score (with help of filemanager) and adds the new score if necessary.
It also loads the scorelist into the gamestate-}
handleGameOver:: String -> GameState -> IO GameState
handleGameOver name gstate@GameState {infoToShow =(GameOver (w, info@GameInfo {stage = st@Stage {scores = sc, sName = scorePath}})) } =
           do let newScore = checkHighscore (name,score info)  sc
              writeScore scorePath newScore
              return gstate{infoToShow = GameOver (w, info{stage = st{scores = newScore}})}
handleGameOver _ gstate                                     = return gstate

--Sort high score and ensure there are 10 records at max
checkHighscore:: (String,Score) -> [(String,Score)] -> [(String,Score)]
checkHighscore val xs = take 10 newList
  where newList = sortBy sorter (val:xs)
        sorter (n1, s1) (n2, s2)
           | s1 < s2 = GT
           | s1 > s2 = LT
           | s1 == s2 = compare n1 n2