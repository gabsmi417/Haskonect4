module Game where
import Board ( Board, Color, Column, End(Win, Tie), showBoard, checkWinCondition )

data Game = Game {
  board :: Board,
  current :: Color
} deriving (Eq, Show)

initialGame :: Game
initialGame = undefined

class Monad m => Interface m where
  -- ask the current player for their next move
  getMove       :: Game -> m Int
  -- send a message to all players
  message       :: String -> m ()
  -- send a message to the indicated player
  playerMessage :: Color -> String -> m ()

makeMove :: Game -> Int -> Maybe Game
makeMove = undefined

-- | make moves until someone wins
playGame :: Interface m => Game -> m ()
playGame game = do
  let color = current game
      gameBoard = board game
  playerMessage color $ showBoard gameBoard
  case checkWinCondition gameBoard color of
    Just (Win color)  -> message $ "Player " ++ show color ++ " wins!"
    Just Tie -> message "It's a Tie!"
    Nothing -> do
      playerMessage (current game) "It's your turn"
      move <- getMove game
      case makeMove game move of
        Just game' -> playGame game'
        Nothing    -> error "BUG: move is invalid!"

instance Interface IO where
  getMove       = undefined
  playerMessage = undefined
  message       = undefined

main :: IO ()
main = playGame initialGame