module Game where
import Board (
  Board,
  Color(Red, Yellow),
  Column,
  End(Win, Tie),
  showBoard,
  checkWinCondition,
  emptyBoard,
  insert
  )

data Game = Game {
  board :: Board,
  current :: Color
} deriving (Eq, Show)

initialGame :: Game
initialGame = Game { board = emptyBoard, current = Red }

class Monad m => Interface m where
  -- ask the current player for their next move
  getMove       :: m Int
  -- send a message to all players
  message       :: String -> m ()
  -- send a message to the indicated player
  playerMessage :: Color -> String -> m ()

makeMove :: Game -> Int -> Maybe Game
makeMove g i = do
  b <- insert (board g) (current g) i
  case current g of
    Red -> return $ g { board = b, current = Yellow }
    Yellow -> return $ g { board = b, current = Red}

-- | make moves until someone wins
playGame :: Interface m => Game -> m ()
playGame game = do
  message $ showBoard (board game)
  let color = current game
  playerMessage color "It's your turn."
  move <- getMove

  case makeMove game move of
    Just game' -> do
      case checkWinCondition (board game') (current game) of
        Just (Win winner) -> message $ "Player " ++ show winner ++ " wins!"
        Just Tie -> message "It's a Tie!"
        Nothing -> playGame game'
    Nothing -> do
      playerMessage color "Invalid move. Please try again."
      playGame game


instance Interface IO where
  getMove :: IO Int
  getMove = do
    putStrLn "Enter your move (column number 1-7):"
    input <- getLine
    case reads input of
      [(n, "")] | n >= 1 && n <= 7 -> return n
      _ -> do
        putStrLn "Invalid input. Please enter a number between 1 and 7."
        getMove

  playerMessage :: Color -> String -> IO ()
  playerMessage c msg = do
    case c of
      Red -> putStr "Red: " 
      Yellow -> putStr "Yellow: "
    putStrLn msg
  
  message :: String -> IO ()
  message = putStrLn

main :: IO ()
main = playGame initialGame
