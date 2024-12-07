module Game where
import Board (
  Board,
  Color(Red, Yellow),
  Column,
  End(Win, Tie),
  getColor,
  showBoard,
  checkWinCondition,
  emptyBoard,
  insert
  )

newtype Game = Game Board deriving (Eq, Show)

initialGame :: Game
initialGame = Game emptyBoard

class Monad m => Interface m where
  -- ask the current player for their next move
  getMove       :: m Int
  -- send a message to all players
  message       :: String -> m ()
  -- send a message to the indicated player
  playerMessage :: Color -> String -> m ()

makeMove :: Game -> Int -> Maybe Game
makeMove (Game b) i = do
  b <- insert b i
  return $ Game b

-- | make moves until someone wins
playGame :: Interface m => Game -> m ()
playGame game@(Game b) = do
  message $ showBoard b
  let color = getColor b
  playerMessage color "It's your turn."
  move <- getMove
  case makeMove game move of
    Just game'@(Game b') -> do
      case checkWinCondition b' of
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
