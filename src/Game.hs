module Game where
import Board
  ( Board,
    Color(Red, Yellow),
    Column,
    End(Win, Tie),
    getColor,
    showBoard,
    checkWinCondition,
    emptyBoard,
    insert
  )
import Minimax
  ( optimalMoveWithAB,
    Move(M)
  )
import Text.Read (readMaybe)

newtype Game = Game Board deriving (Eq, Show)

initialGame :: Game
initialGame = Game emptyBoard

class Monad m => Interface m where
  getInput :: m String
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

multiPlayer :: Interface m => Game -> m ()
multiPlayer game@(Game b) = do
  playerMessage (getColor b) ("\n" ++ showBoard b)
  case checkWinCondition b of
    Just (Win p)  -> message $ "Player " ++ show p ++ " wins!"
    Just Tie      -> message "It's a Tie!"
    Nothing       -> do
      playerMessage (getColor b) "It's your turn"
      move <- getMove
      case makeMove game move of
        Just game' -> multiPlayer game'
        Nothing    -> do
          playerMessage (getColor b) "Invalid move. Please try again."
          multiPlayer game

singlePlayer :: Interface m => Int -> Game -> m ()
singlePlayer depth game@(Game b) = do
  playerMessage (getColor b) ("\n" ++ showBoard b)
  case checkWinCondition b of
    Just (Win p) -> message $ "Player " ++ show p ++ " wins!"
    Just Tie     -> message "It's a Tie!"
    Nothing      -> do
      -- gets the current player's eval value (likelihood to win)
      case optimalMoveWithAB depth b of
        (val, Just (M move')) -> do
          if getColor b == Red then
            playerMessage Red ("Current value: " ++ show val)
          else playerMessage Yellow ("Current value: " ++ show (negate val))
        _ -> return ()
      playerMessage (getColor b) "It's your turn"
      -- handling a move
      let handleMove m = maybe invalidMove (singlePlayer depth) (makeMove game m)
          invalidMove = do
            playerMessage (getColor b) "Invalid move. Please try again."
            singlePlayer depth game
      case getColor b of
        Red -> getMove >>= handleMove
        Yellow ->
          case optimalMoveWithAB depth b of
            (val, Just (M move')) -> do
              handleMove move'
            _ -> invalidMove

playGame :: Interface m => Game -> m ()
playGame game = do
  let retry = do
        message "Invalid input. Please try again."
        playGame game

  message "Press 1 to play against your friend. Press 2 to play against a bot."
  input <- getInput
  case readMaybe input of
    Just 1 -> multiPlayer game
    Just 2 -> do
      message "Enter bot lookahead depth: "
      input <- getInput
      maybe retry (`singlePlayer` game) (readMaybe input)
    _ -> retry

instance Interface IO where
  getInput :: IO String
  getInput = getLine

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
