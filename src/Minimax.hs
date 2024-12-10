module Minimax where
import Board (Board, Color (Red, Yellow), Piece, insert, emptyBoard, getColor, checkWinCondition, End (Win), Column, getRows, allRowsToList, lastTwoAreThreats)
import State (State)
import State qualified as S

type MinimaxState a = S.State Board a

data Move where
  M :: Int -> Move

deriving instance Show Move
deriving instance Eq Move

isLegalMove :: Board -> Move -> Bool
isLegalMove = undefined

optimalMove :: Board -> Maybe Move
optimalMove b = snd $ S.evalState (boundedMinMax 6) b

boundedMinMax :: Int -> State Board (Int, Maybe Move)
boundedMinMax 0 = do
  b <- S.get
  return (evalBoard b, Nothing)
boundedMinMax steps = do
  b <- S.get
  let moves = foldr (\x acc ->
        case insert b x of
          Nothing -> acc
          Just b' ->
            (fst $ fst (S.runState (boundedMinMax (steps - 1)) b'), Just (M x) ): acc
        ) [] [1..7]
  case getColor b of
    Red -> return $ maxOfMoves moves
    Yellow -> return $ minOfMoves moves

maxOfMoves :: [(Int, Maybe Move)] -> (Int, Maybe Move)
maxOfMoves =
  foldr (\(i', m') (i, m) ->
    if i' > i then (i', m') else (i, m)
  ) (-101, Just (M (-1)))

minOfMoves :: [(Int, Maybe Move)] -> (Int, Maybe Move)
minOfMoves =
  foldr (\(i', m') (i, m) ->
    if i' < i then (i', m') else (i, m)
  ) (101, Just (M (-1)))

checkIfFirstGreater :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
checkIfFirstGreater e1 e2 e3 e4 e5 e6 e7 =
  e1 > e2 && e1 > e3 && e1 > e4 && e1 > e5 && e1 > e6 && e1 > e7

-- heuristics
-- Assume evalBoard returns a value between -100, 100 where
--    100 is winning for P1 (Red) and -100 is winning for P2 (Yellow)
evalBoard :: Board -> Int
evalBoard b =
  case checkWinCondition b of
    Just (Win Yellow) -> -1000
    Just (Win Red) -> 1000
    _ ->
      let threats = getAllThreats b in
        claimEven threats + baseInverse threats + vertical b

claimEven :: [(Int, Int, Color)] -> Int
claimEven =
  foldr (\(i, j, c) acc ->
    case c of
      Red  ->   if even j then acc + 5 else acc + 10
      Yellow -> if even j then acc - 10 else acc - 5
  ) 0

baseInverse :: [(Int, Int, Color)] -> Int
baseInverse =
  foldr (\(i, j, c) acc ->
    case (c, j) of
      (Red, 0) -> acc - 1 -- slightly less threatening
      (Yellow, 0) -> acc + 1 -- slightly less threatening
      _ -> 0
  ) 0

vertical :: Board -> Int
vertical b =
  foldr (\x acc ->
    if lastTwoAreThreats x then 1 + acc
    else acc
  ) 0 (allRowsToList $ getRows b)

getAllThreats :: Board -> [(Int, Int, Color)]
getAllThreats b = []

-- >>> optimalMove emptyBoard

-- turning on heuristics shouldn't give you worse heuristics

-- Use algo playing against itself for checking if increasing search depth results in >= outcome

-- Quick Check 
-- check that playing moves results in a >= evaluations (wait is this true)

-- if m is an optimal move then playing m would result in a valid board state
testOptimalMove :: Board -> Bool
testOptimalMove b =
  let Just (M i) = optimalMove b in
    case insert b i of
      Just b' -> True
      Nothing -> False

-- keep track of number of steps
