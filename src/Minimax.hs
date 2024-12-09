module Minimax where
import Board (Board, Color (Red, Yellow), Piece, insert, emptyBoard, getColor, checkWinCondition, End (Win))

data Move where
  M :: Int -> Move

deriving instance Show Move
deriving instance Eq Move

isLegalMove :: Board -> Move -> Bool
isLegalMove = undefined

optimalMove :: Board -> Maybe Move
optimalMove b = snd $ boundedMinMax b 5

-- TODO: Could refactor this with State Monad
boundedMinMax :: Board -> Int -> (Int, Maybe Move)
boundedMinMax b 0 = (evalBoard b, Nothing)
boundedMinMax b steps = 
  let moves = foldr (\x acc -> 
        case insert b x of
          Nothing -> acc
          Just b' -> 
            (fst $ boundedMinMax b' (steps - 1), Just (M x)) : acc
        ) [] [1, 2, 3, 4, 5, 6, 7]
  in
    case getColor b of
      Red -> maxOfMoves moves
      Yellow -> minOfMoves moves

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


-- huristics
-- Assume evalBoard returns a value between -100, 100 where
--    100 is winning for P1 (Red) and -100 is winning for P2 (Yellow)
evalBoard :: Board -> Int
evalBoard b = 
  case checkWinCondition b of
    Just (Win Yellow) -> -100
    Just (Win Red) -> 100
    _ -> 0

-- >>> optimalMove emptyBoard
-- Just (M 7)


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
