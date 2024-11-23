module Minimax where
import Board (Board, Piece, insert)

data Move where
  M :: Piece -> Int -> Move

optimalMove :: Board -> Move
optimalMove = undefined

-- Quick Check 
-- check that playing moves results in a >= evaluations (wait is this true)

-- if m is an optimal move then playing m would result in a valid board state
testOptimalMove :: Board -> Bool
testOptimalMove b =
  let M p i = optimalMove b in
    case insert b p i of
      Just b' -> True
      Nothing -> False

