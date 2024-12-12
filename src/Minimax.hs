module Minimax (
  optimalMoveWithAB,
  Move(M),
  qc
) where
import Board (
  Board,
  Color (Red, Yellow),
  Piece,
  insert,
  emptyBoard,
  exampleBoard30,
  exampleBoardFail,
  getColor,
  checkWinCondition,
  End (Win, Tie), Column,
  getRows, allRowsToList,
  lastTwoAreThreats,
  showBoard,
  getWinningPositions
  )
import State (State)
import State qualified as S
import Data.Maybe (fromJust, isJust)
import Test.QuickCheck
import qualified Test.QuickCheck as QC

type MinimaxState a = S.State Board a

data Move where
  M :: Int -> Move

deriving instance Show Move
deriving instance Eq Move

{-
  Code before:
  we noticed that this is similar to the type of State
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
-}


{-
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
-}

-- | bounded negamax algorithm with alpha beta pruning
-- | outputs a state with board as the Store, and a tuple (value, maybe move)
boundedNegaMaxWithAlphaBeta :: Int -> Int -> Int ->  State Board (Int, Maybe Move)
boundedNegaMaxWithAlphaBeta 0 alpha beta = do
  b <- S.get
  return (evalBoard b, Nothing)
boundedNegaMaxWithAlphaBeta steps alpha beta =
  do
  b <- S.get
  let c = getColor b in
    case checkWinCondition b of
      Just (Win c') | c' == c -> return (1000, Nothing)
      Just (Win c') -> return (-1000, Nothing)
      Just Tie -> return (0, Nothing)
      _ ->
        let (val, move, _, _) = foldr (\x acc@(val', move', alpha', br) ->
              if br then acc else
              case insert b x of
                Nothing -> acc
                Just b' ->
                  let value = negate (fst $ fst (S.runState (boundedNegaMaxWithAlphaBeta (steps - 1) (negate beta) (negate alpha')) b')) in
                    let alpha'' = max alpha' value in
                    if alpha'' >= beta then
                      (value, Just (M x), alpha'', True) -- Does not matter what we return here
                    else if value > val' then
                      (value, Just (M x), alpha'', False)
                    else
                      (val', move', alpha'', br)
              ) (-1001, Nothing, alpha, False) [1..7] in
        return (val, move)


-- heuristics
-- Assume evalBoard returns a value between -100, 100 where
--    100 is winning for P1 (Red) and -100 is winning for P2 (Yellow)
evalBoard :: Board -> Int
evalBoard b =
  case checkWinCondition b of
    Just (Win Yellow) -> -1000
    Just (Win Red) -> 1000
    Just Tie -> 0
    _ ->
      let threats = getWinningPositions b in
        claimEven threats + baseInverse threats + vertical b

-- | implements the claimEven strategy discussed in our project proposal
-- | States that threats on even positions for player 1 are morre threatening then those on odd
-- | and viclaimEven :: [(Int, Int, Color)] -> Int
claimEven =
  foldr (\(i, j, c) acc ->
    case c of
      Red  ->   if even j then acc + 5 else acc + 10
      Yellow -> if even j then acc - 10 else acc - 5
  ) 0

-- | implements the baseInverse strategy in the paper linked in our project proposal
-- | States that horizontal threats are slightly less worse than diagonal threats
baseInverse :: [(Int, Int, Color)] -> Int
baseInverse =
  foldr (\(i, j, c) acc ->
    case (c, j) of
      (Red, 0) -> acc - 1 -- slightly less threatening
      (Yellow, 0) -> acc + 1 -- slightly less threatening
      _ -> 0
  ) 0

-- | implements the vertica strategy in the paper linked in our project proposal
vertical :: Board -> Int
vertical b =
  foldr (\x acc ->
    if lastTwoAreThreats x then 1 + acc
    else acc
  ) 0 (allRowsToList $ getRows b)

{- OLD MIN MAX
  optimalMove :: Board -> Maybe Move
  optimalMove b = snd $ S.evalState (boundedMinMax 6) b
-}

optimalMoveWithAB :: Int -> Board -> (Int, Maybe Move)
optimalMoveWithAB depth = S.evalState $ boundedNegaMaxWithAlphaBeta depth (-1000) 1000

playOptimalMove :: Int -> Board -> Maybe Board
playOptimalMove depth b =
  case snd $ optimalMoveWithAB depth b of
    Nothing -> Nothing
    Just (M i) -> insert b i

playGameTillFinish :: Int -> Board -> Board
playGameTillFinish depth b =
  case playOptimalMove depth b of
    Nothing -> b
    Just b' -> playGameTillFinish depth b'

-- stops early if the game hits a winning state 
playGameForKMoves :: Int -> Int -> Board -> Board
playGameForKMoves _ 0 b = b
playGameForKMoves depth k b =
  case playOptimalMove depth b of
    Nothing -> b
    Just b' -> playGameForKMoves (depth - 1) (k - 1) b'

-- >>> optimalMoveWithAB 5 emptyBoard
-- (0,Just (M 7))

-- >>> optimalMoveWithAB 5 exampleBoard30
-- (-1000,Just (M 6))

printKMoves :: Int -> IO ()
printKMoves x = putStrLn $ showBoard $ playGameForKMoves 3 x emptyBoard


playOptimalMovesAgainst :: Int -> [Int] -> Board -> Maybe Board
playOptimalMovesAgainst depth [] b = Just b
playOptimalMovesAgainst depth (mv:tl) b =
  do
    b' <- playOptimalMove depth b
    b'' <- insert b' mv
    playOptimalMovesAgainst depth tl b''


-- Quick Check

-- ATTEMPTED TEST CASE BUT DOES NOT QUITE WORK FOR OUR MODEL
-- prop_not_stricly_worse :: Board -> Property
-- prop_not_stricly_worse b =
--   QC.forAll (QC.vectorOf 5 (QC.chooseInt (1, 7))) $ \l ->
--     let bShort = playOptimalMovesAgainst 1 l b
--         bTall = playOptimalMovesAgainst 4 l b in
--           isJust bShort && isJust bTall QC.==>
--           let (evalShort, _) = optimalMoveWithAB 4 (fromJust bShort)
--               (evalTall, _) = optimalMoveWithAB 4 (fromJust bTall) in
--                 case getColor $ fromJust bShort of
--                   Red -> evalShort <= evalTall
--                   Yellow -> evalShort >= evalTall


prop_winning_eval_is_still_winning :: Board -> Property
prop_winning_eval_is_still_winning b =
  let (e, m) = optimalMoveWithAB 4 b in
    isJust m && (e == 1000 || e == -1000) QC.==>
      case m of
        Nothing -> False
        Just (M i) ->
          case insert b i of
            Nothing -> False
            Just b' -> fst (optimalMoveWithAB 3 b') == (-e)

-- if a position is winning than playing the optimal moves will you get to 
prop_winning_eval_is_playable_to_winning :: Board -> Property
prop_winning_eval_is_playable_to_winning b =
  let (e, m) = optimalMoveWithAB 3 b in
    isJust m && (e == 1000 || e == -1000) QC.==> 
      let b' = playGameForKMoves 3 3 b in
        evalBoard b' == 1000 || evalBoard b' == -1000


prop_eval_changes_with_depth :: Board -> Property
prop_eval_changes_with_depth b =
  let (e, m) = optimalMoveWithAB 3 b in
    isJust m QC.==>
      case m of
        Nothing -> False
        Just (M i) ->
          case insert b i of
            Nothing -> False
            Just b' -> fst (optimalMoveWithAB 2 b') == (-e)


-- if m is an optimal move then playing m would result in a valid board state
prop_optimal_move_is_valid :: Board -> Bool
prop_optimal_move_is_valid b =
  case optimalMoveWithAB 3 b of
    (_, Nothing) -> True
    (_, Just (M i)) ->
      case insert b i of
        Just b' -> True
        Nothing -> False

qc :: IO ()
qc = do
  putStrLn "optimal_move_is_valid"
  QC.quickCheck prop_optimal_move_is_valid
  putStrLn "winning_eval_is_still_winning"
  QC.quickCheck prop_winning_eval_is_still_winning
  putStrLn "winning_eval_is_playable_to_winning"
  QC.quickCheck prop_winning_eval_is_playable_to_winning
  putStrLn "eval_changes_with_depth"
  QC.quickCheck prop_eval_changes_with_depth
