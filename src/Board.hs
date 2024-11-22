module Board where
import Data.List (transpose)
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))

-- Basic Board definition without Gadts

data Color where
  Red :: Color
  Yellow :: Color

data Piece where
  Empty :: Piece
  NonEmpty :: Color -> Piece

{-
An alternate option for the following objects are using [Piece] lists and [Column] lists
However this allows the api to create invalid size boards or inconsistet boards sizes
but it does allow for nicer inserts
-}

data Column where
  C :: Piece -> Piece -> Piece -> Piece -> Piece -> Piece -> Column


data Board where
  B :: Column -> Column -> Column -> Column -> Column -> Column -> Column -> Board

deriving instance Show Color
deriving instance Eq Color

deriving instance Show Piece
deriving instance Eq Piece

deriving instance Show Column
deriving instance Eq Column

deriving instance Show Board
deriving instance Eq Board

numRows :: Int
numRows = 6

numCols :: Int
numCols = 7

emptyCol :: Column
emptyCol = C Empty Empty Empty Empty Empty Empty

emptyBoard :: Board
emptyBoard = B emptyCol emptyCol emptyCol emptyCol emptyCol emptyCol emptyCol

getCol :: Board -> Int -> Maybe Column
getCol b@(B c1 c2 c3 c4 c5 c6 c7) i =
  case i of
    1 -> Just c1
    2 -> Just c2
    3 -> Just c3
    4 -> Just c4
    5 -> Just c5
    6 -> Just c6
    7 -> Just c7
    _ -> Nothing

replaceCol :: Board -> Int -> Column -> Maybe Board
replaceCol b@(B c1 c2 c3 c4 c5 c6 c7) i c' =
  case i of
    1 -> Just $ B c' c2 c3 c4 c5 c6 c7
    2 -> Just $ B c1 c' c3 c4 c5 c6 c7
    3 -> Just $ B c1 c2 c' c4 c5 c6 c7
    4 -> Just $ B c1 c2 c3 c' c5 c6 c7
    5 -> Just $ B c1 c2 c3 c4 c' c6 c7
    6 -> Just $ B c1 c2 c3 c4 c5 c' c7
    7 -> Just $ B c1 c2 c3 c4 c5 c6 c'
    _ -> Nothing

insertIntoCol :: Column -> Piece -> Maybe Column
insertIntoCol c@(C Empty p2    p3    p4    p5    p6   ) p' = Just $ C p' p2 p3 p4 p5 p6
insertIntoCol c@(C p1    Empty p3    p4    p5    p6   ) p' = Just $ C p1 p' p3 p4 p5 p6
insertIntoCol c@(C p1    p2    Empty p4    p5    p6   ) p' = Just $ C p1 p2 p' p4 p5 p6
insertIntoCol c@(C p1    p2    p3    Empty p5    p6   ) p' = Just $ C p1 p2 p3 p' p5 p6
insertIntoCol c@(C p1    p2    p3    p4    Empty p6   ) p' = Just $ C p1 p2 p3 p4 p' p6
insertIntoCol c@(C p1    p2    p3    p4    p5    Empty) p' = Just $ C p1 p2 p3 p4 p5 p'
insertIntoCol c _ = Nothing -- Column is full


insert :: Board -> Piece -> Int -> Maybe Board
insert b p i = do
  c <- getCol b i
  c' <- insertIntoCol c p
  replaceCol b i c'

-- >>> insert emptyBoard (NonEmpty Red) 1
-- Just (B (C (NonEmpty Red) Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty))

testInsert :: Maybe Board
testInsert =
  do
    t1 <- insert emptyBoard (NonEmpty Red)    1
    t2 <- insert t1         (NonEmpty Yellow) 1
    t3 <- insert t2         (NonEmpty Yellow) 1
    insert t3               (NonEmpty Red)    1

-- >>> testInsert
-- Just (B (C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Red) Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty))

-- | convert a column to a row of pieces
columnToList :: Column -> [Piece]
columnToList (C p1 p2 p3 p4 p5 p6) = [p1, p2, p3, p4, p5, p6]

-- | represent a board as a list of pieces, where each list is a column
columns :: Board -> [[Piece]]
columns (B c1 c2 c3 c4 c5 c6 c7) =
  [columnToList c1, columnToList c2, columnToList c3, columnToList c4, columnToList c5, columnToList c6, columnToList c7]

-- | represent a board as a list of pieces, where each list is a row
rows :: Board -> [[Piece]]
rows b = transpose $ columns b

-- | checks if 4 consecutive pieces in a Piece list are the given color
check4Consecutive :: Color -> [Piece] -> Bool
check4Consecutive c (x1:x2:x3:x4:xs)
  | x1 == NonEmpty c && x1 == x2 && x2 == x3 && x3 == x4 = True
  | otherwise = check4Consecutive c (x2:x3:x4:xs)
check4Consecutive _ _ = False

checkRows :: Board -> Color -> Bool
checkRows b c = any (check4Consecutive c) (rows b)

checkColumns :: Board -> Color -> Bool
checkColumns b c = any (check4Consecutive c) (columns b)

-- | get the piece at i, j position (1 indexing)
getPiece :: [[Piece]] -> Int -> Int -> Maybe Color
getPiece b i j = do
  r <- b !? i
  piece <- r !? j
  case piece of
    NonEmpty c -> return c
    _ -> Nothing

-- | get the nth index of a list, returns Nothing if out of bounds
(!?) :: [a] -> Int -> Maybe a
ls !? n
  | n < 0 = Nothing
  | otherwise = case drop (n - 1) ls of
      (y:_) -> Just y
      _ -> Nothing

checkDiagonal :: [[Piece]] -> Color -> (Int, Int) -> Bool
checkDiagonal b c (i, j) =
  all (== Just c) [getPiece b (i + k) (j + k) | k <- [0..3]]

checkAntiDiagonal :: [[Piece]] -> Color -> (Int, Int) -> Bool
checkAntiDiagonal b c (i, j) =
  all (== Just c) [getPiece b (i - k) (j + k) | k <- [0..3]]

checkDiagonals :: Board -> Color -> ([[Piece]] -> Color -> (Int, Int) -> Bool) -> Bool
checkDiagonals b c f =
  any (f (rows b) c) [(i, j) | i <- [1..numRows], j <- [1..numCols]]

checkWinCondition :: Board -> Color -> Bool
checkWinCondition b c = checkRows b c || checkColumns b c || checkDiagonals b c checkDiagonal || checkDiagonals b c checkAntiDiagonal

winColBoard :: Board
winColBoard = B c1 c2 c3 c4 c5 c6 c7
  where
    c1 = emptyCol
    c2 = emptyCol
    c3 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c4 = C (NonEmpty Red) (NonEmpty Red) (NonEmpty Red) Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Yellow) Empty Empty
    c6 = emptyCol
    c7 = emptyCol

winRowBoard :: Board
winRowBoard = B c1 c2 c3 c4 c5 c6 c7
  where
    c1 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c2 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c3 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c4 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c5 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty
    c6 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty
    c7 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty

winDiagonalBoard :: Board
winDiagonalBoard = B c1 c2 c3 c4 c5 c6 c7
  where
    c1 = emptyCol
    c2 = emptyCol
    c3 = C (NonEmpty Red) (NonEmpty Red) (NonEmpty Red) (NonEmpty Yellow) Empty Empty
    c4 = C (NonEmpty Red) (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Red) Empty Empty
    c6 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty
    c7 = emptyCol

winAntiDiagonalBoard :: Board
winAntiDiagonalBoard = B c1 c2 c3 c4 c5 c6 c7
  where
    c1 = emptyCol
    c2 = emptyCol
    c3 = emptyCol
    c4 = C (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty
    c6 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Red) Empty Empty Empty
    c7 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) Empty Empty

exampleBoard1 :: Board
exampleBoard1 = B c1 c2 c3 c4 c5 c6 c7
  where
    c1 = emptyCol
    c2 = emptyCol
    c3 = emptyCol
    c4 = C (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty
    c6 = C (NonEmpty Yellow) (NonEmpty Red) Empty Empty Empty Empty
    c7 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) Empty Empty

test_win_conditions :: Test
test_win_conditions =
  "win conditions tests"
    ~: TestList
      [
        checkWinCondition winColBoard Yellow ~?= True,
        checkWinCondition winColBoard Red ~?= False,
        checkWinCondition winRowBoard Red ~?= True,
        checkWinCondition winDiagonalBoard Yellow ~?= True,
        checkWinCondition winDiagonalBoard Red ~?= False,
        checkWinCondition winAntiDiagonalBoard Red ~?= True,
        checkWinCondition emptyBoard Red ~?= False,
        checkWinCondition exampleBoard1 Red ~?= False
      ]

-- >>> runTestTT test_win_conditions
-- Counts {cases = 8, tried = 8, errors = 0, failures = 0}
