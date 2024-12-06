module Board (
  Board,
  Color(Red, Yellow),
  Column,
  Piece,
  End(Win, Tie),
  emptyBoard,
  insert,
  showBoard,
  checkWinCondition
) where
import Data.List (transpose)
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    Testable (..),
    (==>),
    elements,
    chooseInt
  )
import qualified Test.QuickCheck as QC
import Data.Maybe (fromMaybe)

-- Basic Board definition without Gadts

data Color where
  Red :: Color
  Yellow :: Color

-- Assumption: Red always goes first

data Piece where
  Empty :: Piece
  NonEmpty :: Color -> Piece

data End = Win Color | Tie deriving (Eq, Show)

{-
An alternate option for the following objects are using [Piece] lists and [Column] lists
However this allows the api to create invalid size boards or inconsistet boards sizes
but it does allow for nicer inserts
-}

data Column where
  C :: Piece -> Piece -> Piece -> Piece -> Piece -> Piece -> Column

-- add color, Vec
data Board where
  B :: Color -> Column -> Column -> Column -> Column -> Column -> Column -> Column -> Board

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
emptyBoard = B Red emptyCol emptyCol emptyCol emptyCol emptyCol emptyCol emptyCol

isColFull :: Board -> Int -> Bool
isColFull b i =
  case getCol b i of
    Just (C _ _ _ _ _ (NonEmpty _)) -> True
    _ -> False

-- have a validity check - combine col and color

getCol :: Board -> Int -> Maybe Column
getCol b@(B c c1 c2 c3 c4 c5 c6 c7) i =
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
replaceCol b@(B c c1 c2 c3 c4 c5 c6 c7) i c' =
  case i of
    1 -> Just $ B c c' c2 c3 c4 c5 c6 c7
    2 -> Just $ B c c1 c' c3 c4 c5 c6 c7
    3 -> Just $ B c c1 c2 c' c4 c5 c6 c7
    4 -> Just $ B c c1 c2 c3 c' c5 c6 c7
    5 -> Just $ B c c1 c2 c3 c4 c' c6 c7
    6 -> Just $ B c c1 c2 c3 c4 c5 c' c7
    7 -> Just $ B c c1 c2 c3 c4 c5 c6 c'
    _ -> Nothing

insertIntoCol :: Column -> Piece -> Maybe Column
insertIntoCol c@(C Empty p2    p3    p4    p5    p6   ) p' = Just $ C p' p2 p3 p4 p5 p6
insertIntoCol c@(C p1    Empty p3    p4    p5    p6   ) p' = Just $ C p1 p' p3 p4 p5 p6
insertIntoCol c@(C p1    p2    Empty p4    p5    p6   ) p' = Just $ C p1 p2 p' p4 p5 p6
insertIntoCol c@(C p1    p2    p3    Empty p5    p6   ) p' = Just $ C p1 p2 p3 p' p5 p6
insertIntoCol c@(C p1    p2    p3    p4    Empty p6   ) p' = Just $ C p1 p2 p3 p4 p' p6
insertIntoCol c@(C p1    p2    p3    p4    p5    Empty) p' = Just $ C p1 p2 p3 p4 p5 p'
insertIntoCol c _ = Nothing -- Column is full


insert :: Board -> Color -> Int -> Maybe Board
insert b color i = do
  c <- getCol b i
  c' <- insertIntoCol c (NonEmpty color)
  replaceCol b i c'

-- >>> insert emptyBoard (NonEmpty Red) 1
-- Just (B (C (NonEmpty Red) Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty))

testInsert :: Maybe Board
testInsert =
  do
    t1 <- insert emptyBoard Red    1
    t2 <- insert t1         Yellow 1
    t3 <- insert t2         Red 1
    insert t3               Yellow    1

-- >>> testInsert
-- Just (B (C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Red) Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty))

-- | convert a board to a list of columns
boardToList :: Board -> [Column]
boardToList (B c c1 c2 c3 c4 c5 c6 c7) = [c1, c2, c3, c4, c5, c6, c7]

-- | convert a column to a list of pieces
columnToList :: Column -> [Piece]
columnToList (C p1 p2 p3 p4 p5 p6) = [p1, p2, p3, p4, p5, p6]

-- | represent a board as a list of pieces, where each list is a column
columns :: Board -> [[Piece]]
columns (B c c1 c2 c3 c4 c5 c6 c7) =
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

checkTie :: Board -> Bool
checkTie b = Empty `notElem` concatMap columnToList (boardToList b)

checkWinCondition :: Board -> Color -> Maybe End
checkWinCondition b c =
  let win = checkRows b c || checkColumns b c || checkDiagonals b c checkDiagonal || checkDiagonals b c checkAntiDiagonal in
    if win then Just $ Win c
    else
      if checkTie b then Just Tie
      else Nothing

winColBoard :: Board
winColBoard = B Yellow c1 c2 c3 c4 c5 c6 c7
  where
    c1 = emptyCol
    c2 = emptyCol
    c3 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c4 = C (NonEmpty Red) (NonEmpty Red) (NonEmpty Red) Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Yellow) Empty Empty
    c6 = emptyCol
    c7 = emptyCol

winRowBoard :: Board
winRowBoard = B Red c1 c2 c3 c4 c5 c6 c7
  where
    c1 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c2 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c3 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c4 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c5 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty
    c6 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty
    c7 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty

winDiagonalBoard :: Board
winDiagonalBoard = B Yellow c1 c2 c3 c4 c5 c6 c7
  where
    c1 = emptyCol
    c2 = emptyCol
    c3 = C (NonEmpty Red) (NonEmpty Red) (NonEmpty Red) (NonEmpty Yellow) Empty Empty
    c4 = C (NonEmpty Red) (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Red) Empty Empty
    c6 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty
    c7 = emptyCol

winAntiDiagonalBoard :: Board
winAntiDiagonalBoard = B Red c1 c2 c3 c4 c5 c6 c7
  where
    c1 = emptyCol
    c2 = emptyCol
    c3 = emptyCol
    c4 = C (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty
    c6 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Red) Empty Empty Empty
    c7 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) Empty Empty

exampleBoard1 :: Board
exampleBoard1 = B Red c1 c2 c3 c4 c5 c6 c7
  where
    c1 = emptyCol
    c2 = emptyCol
    c3 = emptyCol
    c4 = C (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty
    c6 = C (NonEmpty Yellow) (NonEmpty Red) Empty Empty Empty Empty
    c7 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) Empty Empty

exampleBoard2 :: Board
exampleBoard2 = B Red c1 c2 c3 c4 c5 c6 c7
  where
    c1 = C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow)
    c2 = emptyCol
    c3 = emptyCol
    c4 = emptyCol
    c5 = emptyCol
    c6 = emptyCol
    c7 = emptyCol

test_insert :: Test
test_insert =
  "insert tests"
    ~: TestList
      [
        insert emptyBoard Red 1 ~?= Just (B Yellow (C (NonEmpty Red) Empty Empty Empty Empty Empty) emptyCol emptyCol emptyCol emptyCol emptyCol emptyCol),
        testInsert ~?= Just (B Red (C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) Empty Empty) emptyCol emptyCol emptyCol emptyCol emptyCol emptyCol),
        insert exampleBoard2 Red 1 ~?= Nothing
      ]

-- >>> runTestTT test_insert
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

test_win_conditions :: Test
test_win_conditions =
  "win conditions tests"
    ~: TestList
      [
        checkWinCondition winColBoard Yellow ~?= Just (Win Yellow),
        checkWinCondition winColBoard Red ~?= Nothing,
        checkWinCondition winRowBoard Red ~?= Just (Win Red),
        checkWinCondition winDiagonalBoard Yellow ~?= Just (Win Yellow),
        checkWinCondition winDiagonalBoard Red ~?= Nothing,
        checkWinCondition winAntiDiagonalBoard Red ~?= Just (Win Red),
        checkWinCondition emptyBoard Red ~?= Nothing,
        checkWinCondition exampleBoard1 Red ~?= Nothing
      ]

-- >>> runTestTT test_win_conditions
-- Counts {cases = 8, tried = 8, errors = 0, failures = 0}

-- Unit tests to add
-- check to see if inserting into full results in nothing
-- check to see if inersting into non-full results in something

-- Quick Check Test Cases
instance Arbitrary Board where
  arbitrary :: Gen Board
  arbitrary = do
    numMoves <- QC.choose (0, 10)
    redMoves <- QC.vectorOf numMoves (QC.chooseInt (1, 7))
    yellowMoves <- QC.vectorOf numMoves (QC.chooseInt (1, 7))
    let redMoves' = map (, Red) redMoves
        yellowMoves' = map (, Yellow) yellowMoves
    return $ buildBoard $ interleave redMoves' yellowMoves'

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x : xs) (y : ys) = x : y : interleave xs ys

buildBoard :: [(Int, Color)] -> Board
buildBoard = foldr f emptyBoard
  where
    f (i, color) acc =
      fromMaybe emptyBoard $ insert acc color i

instance Arbitrary Color where
  arbitrary :: Gen Color
  arbitrary = elements [Red, Yellow]

count :: Color -> Board -> Int
count c b = undefined

simulateGame :: [Int] -> [Int] -> Board -> Maybe Board
simulateGame redMoves yellowMoves b = undefined

-- check to see if numRed == numYellow || numRed == numYellow + 1 after simulate game (asuming not nothing)

testColorBalance :: Board -> Bool
testColorBalance b =
  let r = count Red b
      y = count Yellow b in
    r == y || r == y + 1

-- check to see if board filled lower rows before higher ones after simulate game

testValidColumn :: Column -> Bool
testValidColumn c@(C Empty Empty Empty Empty Empty Empty) = True
testValidColumn c@(C (NonEmpty p1) Empty Empty Empty Empty Empty) = True
testValidColumn c@(C (NonEmpty _) (NonEmpty _) Empty Empty Empty Empty) = True
testValidColumn c@(C (NonEmpty _) (NonEmpty _) (NonEmpty _) Empty Empty Empty) = True
testValidColumn c@(C (NonEmpty _) (NonEmpty _) (NonEmpty _) (NonEmpty _) Empty Empty) = True
testValidColumn c@(C (NonEmpty _) (NonEmpty _) (NonEmpty _) (NonEmpty _) (NonEmpty _) Empty) = True
testValidColumn c@(C (NonEmpty _) (NonEmpty _) (NonEmpty _) (NonEmpty _) (NonEmpty _) (NonEmpty _)) = True
testValidColumn _ = False

testValidBoardFilling :: Board -> Bool
testValidBoardFilling b =
  foldr (\c acc -> testValidColumn c && acc) True (boardToList b)

-- helper function to print the board
showBoard :: Board -> String
showBoard b =
  let row_ls = reverse $ rows b in
    let str_ls = "1 2 3 4 5 6 7" : toString row_ls in
      unlines str_ls
    where
      toString ls = do
        l <- ls
        return $ do
          x <- l
          f x
      f Empty = ". "
      f (NonEmpty Red) = "x "
      f (NonEmpty Yellow) = "o "

-- >>> showBoard emptyBoard
-- "1 2 3 4 5 6 7\n. . . . . . . \n. . . . . . . \n. . . . . . . \n. . . . . . . \n. . . . . . . \n. . . . . . . \n"
-- >>> showBoard exampleBoard1
-- "1 2 3 4 5 6 7\n. . . . . . . \n. . . . . . . \n. . . . . . x \n. . . . o . o \n. . . o x x x \n. . . x o o o \n"
-- >>> showBoard winAntiDiagonalBoard
-- "1 2 3 4 5 6 7\n. . . . . . . \n. . . . . . . \n. . . . . . x \n. . . . o x o \n. . . o x x x \n. . . x o o o \n"
