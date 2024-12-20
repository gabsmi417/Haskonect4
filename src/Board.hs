{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Board (
  Board,
  Color(Red, Yellow),
  Column,
  Piece,
  End(Win, Tie),
  getColor,
  getRows,
  emptyBoard,
  exampleBoard30,
  exampleBoardFail,
  insert,
  showBoard,
  checkWinCondition,
  allRowsToList,
  lastTwoAreThreats,
  getWinningPositions,
  test_all,
  qc
) where
import Data.List (transpose, elemIndex)
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
import Data.Maybe (fromMaybe, isJust, fromJust, isNothing)

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

-- | contains all columns
data Rows where
  R :: Column -> Column -> Column -> Column -> Column -> Column -> Column -> Rows

{-
  This used to be our board:
  data Board where
  B :: Color -> Column -> Column -> Column -> Column -> Column -> Column -> Column -> Board

  insert:
  insert :: Board -> Color -> Int -> Maybe Board
  insert b color i = do
    c <- getCol b i
    c' <- insertIntoCol c (NonEmpty color)
    replaceCol b i c'

  arbitrary:
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

  buildBoard :: [(Int, Piece)] -> Board
  buildBoard :: [(Int, Color)] -> Board
  buildBoard = foldr f emptyBoard
    where
      f (i, color) acc =
        fromMaybe emptyBoard $ insert acc color i
  
  makeMove from Game.hs
  makeMove :: Game -> Int -> Maybe Game
  makeMove g i = do
    b <- insert (board g) (current g) i
    case current g of
      Red -> return $ g { board = b, current = Yellow }
      Yellow -> return $ g { board = b, current = Red}
-}

data Board where
  Board :: ColoredBoard c -> Board

data ColoredBoard c where
  Br :: Rows -> ColoredBoard Red
  By :: Rows -> ColoredBoard Yellow

deriving instance Show Color
deriving instance Eq Color

deriving instance Show Piece
deriving instance Eq Piece

deriving instance Show Column
deriving instance Eq Column

deriving instance Show Rows
deriving instance Eq Rows

deriving instance Show (ColoredBoard c)
deriving instance Eq (ColoredBoard c)

deriving instance Show Board
instance Eq Board where
  (==) :: Board -> Board -> Bool
  Board (By r1) == Board (By r2) = r1 == r2
  Board (Br r1) == Board (Br r2) = r1 == r2
  Board _ == Board _ = False

numRows :: Int
numRows = 6

numCols :: Int
numCols = 7

emptyCol :: Column
emptyCol = C Empty Empty Empty Empty Empty Empty

emptyBoard :: Board
emptyBoard = Board (Br (R emptyCol emptyCol emptyCol emptyCol emptyCol emptyCol emptyCol))

-- | get the color of the current board
getColor :: Board -> Color
getColor (Board (Br r)) = Red
getColor (Board (By r)) = Yellow

getRows :: Board -> Rows
getRows (Board (Br r)) = r
getRows (Board (By r)) = r

isColFull :: Board -> Int -> Bool
isColFull b i =
  case getCol b i of
    Just (C _ _ _ _ _ (NonEmpty _)) -> True
    _ -> False

getCol :: Board -> Int -> Maybe Column
getCol b = getColHelper (getRows b)

getColHelper :: (Eq a, Num a) => Rows -> a -> Maybe Column
getColHelper (R c1 c2 c3 c4 c5 c6 c7) i =
  case i of
    1 -> Just c1
    2 -> Just c2
    3 -> Just c3
    4 -> Just c4
    5 -> Just c5
    6 -> Just c6
    7 -> Just c7
    _ -> Nothing

-- Flips the color
replaceCol :: Board -> Int -> Column -> Maybe Board
replaceCol (Board (Br r)) i c' = Just $ Board (By (replaceColHelper r i c'))
replaceCol (Board (By r)) i c' = Just $ Board (Br (replaceColHelper r i c'))

replaceColHelper :: (Eq a, Num a) => Rows -> a -> Column -> Rows
replaceColHelper r@(R c1 c2 c3 c4 c5 c6 c7) i c' =
  case i of
    1 -> R c' c2 c3 c4 c5 c6 c7
    2 -> R c1 c' c3 c4 c5 c6 c7
    3 -> R c1 c2 c' c4 c5 c6 c7
    4 -> R c1 c2 c3 c' c5 c6 c7
    5 -> R c1 c2 c3 c4 c' c6 c7
    6 -> R c1 c2 c3 c4 c5 c' c7
    7 -> R c1 c2 c3 c4 c5 c6 c'
    _ -> r

-- | inserting a piece into a specific column
insertIntoCol :: Column -> Piece -> Maybe Column
insertIntoCol c@(C Empty p2    p3    p4    p5    p6   ) p' = Just $ C p' p2 p3 p4 p5 p6
insertIntoCol c@(C p1    Empty p3    p4    p5    p6   ) p' = Just $ C p1 p' p3 p4 p5 p6
insertIntoCol c@(C p1    p2    Empty p4    p5    p6   ) p' = Just $ C p1 p2 p' p4 p5 p6
insertIntoCol c@(C p1    p2    p3    Empty p5    p6   ) p' = Just $ C p1 p2 p3 p' p5 p6
insertIntoCol c@(C p1    p2    p3    p4    Empty p6   ) p' = Just $ C p1 p2 p3 p4 p' p6
insertIntoCol c@(C p1    p2    p3    p4    p5    Empty) p' = Just $ C p1 p2 p3 p4 p5 p'
insertIntoCol c _ = Nothing -- Column is full

-- | inserts a piece into a given column
insert :: Board -> Int -> Maybe Board
insert b i = do
  c <- getCol b i
  c' <- insertIntoCol c (NonEmpty (getColor b))
  replaceCol b i c'

-- HELPER FUNCTIONS FOR HEURISITCS
lastTwoAreThreats :: Column -> Bool
lastTwoAreThreats col@(C (NonEmpty c) (NonEmpty c') Empty Empty Empty Empty) | c == c' = True
lastTwoAreThreats col@(C _ (NonEmpty c) (NonEmpty c') Empty Empty Empty) | c == c' = True
lastTwoAreThreats col@(C _ _ (NonEmpty c) (NonEmpty c') Empty Empty) | c == c' = True
lastTwoAreThreats _ = False

{- Checking win conditions -}

-- | convert a board to a list of columns
allRowsToList :: Rows -> [Column]
allRowsToList (R c1 c2 c3 c4 c5 c6 c7) = [c1, c2, c3, c4, c5, c6, c7]

-- | convert a column to a list of pieces
columnToList :: Column -> [Piece]
columnToList (C p1 p2 p3 p4 p5 p6) = [p1, p2, p3, p4, p5, p6]

-- | represent a board as a list of pieces, where each list is a column
columns :: Rows -> [[Piece]]
columns r@(R c1 c2 c3 c4 c5 c6 c7) =
  [columnToList c1, columnToList c2, columnToList c3, columnToList c4, columnToList c5, columnToList c6, columnToList c7]

-- | represent a board as a list of pieces, where each list is a row
rows :: Rows -> [[Piece]]
rows r = transpose $ columns r

-- | checks if 4 consecutive pieces in a Piece list are the given color
check4Consecutive :: Color -> [Piece] -> Bool
check4Consecutive c (x1:x2:x3:x4:xs)
  | x1 == NonEmpty c && x1 == x2 && x2 == x3 && x3 == x4 = True
  | otherwise = check4Consecutive c (x2:x3:x4:xs)
check4Consecutive _ _ = False

-- | checks if there's a win for the given color in any of the rows
checkRows :: Rows -> Color -> Bool
checkRows r c = any (check4Consecutive c) (rows r)

-- | checks if there's a win for the given color in any of the columns
checkColumns :: Rows -> Color -> Bool
checkColumns r c = any (check4Consecutive c) (columns r)

-- | get the piece at i, j position (1 indexing)
getPiece :: [[Piece]] -> Int -> Int -> Maybe Color
getPiece b i j = do
  r <- b !? i
  piece <- r !? j
  case piece of
    NonEmpty c -> return c
    _ -> Nothing

-- | get the nth element of a list, returns Nothing if out of bounds
(!?) :: [a] -> Int -> Maybe a
ls !? n
  | n <= 0 = Nothing
  | otherwise = case drop (n - 1) ls of
      (y:_) -> Just y
      _ -> Nothing

-- | checks if there's a win for the given color in any of the antidiagonals
checkAntiDiagonal :: [[Piece]] -> Color -> (Int, Int) -> Bool
checkAntiDiagonal b c (i, j) =
  all (== Just c) [getPiece b (i + k) (j + k) | k <- [0..3]]

-- | checks if there's a win for the given color in any of the diagonals
checkDiagonal :: [[Piece]] -> Color -> (Int, Int) -> Bool
checkDiagonal b c (i, j) =
  all (== Just c) [getPiece b (i - k) (j + k) | k <- [0..3]]

-- | checks diagonal/antidiagonal depending on the function passed in
checkDiagonals :: Rows -> Color -> ([[Piece]] -> Color -> (Int, Int) -> Bool) -> Bool
checkDiagonals r c f =
  any (f (columns r) c) [(i, j) | i <- [1..numRows], j <- [1..numCols]]

-- | checks if the current board is tied
checkTie :: Rows -> Bool
checkTie r = Empty `notElem` concatMap columnToList (allRowsToList r)

-- | checks if the opposite color is winning
checkWinCondition :: Board -> Maybe End
checkWinCondition (Board (Br r)) = checkWinConditionHelper r Yellow
checkWinCondition (Board (By r)) = checkWinConditionHelper r Red

checkWinConditionHelper :: Rows -> Color -> Maybe End
checkWinConditionHelper r c =
  let win = checkRows r c|| checkColumns r c || checkDiagonals r c checkDiagonal || checkDiagonals r c checkAntiDiagonal in
    if win then Just $ Win c
    else
      if checkTie r then Just Tie
      else Nothing

{- Get positions such that if a move with that color is played, the color wins -}

getWinningColumnPositions :: Rows -> Color -> [(Int, Int, Color)]
getWinningColumnPositions r color =
  let cols = zip (columns r) [1..7] in
    foldr f [] cols
    where
      f ([NonEmpty c1, NonEmpty c2, NonEmpty c3, Empty, _, _], j) acc
        | c1 == color && c2 == color && c3 == color = (3, j, color) : acc
      f ([_, NonEmpty c1, NonEmpty c2, NonEmpty c3, Empty, _], j) acc
        | c1 == color && c2 == color && c3 == color = (2, j, color) : acc
      f ([_, _, NonEmpty c1, NonEmpty c2, NonEmpty c3, Empty], j) acc
        | c1 == color && c2 == color && c3 == color = (1, j, color) : acc
      f _ acc = acc

getWinningRowPositions :: Rows -> Color -> [(Int, Int, Color)]
getWinningRowPositions r color =
  foldr f [] (zip (reverse $ rows r) [1..6])
  where
    f (row, i) acc = acc ++ findPositions row i
    findPositions row i =
      [ (i, j, color)
      | (j, Empty) <- zip [1..7] row,
        let left = takeWhile (== NonEmpty color) $ reverse $ take (j - 1) row,
        let right = takeWhile (== NonEmpty color) $ drop j row,
        length left + length right == 3
      ]

-- | generates a list of diagonals, where each diagonal is a list of coordinates
diagonals :: [[(Int, Int)]]
diagonals =
  [
    [(i + k, k + 1) | k <- [0 .. 5], i + k <= numRows]
    | i <- [1..numRows]
  ]
  ++
  [
    [(k + 1, j + k) | k <- [0 .. 5], j + k <= numCols]
    | j <- [2..numCols]
  ]

-- | generates a list of antidiagonals, where each antidiagonal is a list of coordinates
antiDiagonals :: [[(Int, Int)]]
antiDiagonals =
  [
    [(i - k, k + 1) | k <- [0 .. 5], i - k >= 1]
    | i <- [1..numRows]
  ]
  ++
  [
    [(numRows - k, j + k) | k <- [0 .. 5], j + k <= numCols]
    | j <- [2..numCols]
  ]

getWinningDiagonalPositions :: Rows -> [[(Int, Int)]] -> Color -> [(Int, Int, Color)]
getWinningDiagonalPositions r d color =
  concatMap processDiagonal d
  where
    processDiagonal diagonal =
      [(i, j, color) |
        let pieces = getPiece $ reverse $ rows r,
        (i, j) <- diagonal,
        isNothing (pieces i j),
        let idx = elemIndex (i, j) diagonal,
        isJust idx,
        let left
              = takeWhile (\ (x, y) -> pieces x y == Just color)
                  $ reverse $ take (fromJust idx) diagonal,
        let right
              = takeWhile (\ (x, y) -> pieces x y == Just color)
                  $ drop (fromJust idx + 1) diagonal,
        length left + length right == 3]

getWinningPositionsHelper :: Rows -> Color -> [(Int, Int, Color)]
getWinningPositionsHelper r c =
  getWinningColumnPositions r c
  ++ getWinningRowPositions r c
  ++ getWinningDiagonalPositions r diagonals c
  ++ getWinningDiagonalPositions r antiDiagonals c

-- | returns all winning positions
getWinningPositions :: Board -> [(Int, Int, Color)]
getWinningPositions b =
  concatMap (getWinningPositionsHelper $ getRows b) [Red, Yellow]

-- helper function to pretty print the board
showBoard :: Board -> String
showBoard b =
  let r = getRows b in
    let row_ls = reverse $ rows r in
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

{- Test Boards -}

winColBoard :: Board
winColBoard = Board $ Br $ R c1 c2 c3 c4 c5 c6 c7
  where
    c1 = emptyCol
    c2 = emptyCol
    c3 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c4 = C (NonEmpty Red) (NonEmpty Red) (NonEmpty Red) Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Yellow) Empty Empty
    c6 = emptyCol
    c7 = emptyCol

winRowBoard :: Board
winRowBoard = Board $ By $ R c1 c2 c3 c4 c5 c6 c7
  where
    c1 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c2 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c3 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c4 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c5 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty
    c6 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty
    c7 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty

winDiagonalBoard :: Board
winDiagonalBoard = Board $ Br $ R c1 c2 c3 c4 c5 c6 c7
  where
    c1 = emptyCol
    c2 = emptyCol
    c3 = C (NonEmpty Red) (NonEmpty Red) (NonEmpty Red) (NonEmpty Yellow) Empty Empty
    c4 = C (NonEmpty Red) (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Red) Empty Empty
    c6 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty
    c7 = emptyCol

winAntiDiagonalBoard :: Board
winAntiDiagonalBoard = Board $ By $ R c1 c2 c3 c4 c5 c6 c7
  where
    c1 = emptyCol
    c2 = emptyCol
    c3 = emptyCol
    c4 = C (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty
    c6 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Red) Empty Empty Empty
    c7 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) Empty Empty

tiedBoard :: Board
tiedBoard = Board $ Br $ R c1 c2 c3 c4 c5 c6 c7
  where
    c1 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red)
    c2 = C (NonEmpty Red) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red)
    c3 = C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Red) (NonEmpty Red) (NonEmpty Yellow)
    c4 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Red)
    c5 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow)
    c6 = C (NonEmpty Red) (NonEmpty Red) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow)
    c7 = C (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red)

exampleBoard1 :: Board
exampleBoard1 = Board $ By $ R c1 c2 c3 c4 c5 c6 c7
  where
    c1 = emptyCol
    c2 = emptyCol
    c3 = emptyCol
    c4 = C (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty
    c6 = C (NonEmpty Yellow) (NonEmpty Red) Empty Empty Empty Empty
    c7 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) Empty Empty

exampleBoard2 :: Board
exampleBoard2 = Board $ By $ R c1 c2 c3 c4 c5 c6 c7
  where
    c1 = C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow)
    c2 = emptyCol
    c3 = emptyCol
    c4 = emptyCol
    c5 = emptyCol
    c6 = emptyCol
    c7 = emptyCol

exampleBoard3 :: Board
exampleBoard3 = Board $ Br $ R c1 c2 c3 c4 c5 c6 c7
  where
    c1 = C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow)
    c2 = C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow)
    c3 = C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow)
    c4 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red)
    c5 = C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow)
    c6 = C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow)
    c7 = C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) Empty Empty

exampleBoard30 :: Board
exampleBoard30 = Board (Br (R (C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) Empty Empty) (C (NonEmpty Red) Empty Empty Empty Empty Empty) (C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Yellow)) (C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red)) (C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red)) (C (NonEmpty Red) Empty Empty Empty Empty Empty) (C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow))))

exampleBoardFail :: Board
exampleBoardFail = Board (Br (R (C (NonEmpty Yellow) Empty Empty Empty Empty Empty) (C (NonEmpty Red) (NonEmpty Red) Empty Empty Empty Empty) (C (NonEmpty Yellow) Empty Empty Empty Empty Empty) (C (NonEmpty Yellow) (NonEmpty Red) Empty Empty Empty Empty) (C (NonEmpty Red) Empty Empty Empty Empty Empty) (C (NonEmpty Yellow) Empty Empty Empty Empty Empty) (C Empty Empty Empty Empty Empty Empty)))

almostWinColumnBoard :: Board
almostWinColumnBoard = Board $ By $ R c1 c2 c3 c4 c5 c6 c7
  where
    c1 = C (NonEmpty Red) (NonEmpty Red) (NonEmpty Red) Empty Empty Empty
    c2 = C (NonEmpty Yellow) Empty Empty Empty Empty Empty
    c3 = C (NonEmpty Red) (NonEmpty Yellow)  (NonEmpty Yellow)  (NonEmpty Yellow) Empty Empty
    c4 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Red) Empty Empty Empty Empty
    c6 = C (NonEmpty Red) (NonEmpty Red) Empty Empty Empty Empty
    c7 = C (NonEmpty Yellow) (NonEmpty Yellow) Empty Empty Empty Empty

almostWinRowBoard :: Board
almostWinRowBoard = Board $ By $ R c1 c2 c3 c4 c5 c6 c7
  where
    c1 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c2 = C Empty Empty Empty Empty Empty Empty
    c3 = C (NonEmpty Red) (NonEmpty Yellow)  (NonEmpty Yellow)  Empty Empty Empty
    c4 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Red) Empty Empty Empty Empty
    c6 = C (NonEmpty Yellow) (NonEmpty Red) Empty Empty Empty Empty
    c7 = C (NonEmpty Yellow) (NonEmpty Red) Empty Empty Empty Empty

almostWinDiagonalBoard :: Board
almostWinDiagonalBoard = Board $ By $ R c1 c2 c3 c4 c5 c6 c7
  where
    c1 = C (NonEmpty Red) Empty Empty Empty Empty Empty
    c2 = C (NonEmpty Red) (NonEmpty Red) Empty Empty Empty Empty
    c3 = C (NonEmpty Yellow) (NonEmpty Yellow)  (NonEmpty Red)  (NonEmpty Red) Empty Empty
    c4 = C (NonEmpty Red) (NonEmpty Red) Empty Empty Empty Empty
    c5 = C (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) Empty Empty Empty
    c6 = C (NonEmpty Yellow) (NonEmpty Yellow) Empty Empty Empty Empty
    c7 = C (NonEmpty Yellow) (NonEmpty Red) Empty Empty Empty Empty

{- Unit Tests -}

testInsert :: Maybe Board
testInsert =
  do
    t1 <- insert emptyBoard 1
    t2 <- insert t1         1
    t3 <- insert t2         1
    insert t3               1

test_insert :: Test
test_insert =
  "insert tests"
    ~: TestList
      [
        insert emptyBoard 1 ~?= Just (Board $ By $ R (C (NonEmpty Red) Empty Empty Empty Empty Empty) emptyCol emptyCol emptyCol emptyCol emptyCol emptyCol),
        testInsert ~?= Just (Board $ Br $ R (C (NonEmpty Red) (NonEmpty Yellow) (NonEmpty Red) (NonEmpty Yellow) Empty Empty) emptyCol emptyCol emptyCol emptyCol emptyCol emptyCol),
        insert exampleBoard2 1 ~?= Nothing
      ]

test_win_conditions :: Test
test_win_conditions =
  "win conditions tests"
    ~: TestList
      [
        checkWinCondition winColBoard ~?= Just (Win Yellow),
        checkWinCondition winRowBoard ~?= Just (Win Red),
        checkWinCondition winDiagonalBoard ~?= Just (Win Yellow),
        checkWinCondition winAntiDiagonalBoard ~?= Just (Win Red),
        checkWinCondition tiedBoard ~?= Just Tie,
        checkWinCondition exampleBoard3 ~?= Nothing,
        checkWinCondition emptyBoard ~?= Nothing,
        checkWinCondition exampleBoard1 ~?= Nothing
      ]

test_winning_positions :: Test
test_winning_positions =
  "winning positions tests"
  ~: TestList
  [
    getWinningPositions almostWinColumnBoard ~?= [(3,1,Red),(2,3,Yellow)],
    getWinningPositions almostWinRowBoard ~?= [(6,2,Red),(5,4,Red)],
    getWinningPositions almostWinDiagonalBoard ~?= [(3,4,Red),(3,4,Yellow)],
    getWinningPositions emptyBoard ~?= [],
    getWinningPositions exampleBoard3 ~?= []
  ]

{- Quick Check Tests -}

instance Arbitrary Board where
  arbitrary :: Gen Board
  arbitrary = do
    numMoves <- QC.choose (0, 10)
    moves <- QC.vectorOf numMoves (QC.chooseInt (1, 7))
    return $ buildBoard moves

buildBoard :: [Int] -> Board
buildBoard = foldr f emptyBoard
  where
    f i acc =
      fromMaybe emptyBoard $ insert acc i

count :: Color -> Board -> Int
count c (Board (Br r)) = countHelper c r
count c (Board (By r)) = countHelper c r

countHelper :: Color -> Rows -> Int
countHelper c (R c1 c2 c3 c4 c5 c6 c7) =
  foldr f 0 [c1, c2, c3, c4, c5, c6, c7]
  where
    f col acc =
      acc + length (filter (== NonEmpty c) (columnToList col))

-- check to see if numRed == numYellow || numRed == numYellow + 1 after simulate game (asuming not nothing)

prop_color_balance :: Board -> Bool
prop_color_balance b =
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

prop_valid_board_filling :: Board -> Bool
prop_valid_board_filling b =
  foldr (\c acc -> testValidColumn c && acc) True (allRowsToList $ getRows b)

-- add insert get color returns the other color
prop_insert_flips_color :: Board -> Property
prop_insert_flips_color b =
  QC.forAll (QC.choose (1, numCols)) $ \i ->
    isJust (insert b i) QC.==>
    getColor b /= getColor (fromJust $ insert b i)

prop_insert_same_col_4_times :: Board -> Bool
prop_insert_same_col_4_times b = fromMaybe True $ do
  b1 <- insert b 1
  b1 <- insert b1 2
  b1 <- insert b1 1
  b1 <- insert b1 3
  b1 <- insert b1 1
  b2 <- insert b1 4
  b1 <- insert b2 1
  return $
    checkWinCondition b1 == Just (Win (getColor b2))
    -- note checkWinCondition uses opposite color of given board due to IO
    -- so we want to use the color for b2

prop_insert_3_winning_position_not_empty :: Board -> Bool
prop_insert_3_winning_position_not_empty b = fromMaybe True $ do
  b1 <- insert b 1
  b1 <- insert b1 2
  b1 <- insert b1 1
  b1 <- insert b1 3
  b1 <- insert b1 1
  return $
    not (null $ getWinningPositions b1) || isColFull b1 1
  
-- >>>  test_all
-- Counts {cases = 11, tried = 11, errors = 0, failures = 0}

test_all :: IO Counts
test_all = runTestTT $ TestList [test_insert, test_win_conditions, test_winning_positions]

qc :: IO ()
qc = do
  putStrLn "color_balance"
  QC.quickCheck prop_color_balance
  putStrLn "valid_board_filling"
  QC.quickCheck prop_valid_board_filling
  putStrLn "insert_flips_color"
  QC.quickCheck prop_insert_flips_color
  putStrLn "insert_same_col_4_times"
  QC.quickCheck prop_insert_same_col_4_times
  putStrLn "insert_3_winning_position_not_empty"
  QC.quickCheck prop_insert_3_winning_position_not_empty
