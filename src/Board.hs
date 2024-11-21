

module Board where

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

