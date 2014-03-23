module Board where

import Data.Array
import Data.List

---------------------------------------
-- Data declaration
---------------------------------------

type Pos = (Int,Int)

data Tile = Black | Blank | White deriving (Show, Eq)

type BArray = Array Pos Tile

data Board = B BArray PosRing Count deriving Show

instance Eq Board where
	B b1 posRing1 count1 == B b2 posRing2 count2 = b1 == b2 && posRing1 == posRing2 && count1 == count2

unequal :: Board -> Board -> Bool
unequal (B b1 pr1 c1) (B b2 pr2 c2) = b1 /= b2 || pr1 /= pr2 || c1 /= c2 

-- PosRing: the pos is around position, the snd pos is related tile pos
type PosRing = [(Pos,Pos)]

type Player = Tile
type Move = Pos
type Dir = (Int,Int)

-- number of black tiles in board; number of white tiles in board
type Count = (Int,Int)

countString :: Board -> String
countString (B _ _ (x,y)) 
	| x > y = show x ++ show y ++ "b"
	| x < y = show x ++ show y ++ "w"
	| otherwise = show x ++ show y ++ "d"

----------------------------------------------------------------------
--Initialization
----------------------------------------------------------------------

-- specify the size for standard 8x8 board
size	:: Int
size 	= 8

board :: [(Pos,Tile)]
board =	[ (i,Blank) | i <- range ((0,0),(2,7)) ] ++ 
		[ (i,Blank) | i <- range ((3,0),(3,2)) ] ++
		[ ((3,3),White), ((3,4),Black) ] ++
		[ (i,Blank) | i <- range ((3,5),(3,7)) ] ++
		[ (i,Blank) | i <- range ((4,0),(4,2)) ] ++
		[ ((4,3),Black), ((4,4),White) ] ++
		[ (i,Blank) | i <- range ((4,5),(4,7)) ] ++
		[ (i,Blank) | i <- range ((5,0),(7,7)) ]

initialPosRing :: PosRing -- calcalating is too much, assign the values first
initialPosRing = ((2,2),(3,3)):
				 ((3,2),(3,3)):((3,2),(4,3)):
				 ((4,2),(3,3)):((4,2),(4,3)):
				 ((5,2),(4,3)):
				 ((5,3),(4,3)):((5,3),(4,4)):
				 ((5,4),(4,3)):((5,4),(4,4)):
				 ((5,5),(4,4)):
				 ((4,5),(4,4)):((4,5),(3,4)):
				 ((3,5),(4,4)):((3,5),(3,4)):
				 ((2,5),(3,4)):
				 ((2,4),(3,4)):((2,4),(3,3)):
				 ((2,3),(3,4)):((2,3),(3,3)):
				 []

initialBoard = B (array ((0,0),(size-1,size-1)) board) initialPosRing (2,2)

----------------------------------------------------------------------
--Board Manipulation: Flodding & Rotating -------------

{- take 3x3 board as an example
row:
row 	1 2 3	 1 2 3
		4 5 6 => 4 5 6
		7 8 9 	 7 8 9
-}
row :: Board -> [[Tile]]
row (B b _ _) = [ [ b ! (x,y) | x <- [0..size-1]] | y <- [0..size-1] ] 

{- take 3x3 board as an example
fold: fold the board along vertival line
fold	1 2 3 	 3 2 1
		4 5 6 => 6 5 4
		7 8 9    9 8 7
-}
fold :: [[Tile]] -> [[Tile]]
fold = (map reverse) 

{-
rotate90: rotate the board clockwise 90'
rotate90
		1 2 3    7 4 1
		4 5 6 => 8 5 2
		7 8 9    9 6 3
-}
rotate90 :: [[Tile]] -> [[Tile]]
rotate90 = reverse . (map reverse) . transpose . (map reverse)

{-
rotate180: rotate the board clockwise 180'
rotate180
		1 2 3    7 4 1 	  9 8 7	
		4 5 6 => 8 5 2 => 6 5 4
		7 8 9    9 6 3    3 2 1
-}
rotate180 :: [[Tile]] -> [[Tile]]
rotate180 = reverse . (map reverse)

{-
rotate270: rotate the board clockwise 270'
rotate270
		1 2 3    7 4 1 	  9 8 7	   3 6 9
		4 5 6 => 8 5 2 => 6 5 4 => 2 5 8
		7 8 9    9 6 3    3 2 1    1 4 7
-}
rotate270 :: [[Tile]] -> [[Tile]]
rotate270 = transpose . (map reverse)
{-
{- take 3x3 board as example
flipV: flip the board vertically
flipV   1 2 3	 3 2 1
		4 5 6 => 6 5 4
		7 8 9	 9 8 7
-}

flipV :: Board -> [[Tile]]
flipV = (map reverse) . row

{- take 3x3 board as an example
flipH: flip the board horizontally
flipH   1 2 3	 7 8 9	
		4 5 6 => 4 5 6
		7 8 9	 1 2 3
-}

flipH :: Board -> [[Tile]]
flipH = reverse . row
{- take 3x3 board as an example
flipC: flip the board along the "top-left to bottom-right" diagonal line
flipC  	1 2 3	 1 4 7
		4 5 6 => 2 5 8
		7 8 9	 3 6 9
-}
flipC :: Board -> [[Tile]]
flipC = transpose . row

{- take 3x3 board as an example
flipD: flip the board along the "bottom-left to up-right" diagonal line
flipD 	1 2 3	 9 6 3
		4 5 6 => 8 5 2
		7 8 9	 7 4 1
-}
flipD :: Board -> [[Tile]]
flipD = reverse . (map reverse) . transpose . row

-- testing:
b = insertTile Black (0,1) initialBoard
rb = row b
fVb = flipV b
fHb = flipH b
fCb = flipC b
fDb = flipD b
-}
-- Board Checking: is symmetric (same before or after flodding and rotating) ----------------------
isSymmetric :: Board -> Board -> Bool
isSymmetric b1 b2 = elem (row b1) bs
	where
	bs = row b2 : rotate90 (row b2) : rotate180 (row b2) : rotate270 (row b2) :
		 foldded : rotate90 foldded : rotate180 foldded : rotate270 foldded : []
	foldded = fold (row b2)
-- testing:
{-
b = insertTile Black (0,1) initialBoard
r = row b
r1 = rotate90 r
r2 = rotate180 r 
r3 = rotate270 r
f = fold r
fr1 = rotate90 f 
fr2 = rotate180 f
fr3 = rotate270 f
-}
---------------------------------------------------------------------

