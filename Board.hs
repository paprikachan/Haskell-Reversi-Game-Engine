module Board where

import Data.Array
import Data.List

---------------------------------------
-- Data declaration
---------------------------------------

-- Board: consists of Tiles, PositionRing, and Count
data Board = B Tiles PositionRing Count Move deriving Show

{-	Tiles: an array of tiles in board. 
	Use Data.Array to present this data structure.
	Position presents the tile index.
	Tile presents the related tile occupied statust.
-}
type Tiles = Array Position Tile

{-	Position: tile position in board.
 	The first integer ranges from 0 to 7, presenting the column index.
	The second integer ranges from 0 to 7, presenting the row index. -}
type Position = (Int,Int)

{-	Tile: current tile occupied status.
	Black for black piece in tile.
	White for white piece in tile.
	Blank for no piece in tile.
-}
data Tile = Black | Blank | White deriving (Show, Eq)

{-	PositionRing: an array of position pairs.
	Detail explanation is in dissertation Implementation chapter. -}
type PositionRing = [(Position,Position)]

{- 	Count: 
	The first integer is the number of black tiles in board.
	The second integer is the number of white tiles in board. -}
type Count = (Int,Int)

{- Move: the move leads to current board, (-1,-1) for initial board. -}
type Move = Position

-- Instance Declaration -- 

instance Eq Board where
	B b1 posRing1 count1 move1 == B b2 posRing2 count2 move2 = b1 == b2 && posRing1 == posRing2 && count1 == count2 -- && move1 == move2

unequal :: Board -> Board -> Bool
unequal (B b1 pr1 c1 _) (B b2 pr2 c2 _) = b1 /= b2 || pr1 /= pr2 || c1 /= c2 
-- ?

getCount :: Board -> Count
getCount (B _ _ c _) = c
{-}	| x > y = show x ++ "/" ++ show y ++ "/b"
	| x < y = show x ++ "/" ++ show y ++ "/w"
	| otherwise = show x ++ "/" ++ show y ++ "/d"
-}
getMove :: Board -> Move
getMove (B _ _ _ m) = m

----------------------------------------------------------------------
--Initialization Board 
----------------------------------------------------------------------

-- specify the size for standard 8x8 board
size	:: Int
size 	= 8

blankTiles :: [(Position,Tile)]
blankTiles = [ (i,Blank) | i <- range ((0,0),(size-1,size-1)) ]

blankTilesArray = array ((0,0),(size-1,size-1)) blankTiles

initialTiles = array4  
	where
		array1 = blankTilesArray // [((3,3), White)] 	-- insert White in position (3,3)
		array2 = array1 // [((3,4), Black)]				-- insert Black in position (3,4)
		array3 = array2 // [((4,3), Black)]				-- insert Black in position (4,3)
		array4 = array3 // [((4,4), White)]				-- insert White in position (4,4)

board :: [(Position,Tile)]
board =	[ (i,Blank) | i <- range ((0,0),(2,7)) ] ++ 
		[ (i,Blank) | i <- range ((3,0),(3,2)) ] ++
		[ ((3,3),White), ((3,4),Black) ] ++
		[ (i,Blank) | i <- range ((3,5),(3,7)) ] ++
		[ (i,Blank) | i <- range ((4,0),(4,2)) ] ++
		[ ((4,3),Black), ((4,4),White) ] ++
		[ (i,Blank) | i <- range ((4,5),(4,7)) ] ++
		[ (i,Blank) | i <- range ((5,0),(7,7)) ]

initialPositionRing :: PositionRing -- calcalating is too much, assign the values first
initialPositionRing =	((2,4),(3,3)):((2,3),(3,3)):((2,2),(3,3)):((3,2),(3,3)):((4,2),(3,3)):
						((3,2),(4,3)):((4,2),(4,3)):((5,2),(4,3)):((5,3),(4,3)):((5,4),(4,3)):
						((3,5),(4,4)):((4,5),(4,4)):((5,5),(4,4)):((5,4),(4,4)):((5,3),(4,4)):
						((2,3),(3,4)):((2,4),(3,4)):((2,5),(3,4)):((3,5),(3,4)):((4,5),(3,4)):[]

	{-((2,2),(3,3)):
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
	-}
initialBoard = B initialTiles initialPositionRing (2,2) (-1,-1)

----------------------------------------------------------------------
--Board Manipulation: Flipping & Rotating -------------
----------------------------------------------------------------------
{- take 3x3 board as an example
row: convert the board from Board into [[Tile]] type
row 	1 2 3	 1 2 3
		4 5 6 => 4 5 6
		7 8 9 	 7 8 9
-}
row :: Board -> [[Tile]]
row (B b _ _ _) = [ [ b ! (x,y) | x <- [0..size-1]] | y <- [0..size-1] ] 

{- take 3x3 board as an example
flip: flip the board horizontly
flip	1 2 3 	 3 2 1
		4 5 6 => 6 5 4
		7 8 9    9 8 7
-}
flip :: [[Tile]] -> [[Tile]]
flip = (map reverse) 

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


