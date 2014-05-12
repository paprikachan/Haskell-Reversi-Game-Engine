
module Play where

import Board
import Data.Array
import Data.List


type Player = Tile
type Direction = (Int,Int)
type TilesLineTail = ([Position],[Position])

-- Move --

-- already a valid move!
{- 	move: returns the next board for a valid move.
	Inputs: specified player, valid move, current board.
	Return: updated board.
-}
move 										:: Player -> Position -> Board -> Board
move p m board@(B b ps (x,y) _) -- insertTile -> flipTiles -> updatePositionRing -> updateCount 
	| p == Black		 					= updateCount updatedPositionRingBoard (1+flippedTilesNum, -flippedTilesNum)
	| p == White 							= updateCount updatedPositionRingBoard (-flippedTilesNum, 1+flippedTilesNum)
											where 
												insertedBoard 					= insertTile p m board
												(flippedBoard,flippedTilesNum) 	= flipTiles p m insertedBoard
												updatedPositionRingBoard 			= updatePositionRing p m flippedBoard
												
												

-- Helper Function for Move --
-- 40'
{-	insertTile: inserts a specified piece to board in specified position.
	Inputs: specified piece colour, specified position, current board
	Return: partly updated board after inserting. 
-}
insertTile 									:: Tile -> Position -> Board -> Board
insertTile t p (B b pps c _)				= B (b // [(p,t)]) pps c p


-- testing
-- (b,n) = flipTiles Black (3,2) initialBoard

-- 50'
{-	flipTiles: flips inside opponent's pieces after placing a piece.
	input: newly placed piece, tile position for the newly placed piece, board after piece placed.
	output: partly updated board after flipping all inside opponent's pieces, the number of flipped opponent's pieces.
-}
flipTiles 									:: Tile -> Position -> Board -> (Board,Int)
flipTiles t p board@(B _ pps _ _) 			= case ps of
												{- 	If the input position is guaranteed valid, there is at least one pair whose second element 
													is a blanked tile position adjacent to the input position.
												-}			
												[] 				-> error "Cannot be empty list in flipTiles, the input position must be valid." 
												-- pass
												otherwise	 	-> checkAndFlip t ps (board,0)    
											where
												{- 	ps: array of pairs whose first element is the same with input position,
													which means all the second elements in each pair are positions of blanked
													tiles adjacent to the input position.
												-}
												ps 										:: PositionRing
												ps 										= [ pp | pp <- pps, fst pp == p ] -- fliter unrelated positions pair 
												{- 	checkAndFlip: check whether the blanked position stored in position ring starts a valid tiles line,
													if true, flip all opponent's tiles in that tiles line and increase the number of flipped tiles.
													if not, check the next blanked position stored in position ring.
												-}		
												checkAndFlip							:: Tile -> PositionRing -> (Board,Int) -> (Board,Int) 
												checkAndFlip t [] (b,n) 				= (b,n)
												checkAndFlip t ((bp,op):ps') (b,n)	
													| validTilesLine t b tilesLine 		= checkAndFlip t ps' (flip t (fst tilesLine) (b,n))
													| otherwise							= checkAndFlip t ps' (b,n)
													where
														tilesLine 						= tilesLineTail t bp dir b
														dir 							= (fst op - fst bp, snd op - snd bp)
												{-	flip: 
													input: 	the tile colour should be flipped into, array of tile positions should be flipped, 
															a pair of current board and the number of current flipped tiles.
													return:	the board after flipping and the number of total flipped tiles.
												-}
												flip 									:: Tile -> [Position] -> (Board,Int) -> (Board,Int)
												flip t [] (b,n) 						= (b,n)
												flip t (pos:poss) ((B b pps' c m),n) 	= flip t poss (B (b // [(pos,t)]) pps' c m,n+1)

-- 20'
{-	updatePositionRing: 
	The first thing is adding all new valid positions pairs, 
	the first elements in pairs are blanked tile positions adjacent
	to the inserted tile in all possible direction.
	The second thing is deleting the broken positions pairs.
	Broken pairs are pairs whose first element is inserted tile position 
	which is not blank any more.
-}
updatePositionRing 							:: Tile -> Position -> Board -> Board
											-- new valid positions + original positions after deleting operation
updatePositionRing t p (B b pps c _)		=  B b ((filter ((== Blank) . (b !) . fst) (positionRing p)) ++ [ pp | pp <- pps, fst pp /= p]) c p
											where
												-- positionRing: generates positionRing for the provided position in all possible direction.
												positionRing :: Position -> PositionRing
												positionRing (x,y) = [ ((x+i,y+j),(x,y)) | (i,j) <- directions, elem (x+i) [0..7], elem (y+j) [0..7]]
												-- directions: 7 possible tiles line direction.
												directions :: [Direction]
												directions = [(-1,-1),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0)]

-- updateCount: update the number of black pieces and white pieces for board.
updateCount 								:: Board -> Count -> Board
updateCount (B b pps (x,y) p)	(i,j) 		 = B b pps (x+i,y+j) p



--Logic Judgments--






-- testing
{-
ps = let (B _ pps _) = initialBoard in [ pp | pp <- pps, fst pp == (3,2) ]
bool = validTilesLine Black (3,2) (0,1) initialBoard -- X
pair = tileLinesPair Black (3,2) (0,1) initialBoard
-}

-- 20'
-- isValidMove: return True if the given move for the specified player is valid in provided board, Flase otherwise.
isValidMove									:: Tile -> Move -> Board -> Bool
isValidMove t p board@(B _ pps _ _)			= case ps of
												[] 			-> False
												-- if there is at least one valid tiles line, the position is a valid move.
												otherwise 	-> or [ validTilesLine t board (tilesLineTail t bp (fst op - fst bp, snd op - snd bp) board) | (bp,op) <- ps ]
											where
												{- 	ps: array of pairs whose first element is the same with input position,
													which means all the second elements in each pair are positions of blanked
													tiles adjacent to the input position.
												-}
												ps :: PositionRing
												ps = [ pp | pp <- pps, fst pp == p ]

-- isEnd: return True if there is no valid move for both players in provided board, False otherwise.
isEnd										:: Player -> Board -> Bool
isEnd p b 									= nextMoves p b == [] && nextMoves (reversePlayer p) b == []





-- nextMoves: provide all possible next valid moves for a specified tile colour in board.
nextMoves 									:: Tile -> Board -> [Position]
nextMoves t board@(B b pps _ _) 			= nub [ ip | (ip,_) <- pps, isValidMove t ip board]


reversePlayer 								:: Player -> Player
reversePlayer Black 						= White
reversePlayer White 						= Black
reversePlayer _ 							= error "blank cannot be reversed"



-- 50'
{-	validTilesLine, a valid tiles line should have the following properties:
					- the sequence of opponent's tiles cannot be empty;
					- the sequence of same tiles cannot be empty;
					- the head of second array should have the same tile colour.
	input: 	the head tile in testing tiles line, which is a blanked tiles adjacent to occupied tiles;
		 	the current board
		 	the tail of testing tiles line
	return: True if valid, False otherwise.
-}
validTilesLine 								:: Tile -> Board -> TilesLineTail -> Bool
validTilesLine t (B b _ _ _)  (ops,sps) 
	-- ops is empty, which means the adjacent tile for start tile is not opponent's colour, so tiles line is invalid.
	| ops == [] 							= False
	-- sps is empty, which means there is no tile with same colour in the rest of tiles line, so tiles line is invlid.
	| sps == []								= False
	-- the first element of sps is the same tile, which means there is no blank tile between oppontent's tiles and 
	-- the same tiles. Therefore, it is a valid tileLines. 
	| b ! (head sps) == t					= True
	{- 	the rest of situations are invalid.
	 	validTilesLine Black ([White,Blank],[Blank,Black,Blank]) = Flase
	 -}
	-- dropWhile (==White) [White,Blank,Black,White] = [Blank,Black,White]
	| otherwise								= False


-- testing
-- pair = tileLinesPair Black (3,2) (0,1) initialBoard


{- 	tilesLineTail: calculate the tail of a tiles line.
	intput: the start tile in the tiles line, which is a blanked tiles adjacent to occupied tiles;
			the position of that tile;
			the direction of the testing tiles line;
			current board.
	return: the first element in pair is the sequence of opponent's tiles;
			the second element in pair is the sequence of tiles starts with a same colour tile.
-}
tilesLineTail 								:: Tile -> Position -> Direction -> Board -> TilesLineTail
{-
takeWhile (/=Black) [White,Blank,Black,White] = [White,Blank]
dropWhile (==White) [White,Blank,Black,White] = [Blank,Black,White]
-}
											-- 		
tilesLineTail t (x,y) (i,j) (B b _ _ _) 	= (takeWhile ((/=t) . (b!)) ps,dropWhile ((== (reversePlayer t)) . (b!)) ps)
											where
												-- ps is the sequence of positions in the tail of the tiles line.
												ps = [ (x+i*n,y+j*n) | n <- [1..tilesLineTailLength (x,y) (i,j)] ]
												{-	tilesLineTailLength:
													input:	start position for a tiles line;
															direction for that tiles line.
													return: the length of the tail of the tiles line, which is the number of the rest of 
															tiles in that tiles line in the specified direction till the board ends.
													eg: 	tilesLineTailLength (3,2) (0,1) = 6
												-}
												tilesLineTailLength :: Position -> Direction -> Int
												tilesLineTailLength (x,y) (i,j)
													| (i,j) == (-1,-1) 	= min x y
													| (i,j) == (-1,0)	= x
													| (i,j) == (-1,1)	= min x (size-1-y)
													| (i,j) == (0,-1)	= y	
													| (i,j) == (0,0)	= error "Tiles line direction cannnot be (0,0)."
													| (i,j) == (0,1) 	= size-1-y
													| (i,j) == (1,-1)	= min (size-1-x) y
													| (i,j) == (1,0)	= size-1-x
													| (i,j) == (1,1)	= min (size-1-x) size-1-y
													| otherwise			= error ("Invalid tiles direction " ++ show (i,j) ++ ".")
													{-}
													| i' < 0 	&& j' < 0 	= min x' y'
													| i' < 0 	&& j' == 0 	= x'
													| i' < 0 	&& j' > 0 	= min x' (size-1-y')
													| i' == 0 	&& j' < 0 	= y'
													| i' == 0 	&& j' == 0 	= error "direction cannot be (0,0)"
													| i' == 0 	&& j' > 0 	= size-1-y'
													| i' > 0 	&& j' < 0 	= min (size-1-x') y'
													| i' > 0 	&& j' == 0 	= size-1-x'
													| i' > 0 	&& j' > 0 	= min (size-1-x') (size-1-y')
													-}		


-- nextValidBoards: returns next valid boards for a given board without duplicate symmetric boards and its related move
nextValidBoards 					:: Tile -> Board -> [Board]
nextValidBoards t b 			 	= filterSymmetric [ move t m b | m <- nextMoves t b]

---------------------------------------------------------------------
-- fliterSymmetric: reduce any symmetric boards for each board in array.
filterSymmetric 					:: [Board] -> [Board]
filterSymmetric [] 					= []
filterSymmetric (b:bs)				= b : filterSymmetric (filter ((== False) . isSymmetric b) bs) 

-- isSymmetric: returns True if the two board is symmetric (same before or after flipping or rotating operations), False otherwise. 
isSymmetric :: Board -> Board -> Bool
isSymmetric b1 b2 = elem (row b1) bs
	where
	bs = row b2 : rotate90 (row b2) : rotate180 (row b2) : rotate270 (row b2) :
		 flipped : rotate90 flipped : rotate180 flipped : rotate270 flipped : []
	flipped = Board.flip (row b2)
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







