
module Play where

import Board
import Data.Array
import Data.List

move 										:: Player -> Move -> Board -> Board
move p m board@(B b ps (x,y)) -- insertTile -> flipTiles -> addTilesRing -> updateCount 
	| p == Black		 					= updateCount addedTilesRingBoard (1+flippedTilesNum, -flippedTilesNum)
	| p == White 							= updateCount addedTilesRingBoard (-flippedTilesNum, 1+flippedTilesNum)
											where 
												addedTilesRingBoard 			= addTilesRing p m flippedBoard
												(flippedBoard,flippedTilesNum) 	= flipTiles p m insertedBoard
												insertedBoard 					= insertTile p m board

-- 40'

insertTile 									:: Tile -> Pos -> Board -> Board
insertTile t p (B b pps c)					= B (b // [(p,t)]) pps c 


-- testing
-- (b,n) = flipTiles Black (3,2) initialBoard

-- 50'
flipTiles 									:: Tile -> Pos -> Board -> (Board,Int)
flipTiles t p board@(B _ pps _) 			= case ps of
												[] 				-> error "cannot be empty list in flipTiles" 
												otherwise	 	-> checkaFlip t ps (board,0)    
											where
												ps 										= [ pp | pp <- pps, fst pp == p ] 
												
												checkaFlip								:: Tile -> [(Pos,Pos)] -> (Board,Int) -> (Board,Int) 
												checkaFlip t [] (b,n) 					= (b,n)
												checkaFlip t ((ip,op):ps') (b,n)	
													| validTilesLine t ip dir b 		= checkaFlip t ps' (flip t (fst (tileLinesPair t ip dir b)) (b,n))
													| otherwise							= checkaFlip t ps' (b,n)
													where
														dir 							= (fst op - fst ip, snd op - snd ip)
												flip 									:: Tile -> [Pos] -> (Board,Int) -> (Board,Int)
												flip t [] (b,n) 						= (b,n)
												flip t (pos:poss) ((B b pps' c),n) 		= flip t poss (B (b // [(pos,t)]) pps' c,n+1)

-- 20'
addTilesRing 								:: Tile -> Pos -> Board -> Board
addTilesRing t p (B b pps c)				=  B b ((filter ((== Blank) . (b !) . fst) (positions p)) ++ [ pp | pp <- pps, fst pp /= p]) c 
											where
												positions :: Pos -> [(Pos,Pos)]
												positions (x,y) = [ ((x+i,y+j),(x,y)) | (i,j) <- [(-1,-1),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0)], elem (x+i) [0..7], elem (y+j) [0..7]]



updateCount 								:: Board -> Count -> Board
updateCount (B b pps (x,y))	(i,j) 			= B b pps (x+i,y+j)



--------------Logic Decision-----------------

-- testing
{-
ps = let (B _ pps _) = initialBoard in [ pp | pp <- pps, fst pp == (3,2) ]
bool = validTilesLine Black (3,2) (0,1) initialBoard -- X
pair = tileLinesPair Black (3,2) (0,1) initialBoard
-}

-- 20'
isValidMove									:: Tile -> Pos -> Board -> Bool
isValidMove t p board@(B _ pps _)			= case ps of
												-- ps is empty
												[] 			-> False
												otherwise 	-> or [ validTilesLine t ip (fst op - fst ip, snd op - snd ip) board | (ip,op) <- ps ]
											where
												ps = [ pp | pp <- pps, fst pp == p ]
									

nextMoves 									:: Tile -> Board -> [Pos]
nextMoves t board@(B b pps _) 				= nub [ ip | (ip,_) <- pps, isValidMove t ip board]


reversePlayer 								:: Player -> Player
reversePlayer Black 						= White
reversePlayer White 						= Black
reversePlayer _ 							= error "blank cannot be reversed"

-- 50'

validTilesLine 								:: Tile -> Pos -> Dir -> Board -> Bool
validTilesLine t p d board@(B b _ _) 
	-- first element is same, rps is empty
	| rps == [] 							= False
	-- all the tiles are reverse tile, which means no same tile at the end
	| sps == []								= False
	-- the first element of sps is the same tile
	| b ! (head sps) == t					= True
	-- dropWhile (==White) [White,Blank,Black,White] = [Blank,Black,White]
	| otherwise								= False
											where 
												(rps,sps) = tileLinesPair t p d board


-- testing
-- pair = tileLinesPair Black (3,2) (0,1) initialBoard


-- inserted tile; inserted tile position; direction; board; (sequence of reverse tiles, sequence of same tiles)
tileLinesPair 								:: Tile -> Pos -> Dir -> Board -> ([Pos],[Pos])
{-
takeWhile (/=Black) [White,Blank,Black,White] = [White,Blank]
dropWhile (==White) [White,Blank,Black,White] = [Blank,Black,White]
-}
tileLinesPair t (x,y) (i,j) (B b _ _) 		= (takeWhile ((/=t) . (b!)) ps,dropWhile ((== (reversePlayer t)) . (b!)) ps)
											where
												ps = [ (x+i*n,y+j*n) | n <- [1..tilesLineLength (x,y) (i,j)] ]
												tilesLineLength :: Pos -> Dir -> Int
												tilesLineLength (x',y') (i',j')
													| i' < 0 	&& j' < 0 	= min x' y'
													| i' < 0 	&& j' == 0 	= x'
													| i' < 0 	&& j' > 0 	= min x' (size-1-y')
													| i' == 0 	&& j' < 0 	= y'
													| i' == 0 	&& j' == 0 	= error "direction cannot be (0,0)"
													| i' == 0 	&& j' > 0 	= size-1-y'
													| i' > 0 	&& j' < 0 	= min (size-1-x') y'
													| i' > 0 	&& j' == 0 	= size-1-x'
													| i' > 0 	&& j' > 0 	= min (size-1-x') (size-1-y')

isEnd										:: Player -> Board -> Bool
isEnd p b 									= nextMoves p b == [] && nextMoves (reversePlayer p) b == []


nextValidBoards 					:: Tile -> Board -> [Board]
nextValidBoards t b 			 	= filterSymmetric [ move t p b | p <- nextMoves t b]
filterSymmetric 					:: [Board] -> [Board]
filterSymmetric [] 					= []
filterSymmetric (b:bs)				= b: filterSymmetric (filter ((== False) . isSymmetric b) bs) 

											