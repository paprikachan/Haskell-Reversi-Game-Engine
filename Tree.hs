-- Game Tree for Reversi Game
module Tree where

import Board
import Play
import Data.Array

---------------------------------------
-- Game Tree Data
---------------------------------------
data Eva = MINIMUM | EV Int | MAXIMUM | EMPTY deriving (Ord,Eq) -- the order thing

compareEva :: Eva -> Eva -> Int
compareEva _ EMPTY = error "the snd eva is EMPTY"
compareEva EMPTY _ = error "the fst eva is EMPTY"
compareEva MAXIMUM _ = 1
compareEva _ MAXIMUM = -1
compareEva MINIMUM _ = -1
compareEva _ MINIMUM = 1
compareEva (EV x) (EV y)
	| x > y = 1
	| x == y = 0
	| x < y = -1

maxEva :: [Eva] -> Eva
maxEva [] = error "Empty eva list in maxEva"
maxEva (ev:evs) = case compareEva ev (maxEva evs) of 
					1 -> ev
					0 -> ev
					-1 -> (maxEva evs)

minEva :: [Eva] -> Eva
minEva [] = error "Empty eva list in minEva"
minEva (ev:evs) = case compareEva ev (minEva evs) of
					1 -> minEva evs
					0 -> ev
					-1 -> ev
-- Int: is the evaluation value for the baord on that node
data Tree a b = N a b [Tree a b]

type MMT = Tree Board Eva

type Depth = Int


-- store valid next boards in the tree, remove the symmetric boards

constructTree 								:: Tile -> Board -> Depth -> MMT
constructTree t b 0							= N b (EV 0) []
constructTree t b d							= N b (EV 0) [ constructTree (reversePlayer t) b' (d-1) | b' <- nextValidBoards t b ]
	where 
		nextValidBoards 					:: Tile -> Board -> [Board]
		nextValidBoards t b 			 	= filterSymmetric [ move t p b | p <- nextMoves t b]
		filterSymmetric 					:: [Board] -> [Board]
		filterSymmetric [] 					= []
		filterSymmetric (b:bs)				= b: filterSymmetric (filter ((== False) . isSymmetric b) bs) 
-- testing



































