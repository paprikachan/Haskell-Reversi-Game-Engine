-- Game Tree for Reversi Game
module Tree where

import Board
import Play
import Data.Array

---------------------------------------
-- Game Tree Data
---------------------------------------
--data EV = MINIMUM | EV Int | MAXIMUM | EMPTY deriving (Ord,Eq) -- the order thing
type EV = Int 

{- 	compareEV: compare two evaluation values, 
	return 1 if the first one is larger,
	return 0 if equal,
	return -1 if the first one is smaller.
	
compareEV :: EV -> EV -> Int
compareEV _ EMPTY = error "the snd evaluation value is EMPTY"
compareEV EMPTY _ = error "the fst evaluation value is EMPTY"
compareEV MAXIMUM _ = 1
compareEV _ MAXIMUM = -1
compareEV MINIMUM _ = -1
compareEV _ MINIMUM = 1
compareEV (EV x) (EV y)
	| x > y = 1
	| x == y = 0
	| x < y = -1

-- maxEV: return the maximum value from a list of evaluation values.
maxEV :: [EV] -> EV
maxEV [] = error "Empty eva list in maxEV"
maxEV (ev:evs) = case compareEV ev (maxEV evs) of 
					1 -> ev
					0 -> ev
					-1 -> (maxEV evs)
-- minEV: return the minimum value from a list of evaluation values.
minEV :: [EV] -> EV
minEV [] = error "Empty eva list in minEV"
minEV (ev:evs) = case compareEV ev (minEV evs) of
					1 -> minEV evs
					0 -> ev
					-1 -> ev
-}
-- Int: is the evaluation value for the baord on that node
data T b v = N b v [T b v]
type Tree = T Board EV 
type Depth = Int


-- store valid next boards in the tree, remove the symmetric boards
{-	constructTree: construct a game tree with specified depth
	Inputs: current player, current board, look-ahead plies
	Return: constructed game tree with initilised evaluation value 0 for each node.
-}
constructTree 								:: Tile -> Board -> Depth -> Tree
constructTree t b 0						= N b 0 []
constructTree t b d						= N b 0 [ constructTree (reversePlayer t) b (d-1) | b <- nextValidBoards t b ]
	




































