{-
	define different evaluation functions for the game board
-} 

module Evaluation where


import Board
import Tree

--Greedy Evaluation: Eve = Value (black counts - white counts)---------------------------- 
-- greedy: generates a heuristic evaluation value using greedy algorithm for a given player and board.
greedy :: Tile -> Board -> EV
greedy Black (B _ _ (x,y) _) = (x - y)
greedy White (B _ _ (x,y) _) = (y - x)

------------------------------------------------------------------------------------------ 