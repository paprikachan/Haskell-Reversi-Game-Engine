{-
	define different evaluation functions for the game board
-} 

module Evaluation where


import Board
import Tree



evaluate :: (Tile -> Board -> Eva) -> Tile -> Board -> Eva
evaluate f = f

--Greedy Evaluation: Eve = Value (black counts - white counts)---------------------------- 
greedy :: Tile -> Board -> Eva
greedy Black (B _ _ (x,y)) = EV (x - y)
greedy White (B _ _ (x,y)) = EV (y - x)

------------------------------------------------------------------------------------------ 