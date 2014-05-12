{-
	define different search techniques for the Reversi game
-}
module Search where

import Board
import Tree
import Evaluation
import Play

import System.Random

----------------------------------------
-- MiniMax
----------------------------------------

data Turn = Min | Max deriving Eq

-- the minimum evaluation value for Reversi is -64
minimumEV = -64
-- the maximum evaluation value for Reversi is 64
maximumEV = 64

reverseTurn :: Turn -> Turn
reverseTurn Min = Max
reverseTurn Max = Min

-- return a heristic best board, if there is no valid move return Nothing
minimax :: Player -> Tree -> Maybe Board 
minimax =  pick minimaxAssign

-- return a heristic best board, if there is no valid move return Nothing
alphaBeta :: Player -> Tree -> Maybe Board 
alphaBeta = pick alphaBetaAssign


{-	pick: first assigns evaluation value to game tree used    
specified assign function, then picks the best next board. If there is no valid 
move, return Nothing.
	Inputs:
	(Turn -> Player -> Tree -> Tree) - assign function to assign evaluation value to game nodes
	Player - current player;
	Tree - game tree before assigning evaluation value
-}
pick :: (Turn -> Player -> Tree -> Tree) -> Player -> Tree -> Maybe Board 
pick assign tile t = case bs of
						[] -> Nothing --"minimax search: bs cannot be empty!"
						_ -> Just (bs !! r) -- now, just pick a random one
	where
		-- (N b ev ts) is assigned game tree after minimaxAssign
		(N b ev ts) = assign Max tile t 
		-- bs is array of next boards with best evaluation value.
		bs = [ b | (N b ev' _) <- ts, ev' == ev ]
--		bs = [ b | (N b ev' _) <- ts, compareEva ev' ev == 0 ]
		-- r is a random index 
		-- g' is the new random generator
		(r,g') = randomR (0,length bs-1) (mkStdGen 1)
		

-- minimaxAssign: assign evaluation value to game nodes using minimax algorithm
minimaxAssign :: Turn -> Player -> Tree -> Tree
minimaxAssign _ tile (N b _ []) = N b (greedy tile b) []
minimaxAssign turn tile (N b _ ts) 
	| turn == Min = N b (minimum evs) ts'
	| turn == Max = N b (maximum evs) ts'
			where
				ts' = [ minimaxAssign (reverseTurn turn) tile t | t <- ts ] 
				evs = [ ev | (N _ ev _) <- ts' ]



-- testing
{-
assignEV :: Turn -> Tile -> Tree Board -> Tree Board
assignEV _ tile (N b _ []) = N b (evaluate greedy tile b) []
assignEV turn tile (N b _ ts) 
			| turn == Min = N b (minimum evs) ts'
			| turn == Max = N b (maximum evs) ts'
					where
						ts' = [ assignEV (reverseTurn turn) tile t | t <- ts ] 
						evs = [ ev | (N _ ev _) <- ts' ]

t = assignEV Max Black (tree Black)

evss = evs t
-- use too much to test
evs :: Tree Board -> [Eva]
evs (N _ ev []) = [ev]
evs (N _ ev ts) = ev:10:concat [ evs t | t <- ts] 
-}

----------------------------------------
-- ∂-ß pruning
----------------------------------------
-- 2 hour
-- assign evaluation value into tree, and find the next board
{-}
alphaBeta :: Tile -> Tree -> Maybe Board 
alphaBeta tile t = case bs of
					[] -> Nothing --error "alphaBeta: bs cannot be empty!"
					_ -> Just (bs !! r) -- now, just pick a random one
	where
		-- (N b eve ts) is the assigned game tree after alphaBetaAssign
		(N b ev ts) = assignEV Max tile t 
		-- bs is an array
		bs = [ b | (N b ev' _) <- ts, ev' == ev ]
--		bs = [ b | (N b ev' _) <- ts, compareEva ev' ev == 0 ]
		(r,g') = randomR (0,length bs-1) (mkStdGen 1)
-}

-- alphaBetaAssign: assign evaluation value to game nodes using alphabeta pruning algorithm
alphaBetaAssign :: Turn -> Player -> Tree -> Tree
alphaBetaAssign _ tile (N b _ []) = N b (greedy tile b) []
alphaBetaAssign turn tile (N b _ ts) 
	| turn == Min = let ts' = betaPruning tile maximumEV ts in N b (minimum [ ev | (N _ ev _) <- ts' ]) ts'
	| turn == Max = let ts' = alphaPruning tile minimumEV ts in N b (maximum [ ev | (N _ ev _) <- ts' ]) ts'
		where 		
			{-	For Max player, the current best board evaluation value is called alpha value. 
			If any child node has a beta value less than or equal to the current alpha value, 
			the search stops below that child node. 
			-}
			alphaPruning :: Player -> EV -> [Tree] -> [Tree]
			alphaPruning tile _ [] = []
			alphaPruning tile alpha ((N b _ []):ts) 
				| alpha >= beta = (N b beta []):(alphaPruning tile alpha ts)
				| otherwise = (N b beta []):(alphaPruning tile beta ts) 
				where
					beta = greedy tile b
			alphaPruning tile alpha (t:ts)  
				| alpha >= beta = (N b beta [t'']):(alphaPruning tile alpha ts)
				| otherwise = (N b beta (t'':betaPruning tile beta ts')):(alphaPruning tile beta ts) 
				where
					(N b _ (t':ts')) = t
					t'' = alphaBetaAssign Max tile t'
					(N _ beta _) = t''
			{-	For Min player, the current best board evaluation value is called beta value. 
			If any child node has an alpha value larger than or equal to the current beta, 
			the search stops below that child node.
			-}
			betaPruning :: Player -> EV -> [Tree] -> [Tree]
			betaPruning tile _ [] = []
			betaPruning tile beta ((N b _ []):ts) 
				| alpha >= beta = (N b alpha []):(betaPruning tile beta ts)
				| otherwise = (N b beta []):(betaPruning tile alpha ts) 
				where
					alpha = greedy tile b
			betaPruning tile beta (t:ts)  
				| alpha >= beta = (N b alpha [t'']):(betaPruning tile beta ts)
				| otherwise = (N b alpha (t'':alphaPruning tile alpha ts')):(betaPruning tile alpha ts)
				where
					(N b _ (t':ts')) = t
					t'' = alphaBetaAssign Max tile t'
					(N _ alpha _) = t''











