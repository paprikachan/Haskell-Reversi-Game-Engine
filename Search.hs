{-
	define different search techniques for the Reversi game
-}
module Search where

import Board
import Tree
import Evaluation

import System.Random

----------------------------------------
-- MiniMax
----------------------------------------

data Turn = Min | Max deriving Eq

reverseTurn :: Turn -> Turn
reverseTurn Min = Max
reverseTurn Max = Min

-- assign evaluation value into tree, and find the next board
minimax :: Tile -> MMT -> Maybe Board 
minimax tile t = case bs of
					[] -> Nothing --"minimax search: bs cannot be empty!"
					_ -> Just (bs !! r) -- now, just pick a random one
	where
		(r,g') = randomR (0,length bs-1) (mkStdGen 1)
		bs = [ b | (N b ev' _) <- ts, compareEva ev' ev == 0 ]
		(N b ev ts) = assignEV Max tile t 

		assignEV :: Turn -> Tile -> MMT -> MMT
		assignEV _ tile (N b _ []) = N b (evaluate greedy tile b) []
		assignEV turn tile (N b _ ts) 
			| turn == Min = N b (minimum evs) ts'
			| turn == Max = N b (maximum evs) ts'
					where
						ts' = [ assignEV (reverseTurn turn) tile t | t <- ts ] 
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
alphaBeta :: Tile -> MMT -> Maybe Board 
alphaBeta tile t = case bs of
					[] -> Nothing --error "alphaBeta: bs cannot be empty!"
					_ -> Just (bs !! r) -- now, just pick a random one
	where
		(r,g') = randomR (0,length bs-1) (mkStdGen 1)
		bs = [ b | (N b ev' _) <- ts, compareEva ev' ev == 0 ]
		(N b ev ts) = assignEV Max tile t 

		assignEV :: Turn -> Tile -> MMT -> MMT
		assignEV _ tile (N b _ []) = N b (evaluate greedy tile b) []
		assignEV turn tile (N b _ ts) 
			| turn == Min = let ts' = betaPruning tile MAXIMUM ts in N b (minimum [ ev | (N _ ev _) <- ts' ]) ts'
			| turn == Max = let ts' = alphaPruning tile MINIMUM ts in N b (maximum [ ev | (N _ ev _) <- ts' ]) ts'
			where 				
		alphaPruning :: Tile -> Eva -> [MMT] -> [MMT]
		alphaPruning tile _ [] = []
		alphaPruning tile alpha ((N b _ []):ts) 
			| compareEva alpha beta >= 0 = (N b beta []):(alphaPruning tile alpha ts)
			| otherwise = (N b beta []):(alphaPruning tile beta ts) 
			where
				beta = evaluate greedy tile b
		alphaPruning tile alpha (t:ts)  
			| compareEva alpha beta >= 0 = (N b beta [t'']):(alphaPruning tile alpha ts)
			| otherwise = (N b beta (t'':betaPruning tile beta ts')):(alphaPruning tile beta ts) -- ？
			where
				(N b _ (t':ts')) = t
				t'' = assignEV Max tile t'
				(N _ beta _) = t''
		betaPruning :: Tile -> Eva -> [MMT] -> [MMT]
		betaPruning tile _ [] = []
		betaPruning tile beta ((N b _ []):ts) 
			| compareEva alpha beta >= 0 = (N b alpha []):(betaPruning tile beta ts)
			| otherwise = (N b beta []):(betaPruning tile alpha ts) 
			where
				alpha = evaluate greedy tile b
		betaPruning tile beta (t:ts)  
			| compareEva alpha beta >= 0 = (N b alpha [t'']):(betaPruning tile beta ts)
			| otherwise = (N b alpha (t'':alphaPruning tile alpha ts')):(betaPruning tile alpha ts)
			where
				(N b _ (t':ts')) = t
				t'' = assignEV Max tile t'
				(N _ alpha _) = t''











