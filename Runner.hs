module Runner where

import Board
import Play
import Tree
import Search
import Parser
import MCTS


import Control.Monad.State

data AI = GreedyMinimax | GreedyAlphaBeta | PositionalMinimax | PositionalAlphaBeta | MonteCarlo


initialGame = Game Black (-1,-1) initialBoard iniMCTree

---------------------------------------------------
playAIGame :: Int -> AI -> Game -> Maybe Game
playAIGame depth ai (Game p m b t) = do
	b' <- computerMove depth ai p b
	return (Game (reversePlayer p) m b' t) 


computerMove :: Int -> AI -> Tile -> Board -> Maybe Board
computerMove depth GreedyMinimax t b = minimax t (constructTree t b depth)
computerMove depth GreedyAlphaBeta t b = alphaBeta t (constructTree t b depth)
computerMove depth PositionalMinimax t b = Just b
computerMove depth PositionalAlphaBeta t b = Just b
--computerMove MonteCarlo t b = monteCarloMove t b

----------------------------------------------------
-- isValidMove	:: Tile -> Pos -> Board -> Bool
validMoveGame :: Game -> Maybe Game
validMoveGame (Game p m b t) = case isValidMove p m b of
								True -> Just (Game (reversePlayer p) m (Play.move p m b) t)
								False -> Nothing 


isEndGame :: Game -> Bool
isEndGame (Game p m b _) = isEnd p b


setTree :: MCT -> Game -> Game
setTree t (Game p m b _) = Game p m b t

iniGameTree :: Game -> Game
iniGameTree (Game p m b _) = Game p m b (MCTS.N (0,0) b [])