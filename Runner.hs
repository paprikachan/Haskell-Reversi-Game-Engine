module Runner where

import Board
import Play
import Tree
import Search
import Parser
import MCTS


import Control.Monad.State

data AI = GreedyMinimax | GreedyAlphaBeta | PositionalMinimax | PositionalAlphaBeta | MonteCarlo

initialGame :: Int -> Game
initialGame id = Game id Black (-1,-1) initialBoard iniMCTree True

---------------------------------------------------
playAIGame :: Int -> AI -> Game -> Maybe Game
playAIGame depth ai (Game id p m b t _) = do
	b' <- computerMove depth ai p b
	return (Game id (reversePlayer p) m b' t True) 


computerMove :: Int -> AI -> Tile -> Board -> Maybe Board
computerMove depth GreedyMinimax t b = minimax t (constructTree t b depth)
computerMove depth GreedyAlphaBeta t b = alphaBeta t (constructTree t b depth)
computerMove depth PositionalMinimax t b = minimax t (constructTree t b depth)
computerMove depth PositionalAlphaBeta t b = alphaBeta t (constructTree t b depth)
--computerMove MonteCarlo t b = monteCarloMove t b

----------------------------------------------------
-- isValidMove	:: Tile -> Pos -> Board -> Bool
validMoveGame :: Game -> Maybe Game
validMoveGame (Game id p m b t _) = case isValidMove p m b of
								True -> Just (Game id (reversePlayer p) m (Play.move p m b) t True)
								False -> Nothing 


isEndGame :: Game -> Bool
isEndGame (Game _ p m b _ _) = isEnd p b


setTree :: MCT -> Game -> Game
setTree t (Game id p m b _ _) = Game id p m b t True

iniGameTree :: Game -> Game
iniGameTree (Game id p m b _ _) = Game id p m b (MCTS.N (0,0) b []) True