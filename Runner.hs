module Runner where

import Board
import Play
import Tree
import Search
import MCTree


data Game = 
	Game{	id      :: Int
        ,   player 	:: Player 
		, 	move 	:: (Int, Int)
		,	board 	:: Board
        ,   mcTree  :: MCTree
        ,   end     :: Bool
		} --deriving Show

-- setter function --
setTree :: MCTree -> Game -> Game
setTree t (Game id p m b _ _) = Game id p m b t True

iniGameTree :: Game -> Game
iniGameTree (Game id p m b _ _) = Game id p m b (MCTree.N (0,0) b []) True

-- Initialisation
initialGame :: Int -> Game
initialGame id = Game id Black (-1,-1) initialBoard iniMCTree True

blankGame :: Game
blankGame = initialGame (-1)

isEndGame :: Game -> Bool
isEndGame (Game _ p m b _ _) = isEnd p b

validMovesGame :: Game -> Maybe Game
validMovesGame (Game id p m b t _) = case isValidMove p m b of
								True -> Just (Game id (reversePlayer p) m (Play.move p m b) t True)
								False -> Nothing 



---------------------------------------------------
-- run AI in game --

data AI = GreedyMinimax | GreedyAlphaBeta | MonteCarlo

playAIGame :: Int -> AI -> Game -> Maybe Game
playAIGame depth ai (Game id p m b t _) = do
	b' <- computerMove depth ai p b
	return (Game id (reversePlayer p) m b' t True) 


computerMove :: Int -> AI -> Player -> Board -> Maybe Board
computerMove depth GreedyMinimax p b = minimax p (constructTree p b depth)
computerMove depth GreedyAlphaBeta p b = alphaBeta p (constructTree p b depth)






