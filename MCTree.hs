{-# LANGUAGE RecordWildCards   #-}

module MCTree where

import Play
import Runner
import MCTS

import Control.Monad.State
import Control.Monad.State.Lazy
import System.Random
import Data.IORef

data Env = Env{
	iniPlayer :: Player,
	player :: Player,
	pathBoards :: [Board],
	gen :: StdGen
}


b1 =  monteCarloMove Black initialBoard
b2 =  monteCarloMove White b1


monteCarloMove :: Player -> Board -> Board
monteCarloMove p b = getBoard $ bestTree ts
	where
		(N _ _ ts) = finalTree
		bestTree :: [MCT] -> MCT
		bestTree [] = error "cannot constructed mcTree"
		bestTree ts = foldr maxTree (N (0,0) b []) ts
		gs = map mkStdGen [1..repeats]
		(r,g) = randomR (0,length gs - 1) (mkStdGen 1)
		finalTree = evalState (monteCarlo repeats (N (0,0) b [])) (Env p p [] (gs !! r))

-- Process method
---------------------------------------
monteCarlo :: Int -> MCT -> State Env MCT
monteCarlo 1 t = processMCT t
monteCarlo n t = do
	processMCT t
	monteCarlo (n-1) t


processMCT :: MCT -> State Env MCT
processMCT t = do
	Env {..} <- get
	setPlayer iniPlayer -- reset player 
	t' <- selectExpand t
	Env {..} <- get
	b <- return (last pathBoards)
	Env {..} <- get
	i <- simulate player b
	backPropogate i pathBoards t'

selectExpand :: MCT -> State Env MCT 
selectExpand t@(N i b []) = do 
	Env{..} <- get
	appendPathBoards b
	setPlayer (reversePlayer player) -- change player
	Env {..} <- get
	-- expand tree
	subs <- return $ nextValidSubs player b
	-- pick random board
	(r,gen') <- return $ randomR (0,(length subs) - 1) gen
	setGen gen'
	appendPathBoards (getBoard $ subs !! r)
	setPlayer (reversePlayer player) -- change player
	return (N i b subs)
selectExpand t@(N i b ts) = do 
	Env{..} <- get -- player, tree -> pathBoards
	appendPathBoards b
	setPlayer (reversePlayer player) -- change player
	-- pick sub tree to do recursion
	sub <- return $ foldr (maxUCT i) (head ts) (tail ts)
	(lsubs,_,rsubs) <- return $ splitSubs (getBoard sub) ts
	sub' <- selectExpand sub
	return (N i b (lsubs++[sub']++rsubs))
				
simulate :: Player -> Board -> State Env Info
simulate p b = if isEnd p b then do
	Env {..} <- get
	return $ returnInfo iniPlayer b
	else do
	Env {..} <- get
	moves <- return $ nextMoves player b 
	(r, gen') <- return $ randomR (0, length moves-1) gen
	setGen gen'
	simulate (reversePlayer player) (move player (moves !! r) b)

backPropogate :: Info -> [Board] -> MCT -> State Env MCT
backPropogate i bs t = return $ backPropogation i bs t

{-}
backPropogate :: Info -> [Board] -> MCT -> State Env MCT
backPropogate (i,j) [pb] (N (x,y) b []) = if b == pb then do
	return (N (x+i,y+j) b []) 
	else do
	error "Wrong PathBoards information1"
backPropogate (i,j) (pb1:pb2:pbs) (N (x,y) b ts) = if pb1 == b then do
	(lsubs,sub,rsubs) <- return $ splitSubs pb2 ts
	sub' <- backPropogate (i,j) (pb2:pbs) sub
	return (N (x+i,y+j) b (lsubs++ [sub'] ++rsubs)) 
	else do
	error "Wrong PathBoards information2"
-}	
backPropogation :: Info -> TBoards -> MCT -> MCT -- the length of two array are same
backPropogation (i,j) [tb] (N (x,y) b ts) 
	| tb == b = N (x+i,y+j) b ts
	| otherwise = error "Wrong TBoards information1"
backPropogation (i,j) (tb1:tb2:tbs) (N (x,y) b ts) 
	| tb1 == b = (N (x+i,y+j) b (ts1++[backPropogation (i,j) (tb2:tbs) t']++ts2))
	| otherwise = error "Wrong TBoards information2"
		where 
			(ts1, t', ts2) = splitSubs tb2 ts		
-------------------------------------------------------------------------------
nextValidSubs :: Player -> Board -> [MCT]
nextValidSubs p b = [(N (0,0) b' []) | b' <- nextValidBoards p b]

splitSubs :: Board -> [MCT] -> ([MCT],MCT,[MCT])
splitSubs b ts 
	| elem b (map (getBoard) ts) = (takeWhile ((/= b).getBoard) ts, head ts', tail ts')
	| otherwise = error "Board is not in PathBoards."
		where
			ts' = dropWhile ((/= b).getBoard) ts 

maxUCT :: Info -> MCT -> MCT -> MCT 
maxUCT (_,v) t1@(N (w1,v1) _ _) t2@(N (w2,v2) _ _) = if (w1/v1 + sqrt(2*(log v)/v1)) >= (w2/v2 + sqrt(2*(log v)/v2)) then t1 else t2		

-- Player is initial player
returnInfo :: Player -> Board -> Info
returnInfo Black (B _ _ (x,y))
	| x > y = (1,1)
	| otherwise = (0,1)
returnInfo White (B _ _ (x,y))
	| x < y = (1,1)
	| otherwise = (0,1)

--Setter Methods
--------------------------------------

setPlayer :: Player -> State Env ()
setPlayer p = do 	
	(Env ip _ pBs g) <- get
	put (Env ip p pBs g)

appendPathBoards :: Board -> State Env ()
appendPathBoards b = do
	(Env ip p pBs g) <- get
	put (Env ip p (pBs++[b]) g)

setGen :: StdGen -> State Env ()
setGen g = do
	(Env ip p pBs _) <- get
	put (Env ip p pBs g) 



data EnvT = EnvT{
	--playerT :: Player,
	currentPlayerT :: IORef Player
	--boardsT :: IORef [Board],
	--genT :: IORef StdGen,
	--treeT :: IORef MCT
}

changePlayerT :: Player -> StateT EnvT IO Player
changePlayerT p = do	
	e <- get
	liftIO $ atomicModifyIORef (currentPlayerT e) $ \_ -> (p, p)

mainT = do
	p <- newIORef Black
	env <- execStateT (changePlayerT White) (EnvT p)
	readIORef $ currentPlayerT env


p = evalState test (Env Black White [] (mkStdGen 1))

test :: State Env Player
test = do
	Env {..} <- get
	setPlayer Black
	Env {..} <- get
	return player











