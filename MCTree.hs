{-# LANGUAGE RecordWildCards   #-}

module MCTree where

import Play
import Board
import Data.Array((!))
import Control.Monad.State
import Control.Monad.State.Lazy
import System.Random
import Data.IORef

data Env = Env{
	iPlayer :: Player,
	player :: IORef Player,
	selectedBoards :: IORef [Board],
	gen :: IORef StdGen
}

data MCTree = N Info Board [MCTree]

type Info = (Wins, Visits)

type Wins = Float

type Visits = Float



----- MonteCarlo API -----------------------------

-- mcSimulationBoard2Board: this function provides the interface to play
-- random simulation on a monte carlo tree.
-- Inputs: simulation time, current player, current board.
-- Return: the most promising next valid board in IO monad.
mcSimulationBoard2Board :: Int -> Player -> Board -> IO Board
mcSimulationBoard2Board simulates p b = do
	-- apply to monteCarloSimulation4Tree
	t' <- mcSimulationBoard2Tree simulates p b
	-- return the root node board of the best subtree 
	return $ getBoard t'

-- mcSimulationBoard2Tree: this function provides the interface to play
-- random simulation on a monte carlo tree.
-- Inputs: simulation time, current player, current board.
-- Return: an updated montecarlo tree generated based on current board in IO monad.
mcSimulationBoard2Tree :: Int -> Player -> Board -> IO MCTree
mcSimulationBoard2Tree simulates p b = do
	-- construct montecarlo tree
	t <- return $ N (0,0) b []
	-- apply to monteCarloSimulation4Tree
	t' <- mcSimulationTree2Tree simulates p t
	-- return the best subtree 
	return t'

-- mcSimulationTree2Board: this function provides the interface to play
-- random simulation on a monte carlo tree.
-- Inputs: simulation time, current player, current montecarlo tree.
-- Return: the most promising next valid board in IO monad.
mcSimulationTree2Board :: Int -> Player -> MCTree -> IO Board
mcSimulationTree2Board simulates p t = do
	-- apply to mcSimulationTree2Tree
	t' <- mcSimulationTree2Tree simulates p t
	-- return the root node board of the best subtree 
	return $ getBoard t'

-- mcSimulationTree2Tree: this function provides the interface to play
-- random simulation on a monte carlo tree.
-- Inputs: simulation time, current player, current montecarlo tree.
-- Return: an updated montecarlo tree generated based on current board in IO monad.
mcSimulationTree2Tree :: Int -> Player -> MCTree -> IO MCTree
mcSimulationTree2Tree simulates p t = do
	-- set monad enviroment
	ip <- newIORef p
	ibs <- newIORef []
	igen <- newIORef (mkStdGen simulates)
	-- exucute monte carlo simulation
	(N _ _ ts) <- evalStateT (monteCarlo simulates t) (Env p ip ibs igen)
	-- return the best subtrees of root node which has the largest wining posibility.
	return $ bestTree ts


-- Process method
---------------------------------------

-- monteCarlo: do montecarlo simulation specified times in monad environment.
monteCarlo :: Int -> MCTree -> StateT Env IO MCTree
monteCarlo 1 t = processMCTree t
monteCarlo n t = do
	processMCTree t
	monteCarlo (n-1) t

-- processMCTree: do montecarlo simulation one time in monad environment.
processMCTree :: MCTree -> StateT Env IO MCTree
processMCTree t = do
	Env {..} <- get
	iniPlayer -- reset player 
	iniSelectedBoards -- reset selectedBoards
	t' <- selectExpand t
	pBs <- liftIO $ readIORef selectedBoards
	b <- return (last pBs)
	p <- liftIO $ readIORef player
	i <- simulate p b
	backPropogate i pBs t'

-- selectExpand: do selection and expansion in monad environment.
selectExpand :: MCTree -> StateT Env IO MCTree 
-- if reach leaf node
selectExpand t@(N i b []) = do 
	Env{..} <- get
	appendSelectedBoards b
	-- expand tree
	p <- liftIO $ readIORef player
	subs <- return $ nextValidSubs p b
	-- pick random board
	g <- liftIO $ readIORef gen
	(r,g') <- return $ randomR (0,(length subs) - 1) g
	setGen g'
	appendSelectedBoards (getBoard $ subs !! r)
	return (N i b subs)
selectExpand t@(N i b ts) = do 
	Env{..} <- get -- player, tree -> selectedBoards
	appendSelectedBoards b
	revPlayer -- change player
	-- pick sub tree to do recursion
	sub <- return $ foldr (maxUCTTree i) (head ts) (tail ts)
	--liftIO $ print "I am here1"
	(lsubs,_,rsubs) <- return $ splitSubs (getBoard sub) ts
	--liftIO $ print "I am here2"
	sub' <- selectExpand sub
	return (N i b (lsubs++[sub']++rsubs))
			
-- simulate: do simulation in monad environment.	
simulate :: Player -> Board -> StateT Env IO Info
simulate p b = if isEnd p b || length (nextValidBoards p b) == 0 then do
	Env {..} <- get
	return $ returnInfo iPlayer b
	else do
	Env {..} <- get
	--p <- liftIO $ readIORef player
	vBoards <- return $ nextValidBoards p b
	g <- liftIO $ readIORef gen
	(r, g') <- return $ randomR (0, length vBoards-1) g
	setGen g'
	if length vBoards == 0 then return $ returnInfo iPlayer b
	else simulate (reversePlayer p) (vBoards !! r)


-- backPropogate: do back propogation in monad environment.
backPropogate :: Info -> [Board] -> MCTree -> StateT Env IO MCTree
backPropogate (i,j) [sb] (N (x,y) b []) = if b == sb then do
	return (N (x+i,y+j) b []) 
	else do
	error "Wrong selectedBoards information1"
backPropogate (i,j) (sb1:sb2:sbs) (N (x,y) b ts) = if sb1 == b then do
	(lsubs,sub,rsubs) <- return $ splitSubs sb2 ts
	sub' <- backPropogate (i,j) (sb2:sbs) sub
	return (N (x+i,y+j) b (lsubs++ [sub'] ++rsubs)) 
	else do
	error "Wrong selectedBoards information2"


-- Tree Manipulation --


-- getBoard: get root node board for a tree
getBoard :: MCTree -> Board
getBoard (N _ b _) = b

-- Player is initial player
returnInfo :: Player -> Board -> Info
returnInfo Black (B _ _ (x,y) _)
	| x > y = (1,1)
	| otherwise = (0,1)
returnInfo White (B _ _ (x,y) _) 
	| x < y = (1,1)
	| otherwise = (0,1)

iniMCTree = N (0,0) initialBoard []


-- subMCTree: 
subMCTree :: Board -> MCTree -> Maybe MCTree
subMCTree b1 tree@(N _ b2 []) 
	| b1 == b2 = Just tree
	| otherwise = Nothing
subMCTree b1 tree@(N i b2 ts)
	| b1 == b2 = Just tree
	| otherwise = case filter notNothing (map (subMCTree b1) ts) of
		[maybeTree] -> maybeTree
		_ -> Nothing
	where 
		notNothing :: Maybe MCTree -> Bool
		notNothing Nothing = False
		notNothing _ = True

-------------------------------------------------------------------------------

-- bestTree: pick the best trees from the array,
-- the best tree is the tree who has the largest winning possibility.
bestTree :: [MCTree] -> MCTree
bestTree [] = error "cannot constructed mcTree"
bestTree ts = foldr maxTree (N (0,0) initialBoard []) ts

-- maxTree: given two trees, return the tree which has the larger winning possibility.
maxTree :: MCTree -> MCTree -> MCTree
maxTree t1@(N (w1,_) _ _) t2@(N (w2,_) _ _) = if w1 >= w2 then t1 else t2 -- won't be initialBoard sicuation

-- maxUCTTree: Info from parent node, two child node, return the child node which has max UCT value.
maxUCTTree :: Info -> MCTree -> MCTree -> MCTree 
maxUCTTree (_,v) t1@(N (w1,v1) _ _) t2@(N (w2,v2) _ _) = if (w1/v1 + sqrt(2*(log v)/v1)) >= (w2/v2 + sqrt(2*(log v)/v2)) then t1 else t2		


-- nextValidSubs; return all next valid sub tree
nextValidSubs :: Player -> Board -> [MCTree]
nextValidSubs p b = [(N (0,0) b' []) | b' <- nextValidBoards p b]

splitSubs :: Board -> [MCTree] -> ([MCTree],MCTree,[MCTree])
splitSubs b ts 
	| elem b (map (getBoard) ts) = (takeWhile ((/= b).getBoard) ts, head ts', tail ts')
	| otherwise = error "Board in PathBoards Not Match Board in Tree"
		where
			ts' = dropWhile ((/= b).getBoard) ts 





--Setter Methods
--------------------------------------

-- revPlayer: change current player in monad environment.
revPlayer :: StateT Env IO Player
revPlayer = do 	
	Env {..} <- get
	liftIO $ atomicModifyIORef player $ \p -> (reversePlayer p,reversePlayer p)

-- reset current player to initial player
iniPlayer :: StateT Env IO Player
iniPlayer = do 	
	Env {..} <- get
	liftIO $ atomicModifyIORef player $ \_ -> (iPlayer, iPlayer)

-- reset selected boards to empty
iniSelectedBoards :: StateT Env IO [Board]
iniSelectedBoards = do
	Env {..} <- get
	liftIO $ atomicModifyIORef selectedBoards $ \_ -> ([], [])

-- appendSelectedBoards: append a board into selected boards
appendSelectedBoards :: Board -> StateT Env IO [Board]
appendSelectedBoards b = do
	Env {..} <- get
	pbs <- liftIO $ readIORef selectedBoards
	liftIO $ atomicModifyIORef selectedBoards $ \_ -> (pbs++[b], pbs++[b])

-- setGen: the random number generator in monad environment.
setGen :: StdGen -> StateT Env IO StdGen
setGen g = do
	Env {..} <- get
	liftIO $ atomicModifyIORef gen $ \_ -> (g,g)








