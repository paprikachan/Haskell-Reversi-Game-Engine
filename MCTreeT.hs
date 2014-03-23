{-# LANGUAGE RecordWildCards   #-}

module MCTreeT where

import Play
import Board
import MCTS
import Data.Array((!))
import Control.Monad.State
import Control.Monad.State.Lazy
import System.Random
import Data.IORef

data Env = Env{
	iPlayer :: Player,
	player :: IORef Player,
	pathBoards :: IORef [Board],
	gen :: IORef StdGen
}



subMCTree :: Board -> MCT -> Maybe MCT
subMCTree b1 tree@(N _ b2 []) 
	| b1 == b2 = Just tree
	| otherwise = Nothing
subMCTree b1 tree@(N i b2 ts)
	| b1 == b2 = Just tree
	| otherwise = case filter notNothing (map (subMCTree b1) ts) of
		[maybeTree] -> maybeTree
		_ -> Nothing
	where 
		notNothing :: Maybe MCT -> Bool
		notNothing Nothing = False
		notNothing _ = True


monteCarloMove :: Int -> Player -> Board -> MCT -> IO MCT
monteCarloMove simulate p b t = do
	ip <- newIORef p
	ibs <- newIORef []
	igen <- newIORef (mkStdGen simulate)
	(N _ _ ts) <- evalStateT (monteCarlo simulate t) (Env p ip ibs igen)
	return $ bestTree ts

-- Process method
---------------------------------------
monteCarlo :: Int -> MCT -> StateT Env IO MCT
monteCarlo 1 t = processMCT t
monteCarlo n t = do
	processMCT t
	monteCarlo (n-1) t


processMCT :: MCT -> StateT Env IO MCT
processMCT t = do
	Env {..} <- get
	iniPlayer -- reset player 
	iniPathBoards -- reset pathBoards
	t' <- selectExpand t
	pBs <- liftIO $ readIORef pathBoards
	b <- return (last pBs)
	p <- liftIO $ readIORef player
	i <- simulate p b
	backPropogate i pBs t'

selectExpand :: MCT -> StateT Env IO MCT 
selectExpand t@(N i b []) = do 
	Env{..} <- get
	appendPathBoards b
	--revPlayer -- change player
	-- expand tree
	p <- liftIO $ readIORef player
	subs <- return $ nextValidSubs p b
	-- pick random board
	g <- liftIO $ readIORef gen
	(r,g') <- return $ randomR (0,(length subs) - 1) g
	setGen g'
	appendPathBoards (getBoard $ subs !! r)
	--revPlayer -- change player
	return (N i b subs)
selectExpand t@(N i b ts) = do 
	Env{..} <- get -- player, tree -> pathBoards
	appendPathBoards b
	revPlayer -- change player
	-- pick sub tree to do recursion
	sub <- return $ foldr (maxUCT i) (head ts) (tail ts)
	liftIO $ print "I am here1"
	(lsubs,_,rsubs) <- return $ splitSubs (getBoard sub) ts
	liftIO $ print "I am here2"
	sub' <- selectExpand sub
	return (N i b (lsubs++[sub']++rsubs))
				
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
	--liftIO $ print "Boards"
	--liftIO $ print $ length vBoards
	--liftIO $ print $ r
	if length vBoards == 0 then return $ returnInfo iPlayer b
	else simulate (reversePlayer p) (vBoards !! r)




backPropogate :: Info -> [Board] -> MCT -> StateT Env IO MCT
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

-------------------------------------------------------------------------------
bestTree :: [MCT] -> MCT
bestTree [] = error "cannot constructed mcTree"
bestTree ts = foldr maxTree (N (0,0) initialBoard []) ts

nextValidSubs :: Player -> Board -> [MCT]
nextValidSubs p b = [(N (0,0) b' []) | b' <- nextValidBoards p b]

splitSubs :: Board -> [MCT] -> ([MCT],MCT,[MCT])
splitSubs b ts 
	| elem b (map (getBoard) ts) = (takeWhile ((/= b).getBoard) ts, head ts', tail ts')
	| otherwise = error "Board in PathBoards Not Match Board in Tree"
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

revPlayer :: StateT Env IO Player
revPlayer = do 	
	Env {..} <- get
	liftIO $ atomicModifyIORef player $ \p -> (reversePlayer p,reversePlayer p)

iniPlayer :: StateT Env IO Player
iniPlayer = do 	
	Env {..} <- get
	liftIO $ atomicModifyIORef player $ \_ -> (iPlayer, iPlayer)

iniPathBoards :: StateT Env IO [Board]
iniPathBoards = do
	Env {..} <- get
	liftIO $ atomicModifyIORef pathBoards $ \_ -> ([], [])

appendPathBoards :: Board -> StateT Env IO [Board]
appendPathBoards b = do
	Env {..} <- get
	pbs <- liftIO $ readIORef pathBoards
	liftIO $ atomicModifyIORef pathBoards $ \_ -> (pbs++[b], pbs++[b])

setGen :: StdGen -> StateT Env IO StdGen
setGen g = do
	Env {..} <- get
	liftIO $ atomicModifyIORef gen $ \_ -> (g,g)








