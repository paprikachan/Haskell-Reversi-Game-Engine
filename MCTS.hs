
module MCTS where

import System.Random
import Data.Array
import Board
import Play
-- pure monte carlo game search

type Info = (Winning, All)

type Winning = Float

type All = Float

type TBoards = [Board]

data MCT = N Info Board [MCT]
type SubMCT = MCT

iniMCTree = N (0,0) initialBoard []

{-}
b1 = mcMove Black initialBoard
b2 = mcMove White b1
b3 = mcMove Black b2
b4 = mcMove White b3
b5 = mcMove Black b4
b6 = mcMove White b5
b7 = mcMove Black b6
b8 = mcMove White b7
b9 = mcMove Black b8
-}
mcMove :: Player -> Board -> Board
mcMove p b = getBoard $ bestTree ts
	where
		(N _ _ ts) = constructMCT p b 
		bestTree :: [MCT] -> MCT
		bestTree [] = error "cannot constructed mcTree"
		bestTree ts = foldr maxTree (N (0,0) b []) ts

maxTree :: MCT -> MCT -> MCT
maxTree t1@(N (w1,_) _ _) t2@(N (w2,_) _ _) = if w1 >= w2 then t1 else t2 -- won't be initialBoard sicuation

getBoard :: MCT -> Board
getBoard (N _ b _) = b

-- repeat times
repeats :: Int
repeats = 2

constructMCT :: Player -> Board -> MCT
constructMCT p b = repeatMCT (repeats,repeatsGens) p (N (0,0) b [])
	where
		repeatsGens = map mkStdGen [1..repeats]
		-- repeat the process for n times
		repeatMCT :: (Int,[StdGen]) -> Player -> MCT -> MCT
		repeatMCT (0,[]) _ t = t
		repeatMCT (n,g:gs) p t = repeatMCT (n-1,gs) p (processMCT g p t)

		-- selection, expansion, simulation and backpropogation process
		processMCT :: StdGen -> Player -> MCT -> MCT
		processMCT g p (N i b []) = tree
			where
				expansionedSubTrees = expand p b
				(index1,g') = randomR (0,(length expansionedSubTrees) - 2) g
				pickedBoard = getBoard ((!!) expansionedSubTrees index1)
				simulatedInfo = simulate g' (reversePlayer p) (reversePlayer p) pickedBoard
				-- back propogation
				tree = backPropogation simulatedInfo [b,pickedBoard] (N i b expansionedSubTrees)
		processMCT g p tree@(N (w,a) b ts) = tree'
			where
				(currentp, tboards) = select (p,tree)
				expansionedSubTrees = expand currentp (last tboards)
				(index2,g') = randomR (0,(length expansionedSubTrees) - 2) g
				pickedBoard = getBoard ((!!) expansionedSubTrees index2)
				simulatedInfo = simulate g' (reversePlayer currentp) (reversePlayer currentp) pickedBoard
				-- back propogation
				tree' = backPropogation simulatedInfo (tboards++[pickedBoard]) (expandedTree tboards tree expansionedSubTrees)
		-- select
		select :: (Player,MCT) -> (Player,TBoards)
		select (p,N _ b []) = error "Function select error: never select a leaf node"
		select (p,t) = (p',tboards)
			where
				(p',_,tboards) = selectLoop (p,t,[])

				selectLoop :: (Player,MCT,TBoards) -> (Player,MCT,TBoards)
				selectLoop (p, N i b [], tbs) = (p, N i b [], tbs++[b])
				selectLoop (p, N i b ts, tbs) = selectLoop (reversePlayer p,selectedSubTree,tbs++[b])
					where
					--
					selectedSubTree :: MCT--selectedSubTree = maxPair [(uctValue (i',b') (i,b),(N i' b' ts'))| (N i' b' ts') <- ts]
					selectedSubTree = foldr maxUCT (head ts) (tail ts)

					maxUCT :: MCT -> MCT -> MCT 
					maxUCT t1@(N i1 _ _) t2@(N i2 _ _) = if uctValue i1 i >= uctValue i2 i then t1 else t2					
					--uctValue: weight node using UCT Algorithm.
					--current evaluated board; parent board; UCT value
					uctValue :: Info -> Info -> Float
					uctValue (nWin1,nVisits1) (_,nVisits2) = nWin1/nVisits1 + sqrt(2*(log nVisits2)/nVisits1)


		-- expand: when reach a leaf node, expand its children
		-- the current player for leaf node; ;
		expand :: Player -> Board -> [SubMCT]
		expand p b = [(N (0,0) b' []) | b' <- nextValidBoards p b]
			--nextValidBoards 					:: Tile -> Board -> [Board]
		expandedTree :: TBoards -> MCT -> [SubMCT] -> MCT 
		expandedTree [tb] (N i b []) sub = N i b sub 
		expandedTree (tb1:tb2:tbs) (N i b ts) sub = N i b (lts++ [expandedTree (tb2:tbs) t sub] ++rts)
			where
				(lts,t,rts) = sliptSubtrees tb2 ts
		expandedTree tbs t sub = error ("tbs " ++ show (length tbs) ++ " t ")

		-- simulate :: make random choices from valid next move boards till the game end, return (1,1) if win, otherwise return (0,1)
		simulate :: StdGen -> Player -> Player -> Board -> Info
		simulate gen p cp b 
			-- if game end for current player, return info pair
			| isEnd cp b = returnInfo p b
			-- if game is not end for current player, get a random valid next board, and similate it again.
			| otherwise = simulate gen' p (reversePlayer cp) (move cp nextMove b)
			where
				(nextMove, gen') = randomMove gen (nextMoves cp b)

				returnInfo :: Player -> Board -> Info
				returnInfo Black (B _ _ (x,y))
					| x > y = (1,1)
					| otherwise = (0,1)
				returnInfo White (B _ _ (x,y))
					| x < y = (1,1)
					| otherwise = (0,1)
				-- random functions
				randomMove :: StdGen -> [Move] -> (Move, StdGen)
				randomMove _ [] = error "Moves cannot be empty in function randomMove"
				randomMove gen bs = ((!!) bs index, gen')
					where (index, gen') = randomR (0,(length bs) - 1) gen

		-----------
		backPropogation :: Info -> TBoards -> MCT -> MCT -- the length of two array are same
		backPropogation (i,j) [tb] (N (x,y) b ts) 
			| tb == b = N (x+i,y+j) b ts
			| otherwise = error "Wrong TBoards information1"
		backPropogation (i,j) (tb1:tb2:tbs) (N (x,y) b ts) 
			| tb1 == b = (N (x+i,y+j) b (ts1++[backPropogation (i,j) (tb2:tbs) t']++ts2))
			| otherwise = error "Wrong TBoards information2"
			where 
				(ts1, t', ts2) = sliptSubtrees tb2 ts
				{-
				*MCTS> dropWhile (/=3) [1..6]
				[3,4,5,6]
				*MCTS> takeWhile (/=3) [1..6]
				[1,2]
				-}
		sliptSubtrees :: Board -> [MCT] -> ([MCT],MCT,[MCT])
		sliptSubtrees b ts 
			| elem b (map (getBoard) ts) = (takeWhile ((/= b).getBoard) ts, head ts', tail ts')
			| otherwise = error "Board is not in TBoards."
				where
					ts' = dropWhile ((/= b).getBoard) ts







