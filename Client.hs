module Client where

import Play
import Board
import Tree
import Runner
import Search

import Data.Char
import Data.Array



showBoard 							:: Board -> IO()
showBoard board@(B b _ c) 			= putStrLn $ "     0   1   2   3   4   5   6   7\n" -- "     a   b   c   d   e   f   g   h\n"
									++	concat (map showRow (board2Rows board))
									++	horizontalLine
									++ 	showCount c
									where
										-- Int: index of that row
										board2Rows :: Board -> [([Tile],Int)]
										board2Rows (B b _ _) = [ ( [ b ! (x,y) | x <- [0..7]],y) | y <- [0..7] ]

										showCount :: Count -> String
										showCount (x,y) = show x ++ " black tiles, " ++ show y ++ " white tiles."
										
										showRow :: ([Tile],Int) -> String
										showRow (ts,n) = horizontalLine ++ " " ++ show (n) ++ " " ++ concat (map showTile ts) ++ "|\n"
										
										horizontalLine :: String
										horizontalLine = "   " ++ concat (replicate 8 "+---") ++ "+\n" 

										showTile :: Tile -> String
										showTile Black = "| B "
										showTile White = "| W "
										showTile Blank = "|   "

-- testing ----------------------------------------------------------
showRows tss = putStrLn $ concat (map showRow tss)
showRow :: [Tile]-> String
showRow ts = horizontalLine ++ concat (map showTile ts) ++ "|\n"
horizontalLine :: String
horizontalLine = concat (replicate 8 "+---") ++ "+\n" 
showTile :: Tile -> String
showTile Black = "| B "
showTile White = "| W "
showTile Blank = "|   "

printBoards :: [Board] -> IO()
printBoards [] = putStr ""
printBoards (b:bs) = do 
						showBoard b
						printBoards bs


b1 = nextValidBoards Black initialBoard
b2 = concat (map (nextValidBoards White) b1)
b3 = concat (map (nextValidBoards Black) b2)


---------------------------------------------------------------------


showPlayer 							:: Player -> String
showPlayer Black 					= "black"
showPlayer White  					= "white"
showPlayer _ 						= error "no other player"



endGame 							:: Board -> IO()
endGame (B _ _ (x,y)) 
	| x < y 						= putStrLn "Game ended, white wins!"
	| x > y 						= putStrLn "Game ended, black wins!"
	| x == y 						= putStrLn "Game ended, black and white get an even!"

{-
userTurn 							:: Player -> Move -> Board -> IO()
userTurn p m b 						= case isValidMove p m b of
										True -> do 
													showBoard (move p m b)
													if isEnd (reversePlayer p) (move p m b) then endGame (move p m b)
													else putStrLn ("Now, it's " ++ showPlayer (reversePlayer p) ++ " to move:")
													cn <-getLine
													getMove cn (reversePlayer p) (move p m b)
										False -> do
													putStrLn "Invalid move, please enter again:"
 													cn <- getLine
 													getMove cn p b
-}
userTurn 							:: Player -> Move -> Board -> IO()
userTurn p m b 						= case isValidMove p m b of
										True -> do 
													showBoard (move p m b)
													if isEnd (reversePlayer p) (move p m b) then endGame (move p m b)
													else putStrLn "Now, it's computer's turn to move."
													computerTurn (reversePlayer p) (move p m b)
										False -> do
													putStrLn "Invalid move, please enter again:"
 													cn <- getLine
 													getMove cn p b

computerTurn						:: Tile -> Board -> IO()
computerTurn t b						= do 
										putStrLn "Press Any Key to show computer's move."
										cn <- getLine
										showBoard (computerMove t b)
										if isEnd (reversePlayer t) (computerMove t b) then endGame (computerMove t b)
										else putStrLn "Now, it's user's turn to move:"
										cn <- getLine 
										getMove cn (reversePlayer t) (computerMove t b)
		
initialGame 						:: [Char] -> IO()
initialGame "B" 					= do 
										showBoard initialBoard
										putStrLn "Game started, black turn to move: "
										cn <- getLine
										getMove cn Black initialBoard
initialGame "W" 					= do
										showBoard initialBoard
										putStrLn "Game started, white turn to move: "
										cn <- getLine
										getMove cn White initialBoard
initialGame _ 						= do 
										putStrLn "Please enter 'N' or 'C': "
										c <- getLine
										initialGame c
 
getMove 							:: String -> Player -> Board -> IO()
getMove [x,y] p b
 	| x >= '0' && x <= '7' 
 	&& y >= '0' && y <= '7' 		= do userTurn p (digitToInt x,digitToInt y) b 
 	| otherwise 					= do 
 										putStrLn "Invalid move, please enter again:"
 										cn <- getLine
 										getMove cn p b
getMove _ p b 						= do 
 										putStrLn "Invalid move, please enter again:"
 										cn <- getLine
 										getMove cn p b
 	
c2i :: Char -> Int 
c2i 'a' = 0
c2i 'b' = 1
c2i 'c' = 2
c2i 'd' = 3
c2i 'e' = 4
c2i 'f' = 5
c2i 'g' = 6
c2i 'h' = 7



----------------------------------------------------------------------
--Testing
----------------------------------------------------------------------

run :: IO()
run = do 
		putStrLn ("Welcome to Reversi!\n" 
				++ "Do you want to be black or white?\n"
				++ "Enter B (black) or W (white):")
		cn <- getLine
		initialGame cn 