{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Board
import Play
import MCTS

import Data.Array
import Data.Char
import Data.List.Split
import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import qualified Data.ByteString.Lazy.Char8 as B


-- | Type of each JSON entry in record syntax.
data GameJSON =
  GameJSON { 	jid     :: String
            ,   jplayer :: String
  		 	,	jmove 	:: String
         	, 	jboard  :: String
         	,	jcount	:: String
         	,	jend	:: String
         	,	jerror	:: String 
         	, 	jmoves	:: String
         	,	jlboards:: String
            ,   jcolor  :: String
           	} deriving (Show)

data Game = 
	Game{	id      :: Int
        ,   player 	:: Player 
		, 	move 	:: (Int, Int)
		,	board 	:: Board
        ,   mcTree  :: MCT
        ,   end     :: Bool
		} --deriving Show

game2GameJSON :: Game -> String -> String -> String -> GameJSON
game2GameJSON (Game id p m b _ _) end err color = GameJSON (show id) (player2J p) (move2S m) (board2B b) (countString b) end err (moves2J (nextMoves p b)) (showBoard b) color

player2J :: Player -> String
player2J Black = "b"
player2J White = "w"

move2S :: Move -> String
move2S (x,y) = chr (x+97):(show(y+1))

board2B :: Board -> String
board2B (B b _ _) = [ tile2Char (b ! (y,x)) | x <- [0..size-1], y <- [0..size-1] ]
			
tile2Char :: Tile -> Char
tile2Char Black = 'b'
tile2Char Blank = 'e'
tile2Char White = 'w'

moves2J :: [Move] -> String
moves2J ms = concat[ move2S m ++ "," | m<-ms]


str2Move :: String -> Move
str2Move [c,n] = (ord c-97, digitToInt n-1) -- check bounds
str2Move _ = error "Parser Error: Invalid Move String Information."

showBoard                           :: Board -> String
showBoard board@(B b _ c)           = "     a   b   c   d   e   f   g   h\n"
                                    ++  concat (map showRow (board2Rows board))
                                    ++  horizontalLine
                                    ++  showCount c
                                    where
                                        -- Int: index of that row
                                        board2Rows :: Board -> [([Tile],Int)]
                                        board2Rows (B b _ _) = [ ( [ b ! (x,y) | x <- [0..7]],y) | y <- [0..7] ]

                                        showCount :: Count -> String
                                        showCount (x,y) = show x ++ " black tiles, " ++ show y ++ " white tiles."
                                        
                                        showRow :: ([Tile],Int) -> String
                                        showRow (ts,n) = horizontalLine ++ " " ++ show (n+1) ++ " " ++ concat (map showTile ts) ++ "|\n"
                                        
                                        horizontalLine :: String
                                        horizontalLine = "   " ++ concat (replicate 8 "+---") ++ "+\n" 

                                        showTile :: Tile -> String
                                        showTile Black = "| B "
                                        showTile White = "| W "
                                        showTile Blank = "|   "








