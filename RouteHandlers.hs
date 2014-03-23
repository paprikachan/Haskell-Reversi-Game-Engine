{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module RouteHandlers where
{-
 - This file defines the route handlers. A handler is responsible for constructing a response to a client.
 - The 'routes' file maps URL's to the handlers defined in this file.
 -}
import Parser
import Runner
import MCTS
import MCTreeT(monteCarloMove,subMCTree)
import Play (reversePlayer)

import Yesod   hiding (Route(..))
import Foundation
import Data.Text
import qualified Data.Aeson as DA
import Data.IORef


-- declare instance in Yesod
instance ToJSON GameJSON where
    toJSON (GameJSON jplayer jmove jboard jcount jend jerror jmoves jlboards) =
            object  [ "player"  .= jplayer
                    , "move"    .= jmove
                    , "board"   .= jboard
                    , "count"   .= jcount
                    , "end"     .= jend
                    , "serror"  .= jerror
                    , "moves"   .= jmoves
                    , "lboard"  .= jlboards
                    ]

--Function for debugging purposes
writeLog :: String -> IO ()
writeLog s = putStrLn "" >> putStrLn ("LOG: " ++ s)

--Handler for server root
getRootR :: Handler Html
getRootR = do
    liftIO $ writeLog "getRoot handler called"
    defaultLayout [whamlet|<p>Server root.|]


-- initialise new game
postNewR :: String -> Handler Value
postNewR s = do
    newGame <- lift $ return $ initialGame
    saveReturnGame newGame ""

postMoveR :: String -> Handler Value
postMoveR sMove = do
    lift $ writeLog ("Recieved Request -- "++sMove)
    GameServer {..} <- getYesod
    originalGame <- lift $ readIORef $ getGame 
    lift $ print move
    newGame <-  lift $ return $ Game (Parser.player originalGame) move (board originalGame) (mcTree originalGame)
    case validMoveGame newGame of
        Just game ->  saveReturnGame game ""
        Nothing -> returnJson $ game2GameJSON originalGame "" ("Invalid Move: "++sMove)
    where
        move = str2Move sMove

postGreedyMiniMaxR :: String -> Handler Value
postGreedyMiniMaxR sDepth = do
    lift $ writeLog ("GreedyMinimax Play")
    aiPlay (read sDepth::Int) GreedyMinimax 

postGreedyAlphaBetaR :: String -> Handler Value
postGreedyAlphaBetaR sDepth = do
    lift $ writeLog ("GreedyAlphaBeta Play")
    aiPlay (read sDepth::Int) GreedyAlphaBeta 

postPositionalMiniMaxR :: String -> Handler Value
postPositionalMiniMaxR sDepth = do
    lift $ writeLog ("PositionalMinimax Play")
    aiPlay (read sDepth::Int) PositionalMinimax 

postPositionalAlphaBetaR :: String -> Handler Value
postPositionalAlphaBetaR sDepth = do
    lift $ writeLog ("PositionalAlphaBeta Play")
    aiPlay (read sDepth::Int) PositionalAlphaBeta 

postMonteCarloR :: String -> Handler Value
postMonteCarloR sSimulate = do
    lift $ writeLog ("MonteCarlo Play")
    --aiPlay MonteCarlo 
    GameServer {..} <- getYesod
    currentGame <- lift $ readIORef $ getGame
    case subMCTree (board currentGame) (mcTree currentGame) of
        Just tree -> do
            liftIO $ atomicModifyIORef getGame $ \_ -> (setTree tree currentGame, setTree tree currentGame)
        _ -> do
            liftIO $ atomicModifyIORef getGame $ \_ -> (iniGameTree currentGame, iniGameTree currentGame)
    newGame <- lift $ readIORef $ getGame
    game <- lift $ mcPlay (read sSimulate::Int) newGame
    saveReturnGame game ""

mcPlay :: Int -> Game -> IO Game
mcPlay simulate (Game p m b t) = if b /= getBoard t then do
    error "Wrong MCTree"
    else do
    tree <- monteCarloMove simulate p b t
    return $ Game (reversePlayer p) m (getBoard tree) tree 

getGameR :: Handler Value
getGameR = do
    GameServer {..} <- getYesod
    game <- lift $ readIORef $ getGame
    returnJson $ game2GameJSON game "" ""


deleteEndR :: Handler Value
deleteEndR = undefined



aiPlay :: Int -> AI -> Handler Value
aiPlay depth ai = do
    GameServer {..} <- getYesod
    (Game p m b t) <- lift $ readIORef $ getGame
    maybeGame <- return $ playAIGame depth ai (Game p m b t)
    case maybeGame of 
        (Just game) -> saveReturnGame game ""
        Nothing -> saveReturnGame (Game (reversePlayer p) m b t) "No valid move, change player."



saveReturnGame :: Game -> String -> Handler Value
saveReturnGame game serror = do 
    GameServer {..} <- getYesod
    lift $ print "Return"
    liftIO $ atomicModifyIORef getGame $ \_ -> (game, game)
    (Game p m b _) <- lift $ return $ game
    lift $ print p
    lift $ putStrLn $ showBoard b
    -- check game end
    if isEndGame game then do
    t <- lift $ return "t"
    lift $ putStrLn t
    returnJson $ game2GameJSON game t serror
    else do
    f <- lift $ return "f"
    lift $ putStrLn f    
    returnJson $ game2GameJSON game f serror










