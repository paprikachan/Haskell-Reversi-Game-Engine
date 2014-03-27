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
    toJSON (GameJSON jid jplayer jmove jboard jcount jend jerror jmoves jlboards jcolor) =
            object  ["id"       .= jid 
                    ,"player"   .= jplayer
                    , "move"    .= jmove
                    , "board"   .= jboard
                    , "count"   .= jcount
                    , "end"     .= jend
                    , "serror"  .= jerror
                    , "moves"   .= jmoves
                    , "lboard"  .= jlboards
                    , "color"   .= jcolor
                    ]

--Function for debugging purposes
writeLog :: String -> IO ()
writeLog s = putStrLn "" >> putStrLn ("LOG: " ++ s)

--Handler for server root
getRootR :: Handler Html
getRootR = do
    liftIO $ writeLog "getRoot handler called"
    defaultLayout [whamlet|<p>Server root.|]


postPlayerPPR :: String -> Handler Value
postPlayerPPR pUserName = do
    GameServer {..} <- getYesod
    waitId <- lift $ readIORef waitIdRef
    if waitId == (-1) then do
    id <- assignId
    lift $ writeLog ("New game " ++ show id ++ ", first player is in.")
    liftIO $ atomicModifyIORef waitIdRef $ \waitId -> (id, id)
    newGame <- lift $ return $ initialGame id
    newGameIORef <- lift $ newIORef newGame
    liftIO $ atomicModifyIORef gamesRef $ \games -> (setGames id newGameIORef games, setGames id newGameIORef games)
    returnJson $ game2GameJSON newGame "" "" "b"
    else do
    lift $ writeLog ("New game " ++ show waitId ++ ", second player is in.")
    liftIO $ atomicModifyIORef waitIdRef $ \waitId -> (-1, -1)
    games <- lift $ readIORef $ gamesRef
    game <- lift $ readIORef $ games !! waitId
    returnJson $ game2GameJSON game "" "" "w"

postPlayerPCR :: String -> Handler Value
postPlayerPCR color = do
    GameServer {..} <- getYesod
    id <- assignId
    lift $ writeLog ("New game " ++ show id)
    newGame <- lift $ return $ initialGame id
    newGameIORef <- lift $ newIORef newGame
    liftIO $ atomicModifyIORef gamesRef $ \games -> (setGames id newGameIORef games, setGames id newGameIORef games)
    returnJson $ game2GameJSON newGame "" "" color



setGames :: Int -> IORef Game -> [IORef Game] -> [IORef Game]
setGames 0 g [] = [g]
setGames 0 g (x:xs) = g:xs
setGames n g (x:xs) = x:(setGames (n-1) g xs) 
setGames _ _ _ = error "Wrong game id"

{-

-}


postMoveR :: Int -> String -> Handler Value
postMoveR id sMove = do
    lift $ writeLog ("Game " ++ show id ++ " Recieved Request -- "++sMove)
    GameServer {..} <- getYesod
    games <- lift $ readIORef $ gamesRef
    originalGame <- lift $ readIORef $ games !! id
    lift $ print move
    newGame <-  lift $ return $ Game id (Parser.player originalGame) move (board originalGame) (mcTree originalGame) True
    case validMoveGame newGame of
        Just game ->  saveReturnGame id game "" ""
        Nothing -> returnJson $ game2GameJSON originalGame "" ("Game " ++ show id ++ " Invalid Move: "++sMove) ""
    where
        move = str2Move sMove

postGreedyMiniMaxR :: Int -> String -> Handler Value
postGreedyMiniMaxR id sDepth = do
    lift $ writeLog ("Game " ++ show id ++ " GreedyMinimax Play")
    aiPlay id (read sDepth::Int) GreedyMinimax 

postGreedyAlphaBetaR :: Int -> String -> Handler Value
postGreedyAlphaBetaR id sDepth = do
    lift $ writeLog ("Game " ++ show id ++ " GreedyAlphaBeta Play")
    aiPlay id (read sDepth::Int) GreedyAlphaBeta 

postPositionalMiniMaxR :: Int -> String -> Handler Value
postPositionalMiniMaxR id sDepth = do
    lift $ writeLog ("Game " ++ show id ++ " PositionalMinimax Play")
    aiPlay id (read sDepth::Int) PositionalMinimax 

postPositionalAlphaBetaR :: Int -> String -> Handler Value
postPositionalAlphaBetaR id sDepth = do
    lift $ writeLog ("Game " ++ show id ++ " PositionalAlphaBeta Play")
    aiPlay id (read sDepth::Int) PositionalAlphaBeta 

postMonteCarloR :: Int -> String -> Handler Value
postMonteCarloR id sSimulate = do
    lift $ writeLog ("Game " ++ show id ++ " MonteCarlo Play")
    --aiPlay MonteCarlo 
    GameServer {..} <- getYesod
    games <- lift $ readIORef $ gamesRef
    currentGame <- lift $ readIORef $ games !! id
    case subMCTree (board currentGame) (mcTree currentGame) of
        Just tree -> do
            liftIO $ atomicModifyIORef (games !! id) $ \_ -> (setTree tree currentGame, setTree tree currentGame)
        _ -> do
            liftIO $ atomicModifyIORef (games !! id) $ \_ -> (iniGameTree currentGame, iniGameTree currentGame)
    newGame <- lift $ readIORef $ games !! id
    game <- lift $ mcPlay (read sSimulate::Int) newGame
    saveReturnGame id game "" ""

mcPlay :: Int -> Game -> IO Game
mcPlay simulate (Game id p m b t _) = if b /= getBoard t then do
    error ("Game " ++ show id ++ " Wrong MCTree")
    else do
    tree <- monteCarloMove simulate p b t
    return $ Game id (reversePlayer p) m (getBoard tree) tree True

getGameR :: Int -> Handler Value
getGameR id = do
    GameServer {..} <- getYesod
    games <- lift $ readIORef $ gamesRef
    game <- lift $ readIORef $ games !! id
    returnJson $ game2GameJSON game "" "" ""


deleteEndR :: Int -> Handler Value
deleteEndR = undefined



aiPlay :: Int -> Int -> AI -> Handler Value
aiPlay id depth ai = do
    GameServer {..} <- getYesod
    games <- lift $ readIORef $ gamesRef
    (Game id p m b t _) <- lift $ readIORef $ games !! id
    maybeGame <- return $ playAIGame depth ai (Game id p m b t True)
    case maybeGame of 
        (Just game) -> saveReturnGame id game "" ""
        Nothing -> saveReturnGame id (Game id (reversePlayer p) m b t True) ("Game " ++ show id ++ " No valid move, change player.") ""



saveReturnGame :: Int -> Game -> String -> String -> Handler Value
saveReturnGame id game serror scolor= do 
    GameServer {..} <- getYesod
    lift $ print "Return"
    games <- lift $ readIORef $ gamesRef
    liftIO $ atomicModifyIORef (games !! id) $ \_ -> (game, game)
    (Game _ p m b _ _) <- lift $ return $ game
    lift $ print p
    lift $ putStrLn $ showBoard b
    -- check game end
    if isEndGame game then do
    t <- lift $ return "t"
    lift $ putStrLn t
    returnJson $ game2GameJSON game t serror scolor
    else do
    f <- lift $ return "f"
    lift $ putStrLn f    
    returnJson $ game2GameJSON game f serror scolor


assignId :: Handler Int
assignId = do
    GameServer {..} <- getYesod
    endIds <- lift $ readIORef endIdsRef
    if endIds == [] then do
    liftIO $ atomicModifyIORef currentIdRef $ \id -> (id+1,id+1)
    lift $ readIORef currentIdRef
    else do
    liftIO $ atomicModifyIORef endIdsRef $ \xs -> (Prelude.tail xs, Prelude.tail xs)
    return $ Prelude.head endIds







