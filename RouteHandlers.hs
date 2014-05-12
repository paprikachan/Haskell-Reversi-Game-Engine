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
import MCTree
import Play (reversePlayer)
import Foundation

-- Additional Packages 
import Yesod   hiding (Route(..))
import Data.Text
import qualified Data.Aeson as DA
import Data.IORef

data Test = Test Bool Int (Int,Int)

instance ToJSON Test where
    toJSON (Test bool int pair)=
        object ["bool" .= bool,
                "int"  .= int,
                "pair" .= pair
        ] 

-- declare instance in Yesod
instance ToJSON GameJSON where
    toJSON (GameJSON jsuccess jerror jid jplayer jmove jboard jcount jend jmoves jlogger jcolor) =
            object  ["success"      .= jsuccess         -- "t" for no error occured, "f" otherwise.
                    ,"error"        .= jerror           -- "" for error
                    -- Game Information
                    ,"id"           .= jid              -- game id
                    ,"player"       .= jplayer          -- current player
                    , "move"        .= jmove            -- current move
                    , "board"       .= jboard           -- current board
                    , "count"       .= jcount           -- current board count (number of blacks and whites)
                    , "end"         .= jend             -- game end or not
                    , "moves"       .= jmoves           -- next valid moves
                    , "logger"      .= jlogger          -- logger string
                    , "color"       .= jcolor           -- player colour
                    ]


-- Hanlder Functions--


postSignUpR :: String -> String -> String -> Handler Value
postSignUpR email pw username = do 
    -- check if the email is already in database
    userList <- runDB $ selectList [UserEmail ==. email] []
    case userList of
        (x:xs) -> returnJson $ gameJSON blankGame False "This email already signed up." False ""
        --object ["success" .= show "f", "message" .= show "This email already sign up."]
        otherwise -> do
            liftIO $ writeLog "I am in here1\n"
            userId <- runDB $ insert $ User email pw username 0
            liftIO $ writeLog "I am in here2\n"
            user <- runDB $ get userId
            returnJson $ gameJSON blankGame True "" False "" -- object ["success" .= show "t"]
    


postSignInR :: String -> String -> Handler Value
postSignInR email pw = do
    -- how to extrat the field informaiton from user -- This is way is better, need Internet to search
    maybeUser <- runDB $ getBy $ UniqueEmail email
    case maybeUser of
        Nothing -> returnJson $ gameJSON blankGame False "No Such Email Address." False ""
        --object ["success" .= show "f", "message" .= show "No Such Email Address"]
        Just (Entity userId user) -> if pw == userPassword user then do
            returnJson $ gameJSON blankGame True "" False "" --object ["success" .= show "t"]
            -- log in 
            else do
            returnJson $ gameJSON blankGame False "Wrong Password." False ""
            --object ["success" .= show "f", "message" .= show "Wrong Password"]
    

    -- Type Problems

    {- print user: user is an Entity, consists of entityKey (user id) and entityVal (user)
    [Entity {entityKey = Key {unKey = PersistInt64 1}, entityVal = User {userEmail = "f", userPassword = "fff", userUsername = "fff", userBestScore = 0}},Entity {entityKey = Key {unKey = PersistInt64 2}, entityVal = User {userEmail = "ff", userPassword = "fff", userUsername = "f", userBestScore = 0}}]
    
    If there is no fitting requirement, return []
    -}


     -- selet the one whose email and pw is matched
     {-}
     userList <- runDB $ selectList ([UserEmail ==. email] ||. [UserPassword ==. pw]) []
     user <- liftIO $ return $ Prelude.head userList
     liftIO $ print (userEmail user)   
     liftIO $ print "\n"
     return $ return $ (userEmail user)   
     -}


-------------Play

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
    returnJson $ gameJSON newGame True "" False "b" --game2GameJSON newGame "" "" "b"
    else do
    lift $ writeLog ("New game " ++ show waitId ++ ", second player is in.")
    liftIO $ atomicModifyIORef waitIdRef $ \waitId -> (-1, -1)
    games <- lift $ readIORef $ gamesRef
    game <- lift $ readIORef $ games !! waitId
    returnJson $ gameJSON game True "" False "w" --game2GameJSON game "" "" "w"

postPlayerPCR :: String -> Handler Value
postPlayerPCR color = do
    GameServer {..} <- getYesod
    id <- assignId
    lift $ writeLog ("New game " ++ show id)
    newGame <- lift $ return $ initialGame id
    newGameIORef <- lift $ newIORef newGame
    liftIO $ atomicModifyIORef gamesRef $ \games -> (setGames id newGameIORef games, setGames id newGameIORef games)
    returnJson $ gameJSON newGame True "" False color --game2GameJSON newGame "" "" color

postMoveR :: Int -> String -> Handler Value
postMoveR id sMove = do
    -- check id
    bool <- validId id 
    if bool == True then do
    lift $ writeLog ("Game " ++ show id ++ " Recieved Request -- "++sMove)
    GameServer {..} <- getYesod
    games <- lift $ readIORef $ gamesRef
    originalGame <- lift $ readIORef $ games !! id
    lift $ print move
    newGame <-  lift $ return $ Game id (Runner.player originalGame) move (board originalGame) (mcTree originalGame) True
    case validMovesGame newGame of
        Just game ->  saveReturnGame id game True "" ""
        Nothing -> returnJson $ gameJSON originalGame False ("Game " ++ show id ++ " Invalid Move: "++sMove) False "" --game2GameJSON originalGame "" ("Game " ++ show id ++ " Invalid Move: "++sMove) ""
    else do
    returnJson $ gameJSON blankGame False "Invalid game ID." False ""
    --object  ["sucess" .= show "f", "message" .= show "Invalid game id."]
    where
        move = str2Move sMove

postGreedyMiniMaxR :: Int -> Int -> Handler Value
postGreedyMiniMaxR id depth = do
    -- check id
    bool <- validId id 
    if bool == True then do
    lift $ writeLog ("Game " ++ show id ++ " GreedyMinimax Play")
    aiPlay id depth GreedyMinimax
    else do
    returnJson $ gameJSON blankGame False "Invalid game ID." False ""--object ["sucess" .= show "f", "message" .= show "Invalid game id."] 

postGreedyAlphaBetaR :: Int -> Int -> Handler Value
postGreedyAlphaBetaR id depth = do
    -- check id
    bool <- validId id 
    if bool == True then do
    lift $ writeLog ("Game " ++ show id ++ " GreedyAlphaBeta Play")
    aiPlay id depth GreedyAlphaBeta
    else do
    returnJson $ gameJSON blankGame False "Invalid game ID." False ""--object ["sucess" .= show "f", "message" .= show "Invalid game id."] 


postMonteCarloR :: Int -> Int -> Handler Value
postMonteCarloR id simulations = do
    -- check id
    bool <- validId id 
    if bool == True then do
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
    game <- lift $ mcPlay simulations newGame
    saveReturnGame id game True "" ""
    else do
    returnJson $ gameJSON blankGame False "Invalid game ID." False ""--object ["sucess" .= show "f", "message" .= show "Invalid game id."]


getGameR :: Int -> Handler Value
getGameR id = do
    GameServer {..} <- getYesod
    -- check id
    bool <- validId id 
    if bool == True then do
    games <- lift $ readIORef $ gamesRef
    game <- lift $ readIORef $ games !! id
    returnJson $ gameJSON game True "" False "" --game2GameJSON game "" "" ""
    else do
    returnJson $ gameJSON blankGame False "Invalid game ID." False ""--object ["sucess" .= show "f", "message" .= show "Invalid game id."]

-- endGame, delete the game, put game id into endIdsRef 
postEndR :: Int -> Handler Value
postEndR id = do
    GameServer {..} <- getYesod
    -- check id
    bool <- validId id 
    if bool == True then do
    endGame id
    returnJson $ gameJSON blankGame True "" True "" -- object ["end" .= show "t"]
    else do
    returnJson $ gameJSON blankGame False "Invalid game ID." False ""--object ["sucess" .= show "f", "message" .= show "Invalid game id."]
-- Helper Functions--

endGame :: Int -> Handler [Int]
endGame id = do
    GameServer {..} <- getYesod
    endIds <- lift $ readIORef $ endIdsRef
    liftIO $ atomicModifyIORef endIdsRef $ \_ -> (id:endIds, id:endIds)


--Function for debugging purposes, write current logger in terminal
writeLog :: String -> IO ()
writeLog s = putStrLn "" >> putStrLn ("LOG: " ++ s)

-- function setGames: given an array of game references, 
-- change the i-th element in array with newly provided game reference. 
-- i is the specified game id, game id number starts with 0.
setGames :: Int -> IORef Game -> [IORef Game] -> [IORef Game]
setGames 0 g [] = [g]
setGames 0 g (x:xs) = g:xs
setGames n g (x:xs) = x:(setGames (n-1) g xs) 
setGames _ _ _ = error "Wrong game id"


{-  mcPlay
    input: specified simulation times, game status
    return: next valid game status after montecarlo simulation
    the return game is wrapped into IO monad 
-}
mcPlay :: Int -> Game -> IO Game
mcPlay simulate (Game id p m b t _) = if b /= getBoard t then do
    error ("Game " ++ show id ++ " Wrong MCTree")
    else do
    tree <- mcSimulationTree2Tree simulate p t
    return $ Game id (reversePlayer p) m (getBoard tree) tree True





aiPlay :: Int -> Int -> AI -> Handler Value
aiPlay id depth ai = do
    GameServer {..} <- getYesod
    games <- lift $ readIORef $ gamesRef
    (Game id p m b t _) <- lift $ readIORef $ games !! id
    maybeGame <- return $ playAIGame depth ai (Game id p m b t True)
    case maybeGame of 
        (Just game) -> saveReturnGame id game True "" ""
        Nothing -> saveReturnGame id (Game id (reversePlayer p) m b t True) False ("Game " ++ show id ++ " No valid move, change player.") ""



saveReturnGame :: Int -> Game -> Bool -> String -> String -> Handler Value
saveReturnGame id game success serror scolor= do 
    GameServer {..} <- getYesod
    lift $ print "Return"
    games <- lift $ readIORef $ gamesRef
    liftIO $ atomicModifyIORef (games !! id) $ \_ -> (game, game)
    (Game _ p m b _ _) <- lift $ return $ game
    lift $ print p
    lift $ putStrLn $ showBoard b
    -- check game end
    if isEndGame game then do
    --t <- lift $ return "t"
    --lift $ putStrLn t
    returnJson $ gameJSON game success serror True scolor --game2GameJSON game t serror scolor
    else do
    --f <- lift $ return "f"
    --lift $ putStrLn f    
    returnJson $ gameJSON game success serror False scolor --game2GameJSON game f serror scolor


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

-- check if the game id is valid.
validId :: Int -> Handler Bool 
validId id = do
    GameServer {..} <- getYesod
    -- less than current id length
    currentId <- lift $ readIORef $ currentIdRef
    -- not belong to endIds
    endIds <- lift $ readIORef $ endIdsRef
    if id >= 0 && id <= currentId && not (elem id endIds) then return $ True
    else return $ False





