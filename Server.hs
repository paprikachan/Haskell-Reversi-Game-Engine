{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Server where

{-
 - This file defines the entry point to the server application
 -}
import Yesod          --The webserver framework we'll use
import Yesod.Static

import Foundation     --Import the underlying site data structures
import RouteHandlers  --Import the sites route handlers

import Data.IORef
import Board
import MCTS
import Runner

--Tie together the site
mkYesodDispatch "GameServer" resourcesGameServer

--Entry point
server = do
    client@(Static settings) <- static "client"
    gameRef <- newIORef initialGame
    warp 3000 $ GameServer client gameRef
