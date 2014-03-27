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
import Data.IORef

import Foundation     --Import the underlying site data structures
import RouteHandlers  --Import the sites route handlers




--Tie together the site
mkYesodDispatch "GameServer" resourcesGameServer

--Entry point
server = do
    client@(Static settings) <- static "client"
    idRef <- newIORef (-1)
    endRef <- newIORef []
    gamesRef <- newIORef []
    numRef <- newIORef 0
    waitIdRef <- newIORef (-1)
    warp 3000 $ GameServer client idRef endRef gamesRef numRef waitIdRef


