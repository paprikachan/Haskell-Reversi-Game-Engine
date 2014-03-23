{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Foundation where
{-
 - This file defines the underlying datastructures the site is built upon.
 - Compile-time settings should end up here
 -}

import Yesod
import Yesod.Static

import Data.IORef
import MCTS
import Parser

--Serve static client files from under /client/
staticFiles "client"
data GameServer = GameServer {	
	getClient :: Static,
	getGame :: IORef Game
	}

--Generate routing map from file
mkYesodData "GameServer" $(parseRoutesFile "routes")
instance Yesod GameServer
