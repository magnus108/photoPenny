{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module State
    ( State(..)
    , States(..)
    , getStates
    , setStates
    , drainQueue
    ) where
    
import Prelude hiding (readFile, writeFile, length)


import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString.Lazy (readFile, writeFile, length)

import System.FilePath
import Control.Exception

import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue

import Utils.ListZipper

data State
    = Dump
    | Dagsdato
    | Doneshooting
    | Photographer
    | Shooting
    | Session
    | Location
    | Main
    deriving (Show, Eq)


deriveJSON defaultOptions ''State

data States = States (ListZipper State)
    deriving (Show, Eq)

deriveJSON defaultOptions ''States


getStates :: FilePath -> FilePath -> IO States
getStates root stateFile = do
        let filepath = root </> stateFile
        state' <- readFile filepath `catch` \e -> 
                fail ("Caught " ++ show (e :: SomeException))
        seq (length state') (return ())

        let state = decode state' :: Maybe States
        case state of
                Nothing -> fail "no states"
                Just y -> return y


setStates:: TBMQueue States -> States -> IO ()
setStates queue states = do
    atomically $ writeTBMQueue queue states


drainQueue :: FilePath -> FilePath -> TBMQueue States -> IO ()
drainQueue root stateFile queue = loop
    where
        loop = do
            mnext <- atomically $ readTBMQueue queue
            case mnext of
                Nothing -> return ()
                Just states -> do
                    let filepath = root </> stateFile
                    writeFile filepath (encode states) `catch` \e -> 
                            fail ("Caught " ++ show (e :: SomeException))
