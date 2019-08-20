{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE LambdaCase #-}                                                      
{-# LANGUAGE DeriveFunctor #-}  

module State
    ( State(..)
    , States(..)
    , getStates
    , setStates
    ) where

import Text.Show
import Data.Function (($))
import Data.Maybe
import Data.Eq
import GHC.Generics
import Control.Monad
    
import System.FilePath

import Conduit
import Data.Conduit.Combinators

import Data.Aeson

import Utils.ListZipper
import Utils.Actions


import System.IO (IO)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import Prelude (error)
import Data.Conduit.Attoparsec
import           Data.Aeson              (FromJSON, Result (..), eitherDecodeStrict',
                                          fromJSON, json, Value)

data State
    = Dump
    | Dagsdato
    | Doneshooting
    | Photographer
    | Shooting
    | Session
    | Location
    | Main
    | Control
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data States = States (ListZipper State)
    deriving (Show, Eq, Generic, ToJSON, FromJSON)



sinkFromJSON :: (MonadThrow m, FromJSON a) => ConduitM ByteString o m a
sinkFromJSON = do
    value <- sinkParser json
    case fromJSON value of
        Error e -> error e
        Success x -> return x


getStates2 :: FilePath -> FilePath -> IO States
getStates2 root stateFile = do
    let filepath = root </> stateFile
    runConduitRes $ sourceFileBS filepath
                 .| sinkFromJSON 


getStates :: FilePath -> FilePath -> TerminalM States
getStates root stateFile = do
    let filepath = root </> stateFile
    state <- readFile filepath
    return (fromJust (decode state))


setStates:: FilePath -> FilePath -> States -> TerminalM ()
setStates root stateFile states = do
    let filepath = root </> stateFile
    writeFile filepath (encode states)
