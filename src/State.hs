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

import Utils.Comonad
import Utils.ListZipper
import Utils.Env
import Utils.Actions

import Utils.FP


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


getStates :: FP -> TerminalM States States
getStates = readFile 


setStates:: FP -> States -> TerminalM States ()
setStates = writeFile 
