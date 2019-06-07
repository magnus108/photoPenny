{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module State
    ( State(..)
    , States(..)
    , getStates
    , setStates
    ) where
    
import Prelude hiding (readFile, writeFile)


import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString.Lazy (readFile, writeFile)

import System.FilePath
import Control.Exception

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
    deriving (Show)


deriveJSON defaultOptions ''State

data States = States (ListZipper State)
    deriving (Show)

deriveJSON defaultOptions ''States

stateFile :: String
stateFile = "config/state.json"

getStates :: FilePath -> IO States
getStates root = do
        let filepath = root </> stateFile
        putStrLn filepath
        state' <- readFile filepath `catch` \e -> 
                fail ("Caught " ++ show (e :: SomeException))
        let state = decode state' :: Maybe States
        case state of
                Nothing -> fail "no states"
                Just y -> return y


setStates:: FilePath -> States -> IO ()
setStates root states = do
    let filepath = root </> stateFile
    writeFile filepath (encode states) `catch` \e -> 
            fail ("Caught " ++ show (e :: SomeException))
