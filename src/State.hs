{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module State
    ( State(..)
    , States(..)
    , getStates
    , setStates
    ) where
    
import Prelude hiding (readFile, writeFile, length)


import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString.Lazy (readFile, writeFile, length)

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
--    | Summary need for treezipper
    deriving (Show, Eq)


deriveJSON defaultOptions ''State

data States = States (ListZipper State)
    deriving (Show, Eq)

deriveJSON defaultOptions ''States

getStates :: FilePath -> FilePath -> IO States
getStates root stateFile = do
        let filepath = root </> stateFile
        state' <- readFile filepath `catch` \e -> 
                fail ("caught " ++ show (e :: SomeException))
        seq (length state') (return ())
        let state = decode state' :: Maybe States
        case state of
                Nothing -> fail "no states"
                Just y -> return y


setStates:: FilePath -> FilePath -> States -> IO ()
setStates root stateFile states = do
    let filepath = root </> stateFile
    state' <- readFile filepath `catch` \e -> 
            fail ("caught " ++ show (e :: SomeException))
    seq (length state') (writeFile filepath (encode states) `catch` \e -> 
            fail ("Caught " ++ show (e :: SomeException)))
