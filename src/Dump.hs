{-# LANGUAGE OverloadedStrings #-}
module Dump
    ( dumpSection 
    ) where


import Elements

import PhotoShake.ShakeConfig
import PhotoShake.Dump

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Utils.ListZipper
import State (State, States(..), setStates)

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue


dumpSection :: FilePath -> FilePath -> TBMQueue States -> ListZipper State -> ShakeConfig -> UI Element
dumpSection root stateFile queue states config = do
    x <- liftIO $ getDump config

    (_, picker) <- mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder ->
            liftIO $ setDump config $ Dump folder


    case x of
        NoDump -> 
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe ikke valgt" ]
                            , mkColumn ["is-12"] [ element picker ]
                            ]
                      ] 

        Dump y -> do
            (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
            on UI.click buttonForward $ \_ -> liftIO $ setStates queue (States (forward states))

            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe" # set (attr "id") "dumpOK" ]
                            , mkColumn ["is-12"] [ element picker ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            , mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ] 
