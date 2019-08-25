{-# LANGUAGE OverloadedStrings #-}
module Dump
    ( dumpSection 
    , dumpOverview
    ) where


import Elements

import PhotoShake.ShakeConfig
import PhotoShake.Dump

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Utils.ListZipper
import Utils.FP
import State (State, States(..), setStates)

import Control.Concurrent.MVar

import Utils.Actions

dumpOverview :: FilePath -> FilePath -> ShakeConfig -> MVar ShakeConfig -> UI Element
dumpOverview stateFile states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getDump conf)

    case x of
        NoDump -> 
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe ikke valgt" ]
                            ]
                      ] 

        Dump y -> do
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe" # set (attr "id") "dumpOK" ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            ]
                      ] 


dumpSection :: FilePath -> FilePath -> MVar States -> ListZipper State -> ShakeConfig -> MVar ShakeConfig -> UI Element
dumpSection root stateFile states'' states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getDump conf)

    (_, picker) <- mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder ->
            liftIO $ withMVar config' $ (\conf -> setDump conf $ Dump folder)


    case x of
        NoDump -> 
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe ikke valgt" ]
                            , mkColumn ["is-12"] [ element picker ]
                            ]
                      ] 

        Dump y -> do
            (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
            on UI.click buttonForward $ \_ -> liftIO $ withMVar states'' $ (\_ ->  interpret $ setStates (mkFP root stateFile) (States (forward states)))

            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe" # set (attr "id") "dumpOK" ]
                            , mkColumn ["is-12"] [ element picker ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            , mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ] 
