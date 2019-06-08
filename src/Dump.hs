{-# LANGUAGE OverloadedStrings #-}
module Dump
    ( dumpSection 
    ) where


import Elements

import PhotoShake.ShakeConfig
import PhotoShake.Dump

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Utils.Comonad
import Utils.ListZipper
import State (State, States(..), setStates)


dumpSection :: FilePath -> ListZipper State -> ShakeConfig -> UI Element
dumpSection root states config = do
    x <- liftIO $ getDump config

    (_, picker) <- mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder ->
            liftIO $ setDump config $ Dump folder

    (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
    on UI.click buttonForward $ \_ -> liftIO $ setStates root (States (forward states))

    case x of
        NoDump -> 
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe ikke valgt" ]
                            , mkColumn ["is-12"] [ element picker ]
                            ]
                      ] 

        Dump y ->
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe" ]
                            , mkColumn ["is-12"] [ element picker ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            , mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ] 
