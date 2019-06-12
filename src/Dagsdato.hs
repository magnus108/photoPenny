{-# LANGUAGE OverloadedStrings #-}
module Dagsdato
    ( dagsdatoSection 
    ) where


import Elements
import PhotoShake.Dagsdato

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Utils.ListZipper
import State (State, States(..), setStates)

import PhotoShake.ShakeConfig

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue


dagsdatoSection :: FilePath -> FilePath -> TBMQueue States -> ListZipper State -> ShakeConfig -> UI Element
dagsdatoSection  root stateFile queue states config =  do
    x <- liftIO $ getDagsdato config

    (_, view) <- mkFolderPicker "dagsDatoPicker" "Vælg config folder" $ \folder ->
        liftIO $ setDagsdato config $ Dagsdato folder

    case x of
        NoDagsdato ->
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato mappe ikke valgt" ]
                            , mkColumn ["is-12"] [ element view ]
                            ]
                      ] 

        Dagsdato y -> do
            (buttonForward, forwardView) <- mkButton "next" "Ok"
            on UI.click buttonForward $ \_ -> liftIO $ setStates queue (States (forward states))

            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato mappe" # set (attr "id") "dagsdatoOK" ]
                            , mkColumn ["is-12"] [ element view ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            , mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ] 
