{-# LANGUAGE OverloadedStrings #-}
module Dagsdato
    ( dagsdatoSection 
    , dagsdatoOverview
    ) where

import Control.Concurrent.MVar

import Elements
import PhotoShake.Dagsdato

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Utils.ListZipper
import Utils.Actions
import State (State, States(..), setStates)

import PhotoShake.ShakeConfig


dagsdatoOverview :: FilePath -> FilePath -> ShakeConfig -> MVar ShakeConfig -> UI Element
dagsdatoOverview stateFile states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getDagsdato conf)
    case x of
        NoDagsdato ->
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato mappe ikke valgt" ]
                            ]
                      ] 

        Dagsdato y -> do
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato mappe" # set (attr "id") "dagsdatoOK" ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            ]
                      ] 

dagsdatoSection :: FilePath -> FilePath -> MVar States -> ListZipper State -> ShakeConfig -> MVar ShakeConfig -> UI Element
dagsdatoSection  root stateFile states'' states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getDagsdato conf)

    (_, view) <- mkFolderPicker "dagsDatoPicker" "Vælg config folder" $ \folder ->
        liftIO $ withMVar config' $ (\conf -> setDagsdato conf $ Dagsdato folder)

    case x of
        NoDagsdato ->
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato mappe ikke valgt" ]
                            , mkColumn ["is-12"] [ element view ]
                            ]
                      ] 

        Dagsdato y -> do
            (buttonForward, forwardView) <- mkButton "next" "Ok"
            on UI.click buttonForward $ \_ -> liftIO $ withMVar states'' $ (\_ -> interpret $ setStates root stateFile (States (forward states)))

            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato mappe" # set (attr "id") "dagsdatoOK" ]
                            , mkColumn ["is-12"] [ element view ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            , mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ] 
