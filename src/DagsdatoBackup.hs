{-# LANGUAGE OverloadedStrings #-}
module DagsdatoBackup
    ( dagsdatoBackupSection 
    , dagsdatoBackupOverview
    ) where

import Control.Concurrent.MVar

import Elements
import PhotoShake.Dagsdato

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Utils.ListZipper
import Utils.Actions
import Utils.FP
import PhotoShake.State (State, States(..), setStates)

import PhotoShake.ShakeConfig


dagsdatoBackupOverview :: FilePath -> FilePath -> ShakeConfig -> MVar ShakeConfig -> UI Element
dagsdatoBackupOverview stateFile states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getDagsdatoBackup conf)
    case x of
        NoDagsdato ->
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato Backup mappe ikke valgt" ]
                            ]
                      ] 

        Dagsdato y -> do
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato Backup mappe" # set (attr "id") "dagsdatoOK" ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            ]
                      ] 

dagsdatoBackupSection :: FilePath -> FilePath -> MVar States -> ListZipper State -> ShakeConfig -> MVar ShakeConfig -> UI Element
dagsdatoBackupSection  root stateFile states'' states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getDagsdatoBackup conf)

    (_, view) <- mkFolderPicker "dagsDatoPicker" "Vælg config folder" $ \folder ->
        liftIO $ withMVar config' $ (\conf -> setDagsdatoBackup conf $ Dagsdato folder)

    case x of
        NoDagsdato ->
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato Backup mappe ikke valgt" ]
                            , mkColumn ["is-12"] [ element view ]
                            ]
                      ] 

        Dagsdato y -> do
            (buttonForward, forwardView) <- mkButton "next" "Ok"
            on UI.click buttonForward $ \_ -> liftIO $ withMVar states'' $ (\_ ->  interpret $ setStates (mkFP root stateFile) (States (forward states)))

            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato backup mappe" # set (attr "id") "dagsdatoBackupOK" ]
                            , mkColumn ["is-12"] [ element view ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            , mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ] 
