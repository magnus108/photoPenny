{-# LANGUAGE OverloadedStrings #-}
module Doneshooting
    ( doneshootingSection 
    , doneshootingOverview
    ) where

import Elements
import PhotoShake.Doneshooting

import Control.Concurrent.MVar

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import PhotoShake.ShakeConfig

import Utils.ListZipper
import Utils.FP
import State (State, States(..), setStates)

import Utils.Actions


doneshootingOverview :: FilePath -> FilePath -> ShakeConfig -> MVar ShakeConfig -> UI Element
doneshootingOverview stateFile states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getDoneshooting conf)
    
    case x of
        NoDoneshooting -> do
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Doneshooting mappe" # set (attr "id") "doneshootingOK" ]
                            ]
                      ] 

        Doneshooting y -> do
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Doneshooting mappe" ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            ]
                      ] 


doneshootingSection :: FilePath -> FilePath -> MVar States -> ListZipper State -> ShakeConfig -> MVar ShakeConfig -> UI Element
doneshootingSection root stateFile states'' states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getDoneshooting conf)

    (_, view) <- mkFolderPicker "doneshotingPicker" "Vælg config folder" $ \folder -> do
            liftIO $ withMVar config' $ (\conf -> setDoneshooting conf $ Doneshooting folder)

    case x of
        NoDoneshooting -> do
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Doneshooting mappe" # set (attr "id") "doneshootingOK" ]
                            , mkColumn ["is-12"] [ element view ]
                            ]
                      ] 

        Doneshooting y -> do

            (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
            on UI.click buttonForward $ \_ -> liftIO $ withMVar states'' $ (\_ ->  interpret $ setStates (mkFP root stateFile) (States (forward states)))

            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Doneshooting mappe" ]
                            , mkColumn ["is-12"] [ element view ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            , mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ] 
