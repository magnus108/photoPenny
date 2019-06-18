{-# LANGUAGE OverloadedStrings #-}
module Doneshooting
    ( doneshootingSection 
    , doneshootingOverview
    ) where

import Elements
import PhotoShake.Doneshooting

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import PhotoShake.ShakeConfig

import Utils.ListZipper
import State (State, States(..), setStates)


doneshootingOverview :: FilePath -> FilePath -> ShakeConfig -> UI Element
doneshootingOverview stateFile states config = do
    x <- liftIO $ getDoneshooting config
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


doneshootingSection :: FilePath -> FilePath -> ListZipper State -> ShakeConfig -> UI Element
doneshootingSection root stateFile states config = do
    x <- liftIO $ getDoneshooting config

    (_, view) <- mkFolderPicker "doneshotingPicker" "Vælg config folder" $ \folder -> do
            liftIO $ setDoneshooting config $ Doneshooting folder

    case x of
        NoDoneshooting -> do
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Doneshooting mappe" # set (attr "id") "doneshootingOK" ]
                            , mkColumn ["is-12"] [ element view ]
                            ]
                      ] 

        Doneshooting y -> do

            (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
            on UI.click buttonForward $ \_ -> liftIO $ setStates root stateFile (States (forward states))

            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Doneshooting mappe" ]
                            , mkColumn ["is-12"] [ element view ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            , mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ] 
