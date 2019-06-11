{-# LANGUAGE OverloadedStrings #-}
module Session
    ( sessionSection
    ) where


---ups
import Shooting
---ups
import PhotoShake.Session

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import Utils.ListZipper 

import PhotoShake.ShakeConfig

import State (State, States(..), setStates)


sessionSection :: FilePath -> FilePath -> ListZipper State -> ShakeConfig -> UI Element
sessionSection root stateFile states config = do
        x <- liftIO $ getSessions config

        (_, importer) <- mkFilePicker "sessionPicker" "Vælg import fil" $ \file -> do
            liftIO $ importSessions config file

        case x of
            NoSessions -> do

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Sessions ikke valgt" ]
                                    , mkColumn ["is-12"] [ element importer ]
                                    ]
                              ] 


            Sessions y -> do
                    let group' = RadioGroup 
                            { action = \xx _ -> do
                                    liftIO $ setSession config $ Sessions xx
                            , view' = \xx -> UI.string (show (focus xx))
                            , title' = "sessions"
                            , items = y
                            }

                    select <- mkRadioGroup group'

                    (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
                    on UI.click buttonForward $ \_ -> liftIO $ setStates root stateFile (States (forward states))

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Sessions type" ]
                                    , mkColumn ["is-12"] [ element select ]
                                    , mkColumn ["is-12"] [ element importer ]
                                    , mkColumn ["is-12"] [ element forwardView ]
                                    ]
                              ] 
