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

import Utils.Comonad
import Utils.ListZipper
import State (State, States(..), setStates)


sessionSection :: FilePath -> ListZipper State -> ShakeConfig -> UI Element
sessionSection root states config = do
        x <- liftIO $ getSessions config

        case x of
            NoSessions -> do
                    (_, importer) <- mkFilePicker "sessionPicker" "Vælg config fil" $ \file -> do
                        liftIO $ importSessions config file

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Sessions ikke valgt" ]
                                    , mkColumn ["is-12"] [ element importer ]
                                    ]
                              ] 


            Sessions y -> do
                    let group' = RadioGroup 
                            { action = \x _ -> do
                                    liftIO $ setSession config $ Sessions x
                            , view' = \x -> UI.string (show (focus x))
                            , title' = "sessions"
                            , items = y
                            }

                    select <- mkRadioGroup group'

                    (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
                    on UI.click buttonForward $ \_ -> liftIO $ setStates root (States (forward states))

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Sessions type" ]
                                    , mkColumn ["is-12"] [ element select ]
                                    , mkColumn ["is-12"] [ element forwardView ]
                                    ]
                              ] 
