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


sessionSection :: ShakeConfig -> UI Element
sessionSection config = do
        x <- liftIO $ getSessions config

        case x of
            NoSessions -> do
                    (_, importer) <- mkFilePicker "sessionPicker" "Vælg config fil" $ \file -> do
                        liftIO $ importSessions config file

                    mkSection [ mkLabel "Sessions ikke valgt"
                              , element importer
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

                    mkSection [ mkLabel "Sessions type"
                              , element select
                              ]
