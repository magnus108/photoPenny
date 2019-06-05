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


sessionSection :: ShakeConfig -> UI (Bool, Element)
sessionSection config = do
        x <- liftIO $ getSessions config
        case x of
            NoSessions -> do
                    gg <- mkSection [ mkLabel "Sessions ikke valgt"
                                    , mkSessionsImporter config
                                    ]
                    return (False, gg)

            Sessions y -> do
                        gg <- mkSection [ mkLabel "Sessions type"
                                        , mkRadioSessions config y
                                        ]
                        return (True, gg)

mkSessionsImporter :: ShakeConfig -> UI Element
mkSessionsImporter config = do
    (_, view) <- mkFilePicker "sessionPicker" "Vælg config fil" $ \file -> do
        liftIO $ importSessions config file
    return view


mkRadioSessions :: ShakeConfig -> ListZipper Session -> UI Element
mkRadioSessions config y = do 
    let group' = RadioGroup 
            { action = \x _ -> do
                    liftIO $ setSession config $ Sessions x
            , view' = \x -> UI.string (show (focus x))
            , title' = "sessions"
            , items = y
            }
    view <- mkRadioGroup group'
    return view

