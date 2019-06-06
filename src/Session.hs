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

            UnApprovedSessions y -> do
                        gg <- mkSection [ mkLabel "Vælg Session"
                                        , mkApprovedSessions config y $ \z _ ->
                                                liftIO $ setSession config $ ApprovedSessions z
                                        ]
                        return (False, gg)

            ApprovedSessions y -> do
                        gg <- mkSection [ mkLabel "Sessions type"
                                        , mkRadioSessions config y
                                        ]
                        return (True, gg)

mkSessionsImporter :: ShakeConfig -> UI Element
mkSessionsImporter config = do
    (_, view) <- mkFilePicker "sessionPicker" "Vælg config fil" $ \file -> do
        liftIO $ importSessions config file
    return view

mkApprovedSessions :: ShakeConfig -> ListZipper Session -> (ListZipper Session -> () -> UI ()) -> UI Element
mkApprovedSessions config y cb = do
    (button, buttonView) <- mkButton "approveSessions" "ok"

    on UI.click button (cb y)

    view <- UI.div #+
                [ mkRadioSessions config y
                , element buttonView
                ]
    return view


mkRadioSessions :: ShakeConfig -> ListZipper Session -> UI Element
mkRadioSessions config y = do 
    let group' = RadioGroup 
            { action = \x _ -> do
                    liftIO $ setSession config $ ApprovedSessions x
            , view' = \x -> UI.string (show (focus x))
            , title' = "sessions"
            , items = y
            }
    view <- mkRadioGroup group'
    return view

