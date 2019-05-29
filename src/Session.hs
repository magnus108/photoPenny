{-# LANGUAGE OverloadedStrings #-}
module Session
    ( sessionSection
    ) where

import Prelude hiding (writeFile)
import Data.ByteString.Lazy hiding (take, putStrLn)

---ups
import Shooting
import PhotoShake.Shooting
---ups
import PhotoShake.Session

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import Utils.ListZipper 

import PhotoShake.ShakeConfig


sessionSection :: FilePath -> FilePath -> UI Element
sessionSection root config = mkSection
                                [ mkLabel "Session Type"
                                , mkRadioSessions root config
                                ]


mkRadioSessions :: FilePath -> FilePath -> UI Element
mkRadioSessions _ config = do 
    sessions <- liftIO $ getSessions config
    let group' = RadioGroup 
            { action = \x _ -> do
                    liftIO $ writeFile config $ encode (Sessions x)
                    return ()
            , view' = \x -> UI.string (show (focus x))
            , title' = "sessions"
            , items = unSessions sessions
            }
    view <- mkRadioGroup group'
    return view

