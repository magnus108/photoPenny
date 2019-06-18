{-# LANGUAGE OverloadedStrings #-}
module Session
    ( sessionSection
    ) where

---ups
import Shooting
---ups

import Data.List

import PhotoShake.Session

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import Utils.ListZipper 
import Utils.Comonad

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
                    let toString = extend (\xx -> case focus xx of
                                Kindergarten x -> ("Børnehave", xx)
                                School -> ("Skole",xx)
                            ) y

                    -- wauw
                    let widgets' = (\(ListZipper ls x rs) -> ListZipper (filter (\zz -> (fst zz) /= (fst x)) $ filter (\zz -> (fst zz) `notElem` (fmap fst rs)) $ nubBy (\a b -> fst a == fst b) ls) x (filter (\zz -> (fst zz) /= (fst x)) $ nubBy (\a b -> fst a == fst b) rs)) toString

                    let group' = RadioGroup 
                            { action = \xx _ -> do
                                    liftIO $ setSession config $ Sessions (focus $ fmap snd xx)
                            , view' = \xx -> UI.string (focus (fmap fst xx))
                            , title' = "sessions"
                            , items = widgets'
                            }

                    select <- mkRadioGroup group'

                    (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
                    on UI.click buttonForward $ \_ -> liftIO $ setStates root stateFile (States (forward states))

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Sessions type" # set (attr "id") "sessionOK" ]
                                    , mkColumn ["is-12"] [ element select ]
                                    , mkColumn ["is-12"] [ element importer ]
                                    , mkColumn ["is-12"] [ element forwardView ]
                                    ]
                              ] 


