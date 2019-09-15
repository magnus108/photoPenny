{-# LANGUAGE OverloadedStrings #-}
module Session
    ( sessionSection 
    ) where

import Control.Monad 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import qualified Message as Msg
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan 

import PhotoShake.Session

import Utils.FP
import Utils.Actions
import Utils.ListZipper
import Utils.Comonad

import Data.List


import Shooting -- deleteme


sessionSection :: Chan Msg.Message -> Sessions -> UI Element
sessionSection msgs x = do

    (_, picker) <- mkFilePicker "sessionPicker" "Vælg import fil" $ \file -> when (file /= "") $ do
        sessions <- liftIO $ interpret $ getSessions $ fp $ start $ file
        liftIO $ Chan.writeChan msgs $ Msg.setSessions sessions

    sessions (mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Sessions ikke valgt" # set (attr "id") "sessionMissing" ]
                                    , mkColumn ["is-12"] [ element picker ]
                                    ]
                              ] ) (\y -> do

                    let toString = extend (\xx -> session (\_ -> ("Børnehave", xx)) ("Skole",xx) (focus xx) ) y

                    -- wauw
                    let widgets' = (\(ListZipper ls x rs) -> ListZipper (filter (\zz -> (fst zz) /= (fst x)) $ filter (\zz -> (fst zz) `notElem` (fmap fst rs)) $ nubBy (\a b -> fst a == fst b) ls) x (filter (\zz -> (fst zz) /= (fst x)) $ nubBy (\a b -> fst a == fst b) rs)) toString

                    let group' = RadioGroup 
                            { action = \xxx _ -> do
                                    liftIO $ Chan.writeChan msgs $ Msg.setSessions $ yesSessions $ focus (fmap snd xxx)
                            , view' = \xx -> UI.string (focus (fmap fst xx))
                            , title' = "sessions"
                            , items = widgets'
                            }

                    select <- mkRadioGroup group'

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Sessions type" # set (attr "id") "sessionOK" ]
                                    , mkColumn ["is-12"] [ element select ]
                                    , mkColumn ["is-12"] [ element picker ]
                                    ]
                              ] 
        ) x

