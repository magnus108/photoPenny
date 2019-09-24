{-# LANGUAGE OverloadedStrings #-}
module Session
    ( sessionSection 
    ) where

import Control.Monad 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements
import Menu

import qualified Message as Msg
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan 

import qualified PhotoShake.Session as Session
import qualified PhotoShake.State as State
import qualified Utils.ListZipper as ListZipper

import Utils.FP
import Utils.Actions
import Utils.Comonad

import Data.List

import qualified PhotoShake.State as State
import qualified Utils.ListZipper as ListZipper

import Shooting -- deleteme


sessionSection :: Element -> Chan Msg.Message -> ListZipper.ListZipper State.State -> Session.Sessions -> UI ()
sessionSection body msgs states sessions = do

    (_, picker) <- mkFilePicker "sessionPicker" "Vælg import fil" $ \file -> when (file /= "") $ do
        sessions <- liftIO $ interpret $ Session.getSessions $ fp $ start $ file
        liftIO $ Chan.writeChan msgs $ Msg.setSessions sessions

    view <- Session.sessions (mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Sessions ikke valgt" # set (attr "id") "sessionMissing" ]
                                    , mkColumn ["is-12"] [ element picker ]
                                    ]
                              ] ) (\y -> do

                    let toString = extend (\xx -> Session.session (\_ -> ("Børnehave", xx)) ("Skole",xx) (ListZipper.focus xx) ) y

                    -- wauw
                    let widgets' = (\(ListZipper.ListZipper ls x rs) -> ListZipper.ListZipper (filter (\zz -> (fst zz) /= (fst x)) $ filter (\zz -> (fst zz) `notElem` (fmap fst rs)) $ nubBy (\a b -> fst a == fst b) ls) x (filter (\zz -> (fst zz) /= (fst x)) $ nubBy (\a b -> fst a == fst b) rs)) toString

                    let group' = RadioGroup 
                            { action = \xxx _ -> do
                                    liftIO $ Chan.writeChan msgs $ Msg.setSessions $ Session.yesSessions $ ListZipper.focus (fmap snd xxx)
                            , view' = \xx -> UI.string (ListZipper.focus (fmap fst xx))
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
        ) sessions

    menu <- mkMenu msgs states 

    element body # set children [menu, view]

    return () 

