{-# LANGUAGE OverloadedStrings #-}
module Session
    ( sessionSection
    , sessionOverview
    ) where

import Control.Concurrent.MVar

import qualified Control.Concurrent.Chan as Chan
import Control.Exception

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
import Utils.Actions
import Utils.FP

import PhotoShake.ShakeConfig

import State (State, States(..), setStates)

sessionOverview :: FilePath -> FilePath -> ShakeConfig -> MVar ShakeConfig -> UI Element
sessionOverview stateFile states config config' = do
        x <- liftIO $ withMVar config' $ (\conf -> getSessions conf)

        case x of
            NoSessions -> do
                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Sessions ikke valgt" ]
                                    ]
                              ] 
            Sessions y -> do
                    let toString = (\xx -> case xx of
                                Kindergarten x -> "Børnehave"
                                School -> "Skole"
                            ) (focus y)

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Sessions type" # set (attr "id") "sessionOK" ]
                                    , mkColumn ["is-12"] [ UI.p # set UI.text toString ]
                                    ]
                              ] 



sessionSection :: FilePath -> FilePath -> MVar States -> ListZipper State -> ShakeConfig -> MVar ShakeConfig -> Chan.Chan String -> UI Element
sessionSection root stateFile states'' states config config' importText = do
        x <- liftIO $ withMVar config' $ (\conf -> getSessions conf)

        (_, importer) <- mkFilePicker "sessionPicker" "Vælg import fil" $ \file -> do
            res <- liftIO $ try $ withMVar config' $ (\conf -> importSessions conf file) :: IO (Either SomeException ())
            liftIO $ case res of
                        Left _ ->  Chan.writeChan importText "Kunne ikke importere denne fil"

                        Right x -> return ()

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
                                    liftIO $ withMVar config' $ (\conf -> setSession conf $ Sessions (focus $ fmap snd xx))
                            , view' = \xx -> UI.string (focus (fmap fst xx))
                            , title' = "sessions"
                            , items = widgets'
                            }

                    select <- mkRadioGroup group'

                    (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
                    on UI.click buttonForward $ \_ -> liftIO $ withMVar states'' $ (\_ -> interpret $ setStates (mkFP root stateFile) (States (forward states)))

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Sessions type" # set (attr "id") "sessionOK" ]
                                    , mkColumn ["is-12"] [ element select ]
                                    , mkColumn ["is-12"] [ element importer ]
                                    , mkColumn ["is-12"] [ element forwardView ]
                                    ]
                              ] 


