{-# LANGUAGE OverloadedStrings #-}
module Photographer
    ( photographerSection
    , photographerOverview
    ) where

import qualified Control.Concurrent.Chan as Chan
import Control.Exception

import Prelude hiding (writeFile)

import PhotoShake.Photographer

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import Utils.FP 
import Utils.ListZipper 
import Utils.Actions 

{- ups -}
import Shooting

import PhotoShake.ShakeConfig

import PhotoShake.State (State, States(..), setStates)

import Control.Concurrent.MVar

photographerOverview :: FilePath -> FilePath -> ShakeConfig -> MVar ShakeConfig -> UI Element
photographerOverview stateFile states config config' = do
        x <- liftIO $ withMVar config' $ (\conf -> getPhotographers conf)

        case x of
            NoPhotographers -> do

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Fotograf ikke valgt - importer fil" ]
                                    ]
                              ] 

            Photographers y -> do
                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Fotograf" # set (attr "id") "photographerOK" ]
                                    , mkColumn ["is-12"] [ UI.string (name (focus y))]
                                    ]
                              ] 

photographerSection :: FilePath -> FilePath -> MVar States -> ListZipper State -> ShakeConfig -> MVar ShakeConfig -> Chan.Chan String -> UI Element
photographerSection root stateFile states'' states config config' importText = do
        x <- liftIO $ withMVar config' $ (\conf -> getPhotographers conf)

        (_, importer) <- mkFilePicker "photographerPicker" "Vælg import fil" $ \file -> do
                res <- liftIO $ try $ withMVar config' $ (\conf -> importPhotographers conf file) :: IO (Either SomeException ())
                liftIO $ case res of
                            Left _ ->  Chan.writeChan importText "Kunne ikke importere denne fil"

                            Right x -> return ()

        case x of
            NoPhotographers -> do

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Fotograf ikke valgt - importer fil" ]
                                    , mkColumn ["is-12"] [ element importer ]
                                    ]
                              ] 

            Photographers y -> do
                    let group = RadioGroup 
                            { action = \xx _ -> do
                                    liftIO $ withMVar config' $ (\conf -> setPhotographers conf $ Photographers xx)
                            , view' = \xx -> UI.string (name (focus xx))
                            , title' = "photographers"
                            , items = y 
                            }

                    select <- mkRadioGroup group

                    (buttonForward, forwardView) <- mkButton "next" "Ok"
                    on UI.click buttonForward $ \_ -> liftIO $ withMVar states'' $ (\_ ->  interpret $ setStates (mkFP root stateFile) (States (forward states)))

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Fotograf" # set (attr "id") "photographerOK" ]
                                    , mkColumn ["is-12"] [ element select ]
                                    , mkColumn ["is-12"] [ element importer ]
                                    , mkColumn ["is-12"] [ element forwardView ]
                                    ]
                              ] 
