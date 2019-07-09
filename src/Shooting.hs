{-# LANGUAGE OverloadedStrings #-}
module Shooting
    ( mkRadioGroup
    , shootingSection
    , shootingOverview
    -- ups
    , RadioGroup(..)
    ) where

import qualified Control.Concurrent.Chan as Chan
import Control.Exception

import PhotoShake.Shooting

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import Utils.ListZipper 
import Utils.Comonad

import PhotoShake.ShakeConfig

import State (State, States(..), setStates)

shootingOverview :: FilePath -> FilePath -> ShakeConfig -> UI Element
shootingOverview stateFile states config = do
        x <- liftIO $ getShootings config

        case x of
            NoShootings-> do

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Shooting ikke valgt" ]
                                    ]
                              ] 

            Shootings y -> do
                    let toString = (\xx -> case xx of
                                Normal -> "Normal"
                                ReShoot -> "Genskydning"
                            ) (focus y)

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Shooting type" # set (attr "id") "shootingOK" ]
                                    , mkColumn ["is-12"] [ UI.p # set UI.text toString ]
                                    ]
                              ] 


shootingSection :: FilePath -> FilePath -> ListZipper State -> ShakeConfig -> Chan.Chan String -> UI Element
shootingSection root stateFile states config importText = do
        x <- liftIO $ getShootings config

        (_, importer) <- mkFilePicker "shootingPicker" "Vælg import fil" $ \file -> do
            res <- liftIO $ try $ importShootings config file :: IO (Either SomeException ())
            liftIO $ case res of
                        Left _ ->  Chan.writeChan importText "Kunne ikke importere denne fil"

                        Right x -> return ()

        case x of
                NoShootings -> do

                        mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Shooting ikke valgt" ]
                                        , mkColumn ["is-12"] [ element importer ]
                                        ]
                                  ] 

                Shootings y -> do

                        let group = RadioGroup 
                                { action = \xx _ -> do
                                        liftIO $ setShooting config $ Shootings xx
                                , view' = \xx -> UI.string (show (focus xx))
                                , title' = "shootings"
                                , items = y
                                }

                        select <- mkRadioGroup group

                        (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
                        on UI.click buttonForward $ \_ -> liftIO $ setStates root stateFile (States (forward states))

                        mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Shooting type" # set (attr "id") "shootingOK" ]
                                        , mkColumn ["is-12"] [ element select]
                                        , mkColumn ["is-12"] [ element importer ]
                                        , mkColumn ["is-12"] [ element forwardView ]
                                        ]
                                  ] 




-- MOVE MEEE
mkRadioGroup :: Eq a => RadioGroup a -> UI Element
mkRadioGroup x = do
    let widgets = extend 
            (\zipper -> do
                    input' <- UI.input # set UI.type_ "radio" # set UI.name (title' x) 
                    input <- if (zipper == (items x)) then
                            set (UI.attr "checked") "" (element input')
                        else
                            return input'
                        
                    on UI.checkedChange input ((action x) zipper)
                    label <- (view' x) zipper  
                    view <- UI.label #. "radio" #+ [element input, element label]
                    return view
            ) (items x)

    view <- UI.div #. "control" #+ (toList widgets)
    return view


data RadioGroup a = RadioGroup
    { action :: ListZipper a -> Bool -> UI ()
    , view' :: ListZipper a -> UI Element
    , title' :: String
    , items :: ListZipper a
    }
