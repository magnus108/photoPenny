{-# LANGUAGE OverloadedStrings #-}
module Shooting
    ( mkRadioGroup
    , shootingSection
    -- ups
    , RadioGroup(..)
    ) where


import PhotoShake.Shooting

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import Utils.ListZipper 
import Utils.Comonad

import PhotoShake.ShakeConfig

import State (State, States(..), setStates)

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue

shootingSection :: FilePath -> FilePath -> TBMQueue States -> ListZipper State -> ShakeConfig -> UI Element
shootingSection root stateFile queue states config = do

        x <- liftIO $ getShootings config

        (_, importer) <- mkFilePicker "shootingPicker" "Vælg import fil" $ \file -> do
            liftIO $ importShootings config file

        case x of
            NoShootings-> do

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
                    on UI.click buttonForward $ \_ -> liftIO $ setStates queue (States (forward states))

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
