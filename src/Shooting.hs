{-# LANGUAGE OverloadedStrings #-}
module Shooting
    ( mkRadioGroup
    , RadioGroup(..)
    , shootingSection
    ) where


import Control.Monad 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import qualified Message as Msg
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan 

import PhotoShake.Shooting

import Utils.FP
import Utils.Actions
import Utils.ListZipper
import Utils.Comonad



shootingSection :: Chan Msg.Message -> Shootings -> UI Element
shootingSection msgs x = do
    (_, picker) <- mkFilePicker "shootingPicker" "Vælg import fil" $ \file -> when (file /= "") $ do
        shootings <- liftIO $ interpret $ getShootings $ fp $ start $ file
        liftIO $ Chan.writeChan msgs $ Msg.setShootings shootings

    shootings (mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Shooting ikke valgt" # set (attr "id") "shootingMissing" ]
                                        , mkColumn ["is-12"] [ element picker ]
                                        ]
                                  ] )
                (\y -> do
                        let group = RadioGroup 
                                { action = \xxx _ -> do
                                        liftIO $ Chan.writeChan msgs $ Msg.setShootings $ yesShootings xxx
                                , view' = \xx -> UI.string (show (focus xx))
                                , title' = "shootings"
                                , items = y
                                }

                        select <- mkRadioGroup group

                        mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Shooting type" # set (attr "id") "shootingOK" ]
                                        , mkColumn ["is-12"] [ element select]
                                        , mkColumn ["is-12"] [ element picker ]
                                        ]
                                  ] ) x



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
