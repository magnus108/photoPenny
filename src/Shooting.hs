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
import Menu

import qualified Message as Msg
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan 

import qualified PhotoShake.Shooting as Shooting

import Utils.FP
import qualified PhotoShake.State as State
import qualified Utils.ListZipper as ListZipper
import Utils.Actions
import Utils.Comonad


shootingSection :: Element -> Chan Msg.Message -> ListZipper.ListZipper State.State -> Shooting.Shootings -> UI ()
shootingSection body msgs states shootings = do
    (_, picker) <- mkFilePicker "shootingPicker" "Vælg import fil" $ \file -> when (file /= "") $ do
        shootings <- liftIO $ interpret $ Shooting.getShootings $ fp $ start $ file
        liftIO $ Chan.writeChan msgs $ Msg.setShootings shootings

    view <- Shooting.shootings (mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Shooting ikke valgt" # set (attr "id") "shootingMissing" ]
                                        , mkColumn ["is-12"] [ element picker ]
                                        ]
                                  ] )
                (\y -> do
                        let group = RadioGroup 
                                { action = \xxx _ -> do
                                        liftIO $ Chan.writeChan msgs $ Msg.setShootings $ Shooting.yesShootings xxx
                                , view' = \xx -> UI.string (show (ListZipper.focus xx))
                                , title' = "shootings"
                                , items = y
                                }

                        select <- mkRadioGroup group

                        mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Shooting type" # set (attr "id") "shootingOK" ]
                                        , mkColumn ["is-12"] [ element select]
                                        , mkColumn ["is-12"] [ element picker ]
                                        ]
                                  ] ) shootings

    menu <- mkMenu msgs states 

    element body # set children [menu, view]

    return () 



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

    view <- UI.div #. "control" #+ (ListZipper.toList widgets)
    return view


data RadioGroup a = RadioGroup
    { action :: ListZipper.ListZipper a -> Bool -> UI ()
    , view' :: ListZipper.ListZipper a -> UI Element
    , title' :: String
    , items :: ListZipper.ListZipper a
    }
