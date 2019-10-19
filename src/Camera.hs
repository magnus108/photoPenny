{-# LANGUAGE OverloadedStrings #-}
module Camera
    ( cameraSection
    ) where


import Control.Monad 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Shooting
import Elements
import Menu

import qualified Message as Msg
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan 

import qualified PhotoShake.Camera as Camera

import Utils.FP
import qualified PhotoShake.State as State
import qualified Utils.ListZipper as ListZipper
import Utils.Actions
import Utils.Comonad


cameraSection :: Element -> Chan Msg.Message -> ListZipper.ListZipper State.State -> Camera.Cameras -> UI ()
cameraSection body msgs states cameras = do
    (_, picker) <- mkFilePicker "cameraPicker" "Vælg import fil" $ \file -> when (file /= "") $ do
        cameras <- liftIO $ interpret $ Camera.getCameras $ fp $ start $ file
        liftIO $ Chan.writeChan msgs $ Msg.setCameras cameras

    view <- Camera.cameras (mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Camera ikke valgt" # set (attr "id") "cameraMissing" ]
                                        , mkColumn ["is-12"] [ element picker ]
                                        ]
                                  ] )
                (\y -> do
                        let group = RadioGroup 
                                { action = \xxx _ -> do
                                        liftIO $ Chan.writeChan msgs $ Msg.setCameras $ Camera.yesCameras xxx
                                , view' = \xx -> UI.string (show (ListZipper.focus xx))
                                , title' = "camera"
                                , items = y
                                }

                        select <- mkRadioGroup group

                        mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Camera type" # set (attr "id") "cameraOK" ]
                                        , mkColumn ["is-12"] [ element select]
                                        , mkColumn ["is-12"] [ element picker ]
                                        ]
                                  ] ) cameras

    menu <- mkMenu msgs states 

    element body # set children [menu, view]

    return () 
