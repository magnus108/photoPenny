{-# LANGUAGE OverloadedStrings #-}
module DoneshootingBackup
    ( doneshootingBackupSection 
    , doneshootingBackupOverview
    ) where

import Elements
import PhotoShake.Doneshooting

import Control.Concurrent.MVar

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import PhotoShake.ShakeConfig

import Utils.ListZipper
import Utils.FP
import PhotoShake.State (State, States(..), setStates)

import Utils.Actions


doneshootingBackupOverview :: FilePath -> FilePath -> ShakeConfig -> MVar ShakeConfig -> UI Element
doneshootingBackupOverview stateFile states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getDoneshootingBackup conf)
    
    doneshooting (mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Doneshooting backup mappe" # set (attr "id") "doneshootingOK" ]
                            ]
                      ]
                 ) 
                 (\ y -> do
                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Doneshooting backup mappe" ]
                                    , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                                    ]
                              ] 
                ) x


doneshootingBackupSection :: FilePath -> FilePath -> MVar States -> ListZipper State -> ShakeConfig -> MVar ShakeConfig -> UI Element
doneshootingBackupSection root stateFile states'' states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getDoneshootingBackup conf)

    (_, view) <- mkFolderPicker "doneshotingPicker" "Vælg config folder" $ \folder -> do
            liftIO $ withMVar config' $ (\conf -> setDoneshootingBackup conf $ yesDoneshooting folder)

    doneshooting ( mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Doneshooting backup mappe" # set (attr "id") "doneshootingBackupOK" ]
                            , mkColumn ["is-12"] [ element view ]
                            ]
                    ]) (\y -> do
                        (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
                        on UI.click buttonForward $ \_ -> liftIO $ withMVar states'' $ (\_ ->  interpret $ setStates (mkFP root stateFile) (States (forward states)))

                        mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Doneshooting backup mappe" ]
                                        , mkColumn ["is-12"] [ element view ]
                                        , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                                        , mkColumn ["is-12"] [ element forwardView ]
                                        ]
                                  ] 
                    ) x
