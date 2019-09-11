{-# LANGUAGE OverloadedStrings #-}
module Dump
    ( dumpSection 
    ) where

import Control.Monad 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements


import qualified Message as Msg
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan 

import PhotoShake.Dump


dumpSection :: Chan Msg.Message -> Dump -> UI Element
dumpSection msgs x = do
    (_, picker) <- mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder -> when (folder /= "") $ do
        liftIO $ Chan.writeChan msgs $ Msg.setDump  $ yesDump folder


    with <- mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe ikke valgt" # set (attr "id") "dumpMissing" ]
                            , mkColumn ["is-12"] [ element picker ]
                            ]
                      ] 
    

            ---(buttonForward, forwardView) <- mkButton "nextDump" "Ok"
            --on UI.click buttonForward $ \_ -> liftIO $ withMVar states'' $ (\_ ->  interpret $ setStates (mkFP root stateFile) (States (forward states)))

    let without = (\z -> mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe" # set (attr "id") "dumpOK" ]
                            , mkColumn ["is-12"] [ element picker ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text z # set (attr "id") "dumpPath" ]
                            --, mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ])

    dump (element with) without x

