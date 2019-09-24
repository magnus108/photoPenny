{-# LANGUAGE OverloadedStrings #-}
module Dump
    ( dumpSection 
    ) where

import Control.Monad 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements
import Menu


import qualified Message as Msg
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan 

import qualified PhotoShake.Dump as Dump
import qualified PhotoShake.State as State
import qualified Utils.ListZipper as ListZipper



dumpSection :: Element -> Chan Msg.Message -> ListZipper.ListZipper State.State -> Dump.Dump -> UI ()
dumpSection body msgs states dump = do
    (_, picker) <- mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder -> when (folder /= "") $ do
        liftIO $ Chan.writeChan msgs $ Msg.setDump  $ Dump.yesDump folder


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

    menu <- mkMenu msgs states 
    view <- Dump.dump (element with) without dump

    element body # set children [menu, view]

    return ()
