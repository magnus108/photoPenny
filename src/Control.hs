{-# LANGUAGE OverloadedStrings #-}
module Control
    ( controlSection
    ) where

import Data.List

import Control.Concurrent.MVar

import qualified Control.Concurrent.Chan as Chan
import qualified Message as Msg
import Control.Exception

import PhotoShake.Control
import PhotoShake.Photographee

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements
import Menu

import qualified Utils.ListZipper as Zipper
import Utils.Comonad

import PhotoShake.ShakeConfig
import PhotoShake.Build
import qualified PhotoShake.Control as Control
import qualified PhotoShake.Grade as Grade

import PhotoShake.State (State, States(..), setStates)

import Control.Monad

import qualified PhotoShake.State as State
import qualified Utils.ListZipper as ListZipper

--controlSection :: FilePath -> MVar States -> MVar ShakeConfig ->  UI Element
--controlSection root states'' config'  = do
controlSection :: Element -> Chan.Chan Msg.Message -> ListZipper.ListZipper State.State -> Grade.Grades -> Control.Result -> UI () -- måske lidt dumt med result hvis ikke der er grades
controlSection body msgs states grades result = do
    view <- Grade.grades ( UI.div #. "field" #+
                        [ UI.label #. "label has-text-dark" # set UI.text "Find elev. Der er ingen Stuer/Klasser"
                        , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                ]
                        ]) (\(Zipper.ListZipper ls y rs) -> do
                                    -- list is sorted on the type level
                                    let zipper = Zipper.sorted (ls ++ rs) (Zipper.ListZipper [] y [])

                                    input <- UI.select # set (attr "style") "width:100%" # set (attr "id") "inputter"
                                    
                                    --hack create extendI
                                    gradeViews <- sequence $ Zipper.iextend (\ i z -> do
                                                        opt <- UI.option # set (attr "value") (extract z) # set (attr "id") (extract z) # set text (extract z)
                                                        opt' <- if (z == zipper) then
                                                                element opt # set (UI.attr "selected") "" # set (UI.attr "id") "selected"
                                                            else
                                                                return opt
 
                                                        let name = ("t" ++ (show i))
                                                        runFunction $ ffi "new CustomEvent(%1,{})" name
                                                        onEvent (domEvent name input) $ \x -> do
                                                                liftIO $ Chan.writeChan msgs $ Msg.setGrades $ Grade.yesGrades z 

                                                        return opt
                                                    ) zipper

                                    _ <- element input # set children (Zipper.toList gradeViews)

                                    on UI.selectionChange input $ \ i -> do
                                        case i of
                                            Nothing -> return ()
                                            Just n -> when (length ls /= n) $ do
                                                runFunction $ ffi "$('#inputter').trigger(%1)" ("t"++(show n))

                                    inputView <- UI.div #. "field" #+
                                        [ UI.div # set (attr "style") "width:100%" #. "select" #+ [ element input ] 
                                        ]


                                    -- sæt lytter på nuværende klasse.. dog i
                                    -- det lange vil man måske ænske valgt for
                                    -- alle klasser?

                                    -- lytter kan virke således at den altid er
                                    -- tilsluttet men kun aktiv hvis state er
                                    -- korret og der er en grade.

                                    control <- case result of 
                                        NoErrors ->
                                            UI.div #+ [ mkLabel "Control"
                                                      , string "xmp tjek er ok"
                                                      ]
                                        Empty -> UI.div #+ 
                                                    [ mkLabel "Control"
                                                    , string "Ingen xmp"
                                                    ]
                                        Errors xs -> do
                                            UI.div #+ (map (\(sys, antal, five, one) ->
                                                    UI.div #+ 
                                                        [ mkLabel sys # set (attr "id") sys
                                                        , if antal then UI.p #. "has-text-success" #+ [string "antal xmp ok"] else UI.p #. "has-text-danger" #+ [string "antal xmp ikke ok"]
                                                        , if five then UI.p #. "has-text-success" #+ [string "antal 5 stjerner ok"] else UI.p #. "has-text-danger" #+ [string "antal 5 stjerner ikke ok"]
                                                        , if one then UI.p #. "has-text-success" #+ [string "antal 1 stjerner ok"] else UI.p #. "has-text-danger" #+ [string "antal 1 stjerner ikke ok"]
                                                        ]
                                                    ) xs)



                                    UI.div #. "field" #+
                                        [ UI.label #. "label has-text-dark" # set UI.text "Klasse/stue"
                                        , UI.div #. "control" #+ [ element inputView ]
                                        , UI.br
                                        , element control
                                        ]
                        ) grades

    view <- mkSection [ mkColumns ["is-multiline"]
                    [ mkColumn ["is-12"] [ mkLabel "Kontrol" # set (attr "id") "kontrolOK" ]
                    , mkColumn ["is-4"] [ element view ]
                    ]
              ] 

    menu <- mkMenu msgs states 

    element body # set children [menu, view]

    return () 
