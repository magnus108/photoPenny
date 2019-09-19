{-# LANGUAGE OverloadedStrings #-}
module Locations
    ( locationSection 
    ) where

import Control.Concurrent.MVar

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import qualified Message as Msg

import Control.Monad 
import Data.List 

import Elements

import qualified PhotoShake.Location as Location


import Utils.Comonad
import qualified Utils.ListZipper as Zipper

import PhotoShake.Photographee
import PhotoShake.Built
import PhotoShake.Dagsdato

import qualified PhotoShake.Grade as Grade

import Debug.Trace 


locationSection :: Chan Msg.Message -> Location.Location -> Grade.Grades -> UI Element
locationSection msgs x grades = do 

    (_, picker) <- mkFilePicker "locationsPicker" "Vælg eksisterende CSV fil" $ \file -> do
        liftIO $ Chan.writeChan msgs $ Msg.setLocation $ Location.yesLocation file

    (_, picker2) <- mkFileMaker "locationsPicker" "Ny CSV" $ \file -> do -- wauw
        liftIO $ Chan.writeChan msgs $ Msg.setLocation $ Location.yesLocation file
    
    pickers <- UI.div #. "field is-grouped" #+ [element picker, element picker2]

    Location.location ( mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Lokations fil ikke valgt" # set (attr "id") "locationMissing" ]
                            , element pickers
                            ]
                      ] ) 
        (\ y -> do
            (buttonOpen, openView) <- mkButton "open" "Åben csv"
            on UI.click buttonOpen $ \_ -> do 
                    runFunction $ ffi $ "require('electron').shell.openItem(" ++ (show y) ++ ")"

            gradesView <- Grade.grades (UI.div #. "field" #+
                                [ UI.label #. "label has-text-dark" # set UI.text "Ingen stuer/klasser"
                                , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                        [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                        ]
                                ])

                        (\(Zipper.ListZipper ls y rs) -> do
                                    -- i dont need this if i just make sure the
                                    -- list is sorted on the type level
                                    let zipper = Zipper.ListZipper (reverse (sort ls)) y (sort rs)
                                    input <- UI.select # set (attr "style") "width:100%" # set (attr "id") "inputter"
                                    
                                    --hack create extendI
                                    gradeViews <- sequence $ Zipper.iextend (\ i z -> do
                                                        opt <- UI.option # set (attr "value") (extract z) # set text (extract z)
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
                                            Just n -> do
                                                runFunction $ ffi "$('#inputter').trigger(%1)" ("t"++(show n))

                                    (gradeInsert, gradeInsertView) <- mkButton "insert" "Tilføj ny"

                                    on UI.click gradeInsert $ \_ -> do 
                                            liftIO $ Chan.writeChan msgs $ Msg.setGrades $ Grade.yesGrades $ Zipper.insert zipper mempty 

                                    (gradeDelete, gradeDeletetView) <- mkButton "delete" "Slet klasser"

                                    on UI.click gradeDelete $ \_ -> do 
                                            liftIO $ Chan.writeChan msgs $ Msg.setGrades $ Grade.noGrades 
                                    

                                    inputView <- UI.div #. "field" #+
                                        [ UI.div # set (attr "style") "width:100%" #. "select" #+ [ element input ] 
                                        ]


                                    input2 <- UI.input #. "input" # set (attr "id") "focusGrade" #  set UI.type_ "text" # set (attr "value") (extract zipper)

                                    inputView2 <- UI.div #. "field" #+
                                        [ UI.label #. "label has-text-dark" # set UI.text "Ændre valgte"
                                        , UI.div #. "control" #+ [ element input2 ] 
                                        ]

                                    (gradeChange, gradeChangeView) <- mkButton "save" "Gem"

                                    on UI.keydown inputView2 $ \keycode -> when (keycode == 13) $ do
                                        UI.setFocus gradeChange
                                        runFunction $ ffi "$('#save').trigger('click')"
                                        return ()

                                    on UI.click gradeChange $ \_ -> do
                                        val <- get value input2
                                        -- simpel sortering vil ikke virke her
                                        liftIO $ Chan.writeChan msgs $ Msg.setGrades $ Grade.yesGrades $ Zipper.mapFocus (\focus -> val) zipper


                                    UI.div #. "field" #+
                                        [ UI.label #. "label has-text-dark" # set UI.text "Klasse/stue"
                                        , UI.div #. "control" #+ [ UI.div #. "buttons has-addons" #+ [element gradeInsert, element gradeDelete] , element inputView]
                                        , UI.br --bads
                                        , UI.div #. "control" #+ [ element inputView2, element gradeChangeView ]
                                        ]



                            ) grades
            
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Lokations mappe" # set (attr "id") "locationOK" ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y # set (attr "id") "locationPath" ]
                            , mkColumn ["is-12"] [ element pickers ]
                            , mkColumn ["is-4"] [ element gradesView ]
                            ]
                      ] 

            --- SUPER BADNESS

        {-

            inputViewGrade <- UI.div #. "field" #+
                [ UI.label #. "label has-text-dark" # set UI.text "Stue/Klasser"
                , UI.div #. "control" #+ [ element gradeInput' ] 
                , UI.br --bads
                , UI.div #. "buttons has-addons" #+ [element gradeInsert, element gradeDelete]
                , UI.br --bads
                , UI.div #+ [ element insertedMsg]
                ]

            on UI.keyup inputViewGrade $ \_ -> liftIO . writeIORef grade =<< get value gradeInput'


            on UI.keydown inputViewGrade  $ \keycode -> when (keycode == 13) $ do
                    grade' <- liftIO $ readIORef grade
                    grades <- liftIO $ withMVar config' $ (\conf -> getGrades conf)
                    case grades of
                        NoGrades ->
                                  liftIO $ withMVar config' $ (\conf -> setGrades conf $ Grades $ ListZipper [] grade' [])
                        Grades (ListZipper ls x rs) ->
                                  liftIO $ withMVar config' $ (\conf -> setGrades conf $ Grades $ ListZipper ls grade' (x:rs))

            on UI.click gradeInsert $ \_ -> do 
                    grade' <- liftIO $ readIORef grade
                    grades <- liftIO $ withMVar config' $ (\conf -> getGrades conf)
                    case grades of
                        NoGrades ->
                                 liftIO $ withMVar config' $ (\conf -> setGrades conf $ Grades $ ListZipper [] grade' [])
                        Grades (ListZipper ls x rs) ->
                                 --dette er en insert
                                 liftIO $ withMVar config' $ (\conf -> setGrades conf $ Grades $ ListZipper ls grade' (x:rs))
                                

            
            on UI.click gradeDelete $ \_ -> do 
                    liftIO $ withMVar config' $ (\conf -> setGrades conf NoGrades)
            -}


            ) x
