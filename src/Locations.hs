{-# LANGUAGE OverloadedStrings #-}
module Locations
    ( locationSection 
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import qualified Message as Msg

import Control.Monad 

import Elements

import qualified PhotoShake.Location as Location


import Utils.ListZipper

import PhotoShake.Photographee
import PhotoShake.Built


locationSection :: Chan Msg.Message -> Location.Location -> UI Element
locationSection msgs x = do

    (_, picker) <- mkFilePicker "locationsPicker" "Vælg eksisterende CSV fil" $ \file -> do
        liftIO $ Chan.writeChan msgs $ Msg.setLocation $ Location.yesLocation file

    (_, picker2) <- mkFileMaker "locationsPicker" "Ny CSV" $ \file -> do -- wauw
        liftIO $ Chan.writeChan msgs $ Msg.setLocation $ Location.yesLocation file
    
    pickers <- mkColumn ["is-12"] [ UI.div #. "field is-grouped" #+ [element picker, element picker2]]

    Location.location ( mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Lokations fil ikke valgt" # set (attr "id") "locationMissing" ]
                            , element pickers
                            ]
                      ] ) 
        (\ y -> do
            (buttonOpen, openView) <- mkButton "open" "Åben csv"
            on UI.click buttonOpen $ \_ -> do 
                    runFunction $ ffi $ "require('electron').shell.openItem(" ++ (show y) ++ ")"

            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Lokations mappe" # set (attr "id") "locationOK" ]
                            , mkColumn ["is-12"] [ element pickers ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y # set (attr "id") "locationPath" ]
                            ]
                      ] 


            {-
            insertedMsg <- case gradesr of 
                    NoGrades -> do
                        UI.div #. "field" #+
                                [ UI.label #. "label has-text-dark" # set UI.text "Ingen stuer/klasser"
                                , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                        [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                        ]
                                ]
                    Grades zipper -> do
                            let toto = (zipper =>> 
                                            (\z -> do
                                                opt <- UI.option # set (attr "value") (focus z) # set text (focus z)
                                                opt' <- if (z == zipper) then
                                                        set (UI.attr "selected") "" (element opt)
                                                    else
                                                        return opt
                                                return (focus z, opt')
                                            )
                                    )

                            jada <- sequence toto

                            let toto' = fmap (\(a,b) -> return b) (sortBy (\a b -> compare (fst a) (fst b)) $ toList jada)

                            inputKinderClass <- UI.select # set (attr "style") "width:100%" #+ toto'
                            inputKinderClass' <- if (not isBuilding) then return inputKinderClass else (element inputKinderClass) # set (attr "disabled") ""
                            inputViewKinderClass' <- UI.div #. "field" #+
                                [ UI.label #. "label has-text-dark" # set UI.text "Stue"
                                , UI.div # set (attr "style") "width:100%" #. "select" #+ [ element inputKinderClass' ] 
                                ]

                            on UI.selectionChange inputKinderClass' $ \xxxx -> do
                                case xxxx of
                                    Nothing -> error "this is bad"
                                    Just n -> do
                                        let val = toList zipper !! n
                                        liftIO $  writeIORef identKinderClass val

                            return inputViewKinderClass'
                            -}
            
            --- SUPER BADNESS

        {-
            (gradeInsert, gradeInsertView) <- mkButton "insert" "Tilføj"
            (gradeDelete, gradeDeletetView) <- mkButton "delete" "Slet klasser"

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
