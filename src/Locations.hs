{-# LANGUAGE OverloadedStrings #-}
module Locations
    ( locationsSection 
    , locationsOverview
    ) where

import Control.Monad
import Control.Concurrent.MVar

import Data.List

import Control.Exception
import Elements
import PhotoShake.Built
import Prelude hiding (readFile, writeFile)
import Data.ByteString.Lazy (writeFile)
import Data.IORef

import Data.Csv

import PhotoShake.Photographee
import PhotoShake.ShakeConfig
import PhotoShake.Location
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (empty)

import Utils.Comonad
import Utils.ListZipper
import State (State, States(..), setStates)

locationsOverview :: FilePath -> FilePath -> ShakeConfig -> MVar ShakeConfig -> UI Element
locationsOverview stateFile states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getLocationFile conf)
    
    case x of
        NoLocation -> 
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Lokations fil" ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text "Ikke valgt" ]
                            ]
                      ] 

        Location y -> do
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Lokations mappe" # set (attr "id") "locationOK" ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y]
                            ]
                      ] 


locationsSection :: FilePath -> FilePath -> MVar States -> ListZipper State -> ShakeConfig -> MVar ShakeConfig -> UI Element
locationsSection root stateFile states'' states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getLocationFile conf)

    (_, view) <- mkFilePicker "locationsPicker" "Vælg eksisterende CSV fil" $ \file -> do
        liftIO $ withMVar config' $ (\conf -> setLocation conf $ Location file)

    (_, view2) <- mkFileMaker "locationsPicker" "Ny CSV" $ \file -> do
        let empty = mempty :: [Photographee]
        _ <- liftIO $ writeFile file (encode empty)
        liftIO $ withMVar config' $ (\conf -> setLocation conf $ Location file)

    case x of
        NoLocation -> 
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Lokations fil ikke valgt" ]
                            , mkColumn ["is-12"] [ UI.div #. "field is-grouped" #+ [element view, element view2]]
                            ]
                      ] 

        Location y -> do
            --dumt
            built <- liftIO $ withMVar config' $ (\conf -> getBuilt conf)
            let isBuilding = case built of
                                NoBuilt -> False
                                NoFind s -> False
                                Building _ _ -> True
                                Built _ _ -> False
            (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
            on UI.click buttonForward $ \_ -> liftIO $ withMVar states'' $ (\_ -> setStates root stateFile (States (forward states)))
            
            (buttonOpen, openView) <- mkButton "open" "Åben csv"
            on UI.click buttonOpen $ \_ -> do 
                    runFunction $ ffi $ "require('electron').shell.openItem(" ++ (show y) ++ ")"

            --DUMT
            grade <- liftIO $ newIORef ""
            gradeInput <- UI.input #. "input" # set UI.type_ "text" 
            gradeInput' <- if (not isBuilding) then return gradeInput else (element gradeInput) # set (attr "disabled") ""

            gradesr <- liftIO $ withMVar config' $ (\conf -> getGrades conf)

            identKinderClass <- case gradesr of 
                        NoGrades -> liftIO $ newIORef "Ingen valg"
                        Grades (ListZipper _ x _) ->
                                liftIO $ newIORef x

            insertedMsg <- case gradesr of 
                    NoGrades -> do
                        UI.div #. "field" #+
                                [ UI.label #. "label has-text-info" # set UI.text "Ingen stuer/klasser"
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
                                [ UI.label #. "label has-text-info" # set UI.text "Stue"
                                , UI.div # set (attr "style") "width:100%" #. "select" #+ [ element inputKinderClass' ] 
                                ]

                            on UI.selectionChange inputKinderClass' $ \xxxx -> do
                                case xxxx of
                                    Nothing -> error "this is bad"
                                    Just n -> do
                                        let val = toList zipper !! n
                                        liftIO $  writeIORef identKinderClass val

                            return inputViewKinderClass'
            
            --- SUPER BADNESS

            (gradeInsert, gradeInsertView) <- mkButton "insert" "Tilføj"
            (gradeDelete, gradeDeletetView) <- mkButton "delete" "Slet klasser"

            inputViewGrade <- UI.div #. "field" #+
                [ UI.label #. "label has-text-info" # set UI.text "Stue/Klasser"
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


            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Lokations mappe" # set (attr "id") "locationOK" ]
                            , mkColumn ["is-12"] [ UI.div #. "field is-grouped" #+ [element view, element view2]]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y]
                            , mkColumn ["is-12"] [ element openView ]
                            , mkColumn ["is-4"] [ element inputViewGrade ]
                            , mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ] 


