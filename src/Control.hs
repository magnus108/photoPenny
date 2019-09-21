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

import qualified Utils.ListZipper as Zipper
import Utils.Comonad

import PhotoShake.ShakeConfig
import PhotoShake.Built
import qualified PhotoShake.Grade as Grade

import PhotoShake.State (State, States(..), setStates)

import Control.Monad

--controlSection :: FilePath -> MVar States -> MVar ShakeConfig ->  UI Element
--controlSection root states'' config'  = do
controlSection :: Chan.Chan Msg.Message -> Grade.Grades -> UI Element
controlSection msgs grades = do
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

                                    inputView <- UI.div #. "field" #+
                                        [ UI.div # set (attr "style") "width:100%" #. "select" #+ [ element input ] 
                                        ]


                                    -- sæt lytter på nuværende klasse.. dog i
                                    -- det lange vil man måske ænske valgt for
                                    -- alle klasser?
                                    
                                   -- _ <- writeChan msgs Msg.getDump 
                                   -- _ <- setupDumpListener manager msgs app

                                   -- x <- liftIO $ withMVar config' $ (\conf -> controlXMP conf y)
{-
                                    case x of 
                                        NoErrors ->
                                            mkSection [ mkColumns ["is-multiline"]
                                                    [ mkColumn ["is-12"] [ mkLabel "Control" ]
                                                    , mkColumn ["is-12"] [ string "xmp tjek er ok"]
                                                    ]
                                                ]
                                        Empty ->
                                            mkSection [ mkColumns ["is-multiline"]
                                                    [ mkColumn ["is-12"] [ mkLabel "Control" ]
                                                    , mkColumn ["is-12"] [ string "Ingen xmp"]
                                                    ]
                                                ]
                                        Errors xs -> do
                                            contets <- mapM (\(sys, antal, five, one) -> 
                                                 mkSection [ mkColumns ["is-multiline"]
                                                        [ mkColumn ["is-12"] [ mkLabel sys ]
                                                        , mkColumn ["is-12"] [ if antal then UI.p #. "has-text-success" #+ [string "antal xmp ok"] else UI.p #. "has-text-danger" #+ [string "antal xmp ikke ok"]]
                                                        , mkColumn ["is-12"] [ if five then UI.p #. "has-text-success" #+ [string "antal 5 stjerner ok"] else UI.p #. "has-text-danger" #+ [string "antal 5 stjerner ikke ok"]]
                                                        , mkColumn ["is-12"] [ if one then UI.p #. "has-text-success" #+ [string "antal 1 stjerner ok"] else UI.p #. "has-text-danger" #+ [string "antal 1 stjerner ikke ok"]]
                                                        ]
                                                    ]
                                                ) xs

                                             UI.div #+ (fmap element contets)) grades
-}
                                    UI.div #. "field" #+
                                        [ UI.label #. "label has-text-dark" # set UI.text "Klasse/stue"
                                        , UI.div #. "control" #+ [ element inputView ]
                                        ]
                        ) grades

    mkSection [ mkColumns ["is-multiline"]
                    [ mkColumn ["is-12"] [ mkLabel "Kontrol" # set (attr "id") "kontrolOK" ]
                    , mkColumn ["is-4"] [ element view ]
                    ]
              ] 
    
{-
    grades <- liftIO $ withMVar config' $ (\conf -> getGrades conf)
    built <- liftIO $ withMVar config' $ (\conf -> getBuilt conf)

    let isBuilding = case built of
                            NoBuilt -> False
                            NoFind s -> False
                            Building _ _ -> True
                            Built _ _ -> False
    gradeSelection <- liftIO $ withMVar config' $ (\conf -> getGradeSelection conf)

    inputViewKinderClasssCopy <- 
        Grade.grades ( UI.div #. "field" #+
                        [ UI.label #. "label has-text-dark" # set UI.text "Find elev. Der er ingen stuer/klasser"
                        , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                ]
                        ])
                    (\zipper -> do
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
                                [ UI.label #. "label has-text-dark" # set UI.text "Find elev"
                                , UI.div # set (attr "style") "width:100%" #. "select" #+ [ element inputKinderClass' ] 
                                ]

                            on UI.selectionChange inputKinderClass' $ \xxxx -> do
                                    case xxxx of
                                        Nothing -> error "this is bad"
                                        Just n -> do
                                            let val = (sort $ toList zipper) !! n
                                            if gradeSelection == (Grade.yesGradeSelection val) then
                                                    return ()
                                            else
                                                liftIO $ withMVar config' $ (\conf -> do
                                                        liftIO $ setGradeSelection conf (Grade.yesGradeSelection val)
                                                        liftIO $ setGrades conf (Grade.yesGrades $ ListZipper [] val (sort $ toList zipper))
                                                )
                

                            return inputViewKinderClass') grades

    errs <- Grade.grades (mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Control" ]
                                        , mkColumn ["is-12"] [ string "Der er ikke valgt en klasse"]
                                        ]
                                    ])
                    (\(ListZipper _ grade _ ) -> do
                        x <- liftIO $ withMVar config' $ (\conf -> controlXMP conf grade)
                        case x of 
                            NoErrors ->
                                mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Control" ]
                                        , mkColumn ["is-12"] [ string "xmp tjek er ok"]
                                        ]
                                    ]
                            Empty ->
                                mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Control" ]
                                        , mkColumn ["is-12"] [ string "Ingen xmp"]
                                        ]
                                    ]
                            Errors xs -> do
                                contets <- mapM (\(sys, antal, five, one) -> 
                                     mkSection [ mkColumns ["is-multiline"]
                                            [ mkColumn ["is-12"] [ mkLabel sys ]
                                            , mkColumn ["is-12"] [ if antal then UI.p #. "has-text-success" #+ [string "antal xmp ok"] else UI.p #. "has-text-danger" #+ [string "antal xmp ikke ok"]]
                                            , mkColumn ["is-12"] [ if five then UI.p #. "has-text-success" #+ [string "antal 5 stjerner ok"] else UI.p #. "has-text-danger" #+ [string "antal 5 stjerner ikke ok"]]
                                            , mkColumn ["is-12"] [ if one then UI.p #. "has-text-success" #+ [string "antal 1 stjerner ok"] else UI.p #. "has-text-danger" #+ [string "antal 1 stjerner ikke ok"]]
                                            ]
                                        ]
                                    ) xs

                                UI.div #+ (fmap element contets)) grades

    UI.div #+ [mkSection [ mkColumns ["is-multiline"]
                                            [ mkColumn ["is-12"] [ element inputViewKinderClasssCopy]]], element errs]
 -}                                   
