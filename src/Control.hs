{-# LANGUAGE OverloadedStrings #-}
module Control
    ( controlSection
    ) where

import Control.Concurrent.MVar

import qualified Control.Concurrent.Chan as Chan
import Control.Exception

import PhotoShake.Control
import PhotoShake.Photographee

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import Utils.ListZipper 
import Utils.Comonad

import PhotoShake.ShakeConfig
import PhotoShake.Built

import State (State, States(..), setStates)


controlSection :: FilePath -> MVar States -> MVar ShakeConfig ->  UI Element
controlSection root states'' config'  = do
    grades <- liftIO $ withMVar config' $ (\conf -> getGrades conf)
    built <- liftIO $ withMVar config' $ (\conf -> getBuilt conf)

    let isBuilding = case built of
                            NoBuilt -> False
                            NoFind s -> False
                            Building _ _ -> True
                            Built _ _ -> False
    gradeSelection <- liftIO $ withMVar config' $ (\conf -> getGradeSelection conf)

    inputViewKinderClasssCopy <- case grades of 
            NoGrades -> do
                UI.div #. "field" #+
                        [ UI.label #. "label has-text-info" # set UI.text "Find elev. Der er ingen stuer/klasser"
                        , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                ]
                        ]
            Grades zipper -> do
                    inputKinderClass <- UI.select # set (attr "style") "width:100%" #+ (fmap (\x -> UI.option # set (attr "value") x # set text x) (toList zipper))
                    inputKinderClass' <- if (not isBuilding) then return inputKinderClass else (element inputKinderClass) # set (attr "disabled") ""
                    inputViewKinderClass' <- UI.div #. "field" #+
                        [ UI.label #. "label has-text-info" # set UI.text "Find elev"
                        , UI.div # set (attr "style") "width:100%" #. "select" #+ [ element inputKinderClass' ] 
                        ]

                    on UI.selectionChange inputKinderClass' $ \xxxx -> do
                            case xxxx of
                                Nothing -> error "this is bad"
                                Just n -> do
                                    let val = toList zipper !! n
                                    if gradeSelection == (GradeSelection val) then
                                            return ()
                                    else
                                        liftIO $ withMVar config' $ (\conf -> do
                                                liftIO $ setGradeSelection conf (GradeSelection val)
                                                liftIO $ setGrades conf (Grades $ ListZipper [] val (toList zipper))
                                        )
        

                    return inputViewKinderClass'
    errs <- case grades of 
                NoGrades -> 
                            mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Control" ]
                                        , mkColumn ["is-12"] [ string "Der er ikke valgt en klasse"]
                                        ]
                                    ]
                Grades (ListZipper _ grade _) -> do
                        x <- liftIO $ withMVar config' $ (\conf -> controlXMP conf grade)
                        case x of 
                            NoErrors ->
                                mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Control" ]
                                        , mkColumn ["is-12"] [ string "xmp tjek er ok"]
                                        ]
                                    ]
                            Errors xs -> do
                                contets <- mapM (\(sys, antal, five, one) -> 
                                     mkSection [ mkColumns ["is-multiline"]
                                            [ mkColumn ["is-12"] [ mkLabel sys ]
                                            , mkColumn ["is-12"] [ string (if antal then "antal xmp ok" else "antal xmp ikke ok")]
                                            , mkColumn ["is-12"] [ string (if five then "antal 5 stjerner ok" else "antal 5 stjerner ikke ok")]
                                            , mkColumn ["is-12"] [ string (if one then "antal 1 stjerner ok" else "antal 1 stjerner ikke ok")]
                                            ]
                                        ]
                                    ) xs

                                UI.div #+ (fmap element contets)

    UI.div #+ [mkSection [ mkColumns ["is-multiline"]
                                            [ mkColumn ["is-12"] [ element inputViewKinderClasssCopy]]], element errs]
                                    