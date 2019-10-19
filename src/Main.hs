{-# LANGUAGE OverloadedStrings #-}
module Main
    ( mainSection 
    ) where

import Data.Maybe
import Data.List
import Data.Char
import Control.Monad

import Utils.Comonad
import Utils.Debounce
import qualified Utils.ListZipper as ListZipper

import qualified Control.Concurrent.Chan as Chan
import qualified Message as Msg

import qualified PhotoShake.Build as Build
import qualified PhotoShake.Grade as Grade
import qualified PhotoShake.Id as Id
import qualified PhotoShake.State as State
import qualified PhotoShake.Session as Session
import qualified PhotoShake.Photographee as Photographee
import PhotoShake.ShakeConfig 

import qualified Utils.ListZipper as ListZipper

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements
import Menu


mkBuild :: Chan.Chan Msg.Message -> UI (Element, Element)
mkBuild msgs = do
    (button, view) <- mkButton "mover" "Flyt filer"
    _ <- element button # set (attr "id") "builderButton"
    on UI.click button $ \_ -> do
        liftIO $ Chan.writeChan msgs $ Msg.build
    return (button, view)

-- SÅÅÅ BAD...
mkBuildKinderGarten :: Chan.Chan Msg.Message -> Session.Type -> UI Element
mkBuildKinderGarten msgs t = do
    (buttonEnkelt, view') <- mkButton "mover" "Flyt enkelt"
    let zipper1 = ListZipper.ListZipper [] (Session.kindergarten Session.group) [Session.kindergarten Session.single,Session.school]

    on UI.click buttonEnkelt $ \_ -> do
        liftIO $ Chan.writeChan msgs $ Msg.SetSessions $ Session.yesSessions zipper1
        liftIO $ Chan.writeChan msgs $ Msg.build

    (buttonGruppe, view'') <- mkButton "mover" "Flyt gruppe"
    let zipper2 = ListZipper.ListZipper [Session.kindergarten Session.group] (Session.kindergarten Session.single) [Session.school]
    on UI.click buttonGruppe $ \_ -> do
        liftIO $ Chan.writeChan msgs $ Msg.SetSessions $ Session.yesSessions zipper2
        liftIO $ Chan.writeChan msgs $ Msg.build
    
    UI.div #. "buttons has-addons" #+ [element buttonEnkelt, element buttonGruppe]

                    

mkCreate :: Chan.Chan Msg.Message -> Element -> Element -> UI (Element, Element)
mkCreate msgs id name = do
    (button, view) <- mkButton "mover" "Opret ny elev"
    on UI.click button $ \_ -> do
        id' <- get value id
        name' <- get value name
        if (id' /= "") || (name' /= "") then
            liftIO $ Chan.writeChan msgs $ Msg.insertPhotographee id' name'
        else 
            return ()
        --Photographee.insertPhotographee xxx idd clas name)

    return (button, view)

sessionMissing :: Element -> Chan.Chan Msg.Message -> ListZipper.ListZipper State.State -> UI ()
sessionMissing body msgs states = do
    view <- mkSection 
            [ mkColumns ["is-multiline"]
                [ mkColumn ["is-12"] [string "Mangler session"]
                ]
            ]

    menu <- mkMenu msgs states  
    _ <- element body # set children [menu, view]
    return ()

mainSection :: Element -> Chan.Chan Msg.Message -> ListZipper.ListZipper State.State -> Grade.Grades -> Id.Id -> DumpFiles -> Session.Sessions -> Build.Build -> Maybe Photographee.Photographee -> [Photographee.Photographee] -> UI ()
mainSection body msgs states grades id dumpFiles sessions build photographee photographees = do
    Session.sessions (sessionMissing body msgs states)
        (\y -> Session.session 
            (mainSectionKindergarten body msgs states grades id dumpFiles build photographee photographees)
            ( mainSectionSchool body msgs states grades id dumpFiles build photographee photographees) 
            (ListZipper.focus y) 
        ) sessions

mainSectionKindergarten :: Element -> Chan.Chan Msg.Message -> ListZipper.ListZipper State.State -> Grade.Grades -> Id.Id -> DumpFiles -> Build.Build -> Maybe Photographee.Photographee -> [Photographee.Photographee] -> Session.Type -> UI ()
mainSectionKindergarten body msgs states grades id dumpFiles build photographee photographees typ = do
    input <- UI.input #. "input" # set (attr "id") "fotoId" #  set UI.type_ "text" 

    action <- liftIO $ mkDebounce defaultDebounceSettings
             { debounceAction = do
                    w <- getWindow input
                    value <- runUI w (get value input)
                    when (Id.toString id /= value) $ do
                        Chan.writeChan msgs $ Msg.setId $ Id.fromString value
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    buildView <- mkBuildKinderGarten msgs typ

    on UI.keyup input $ \keycode -> do
            liftIO $ action 

    inputView <- UI.div #. "field" #+
        [ UI.label #. "label has-text-dark" # set UI.text "Foto Id"
        , UI.div #. "control" #+ [ element input ] 
        ]

 
    let dumpFilesToCount x = case x of
            DumpFiles xs ->
                show $ length $ fmap fst xs
            DumpFilesError ->
                "Cr2 og jpg stemmer ikke overens"
            NoDump ->
                "Igen dump mappe"

    let dumpFilesCount x = case x of
            DumpFilesError -> 0
            NoDump -> 0
            DumpFiles xs -> length $ fmap fst xs

    label <- mkLabel "Antal billeder i dump:"

    dumpSize <- mkColumn ["is-12"] 
                    [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-12"] [ element label ]
                        , mkColumn ["is-12"] [ UI.string (dumpFilesToCount dumpFiles) #. "is-size-1 has-text-danger has-text-weight-bold" # set (attr "id") "count"]
                        ]
                    ]

    grade <- mkColumn ["is-12"] 
        [ Grade.grades ( UI.div #. "field" #+
                        [ UI.label #. "label has-text-dark" # set UI.text "Find elev. Der er ingen Stuer/Klasser"
                        , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                ]
                        ]) (\(ListZipper.ListZipper ls y rs) -> do
                                    -- list is sorted on the type level
                                    let zipper = ListZipper.sorted (ls ++ rs) (ListZipper.ListZipper [] y [])

                                    input <- UI.select # set (attr "style") "width:100%" # set (attr "id") "inputter"
                                    input2 <- UI.select # set (attr "style") "width:100%" # set (attr "id") "inputter2"

                                    gradeViews2 <- sequence $ ListZipper.iextend (\ i z -> do
                                                        opt <- UI.option # set (attr "value") (extract z) # set (attr "id") (extract z) # set text (extract z)
                                                        opt' <- if (z == zipper) then
                                                                element opt # set (UI.attr "selected") "" # set (UI.attr "id") "selected"
                                                            else
                                                                return opt
 
                                                        let name = ("w" ++ (show i))
                                                        runFunction $ ffi "new CustomEvent(%1,{})" name
                                                        onEvent (domEvent name input2) $ \x -> do
                                                                liftIO $ Chan.writeChan msgs $ Msg.setGrades $ Grade.yesGrades z 

                                                        return opt
                                                    ) zipper

                                    _ <- element input2 # set children (ListZipper.toList gradeViews2)
                                    
                                    --hack create extendI
                                    gradeViews <- sequence $ ListZipper.iextend (\ i z -> do
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

                                    _ <- element input # set children (ListZipper.toList gradeViews)

                                    on UI.selectionChange input2 $ \ i -> do
                                        case i of
                                            Nothing -> return ()
                                            Just n -> when (length ls /= n) $ do
                                                runFunction $ ffi "$('#inputter2').trigger(%1)" ("w"++(show n))

                                    on UI.selectionChange input $ \ i -> do
                                        case i of
                                            Nothing -> return ()
                                            Just n -> when (length ls /= n) $ do
                                                runFunction $ ffi "$('#inputter').trigger(%1)" ("t"++(show n))

                                    inputView <- UI.div #. "field" #+
                                        [ UI.div # set (attr "style") "width:100%" #. "select" #+ [ element input ] 
                                        ]


                                    inputView2 <- UI.div #. "field" #+
                                        [ UI.div # set (attr "style") "width:100%" #. "select" #+ [ element input2 ]
                                        ]


                                    let photographeesUI = fmap (\c -> do
                                                let ident = Photographee._ident c
                                                let name = Photographee._name c
                                                let s = name ++ ", " ++ ident
                                                (button, buttonView) <- mkButton ident s
                                                on UI.click button $ \_ -> do 
                                                    liftIO $ Chan.writeChan msgs $ Msg.setId $ Id.fromString ident
                                                mkColumn ["is-12"] [ element buttonView ]
                                            ) (sortOn (Photographee._name) photographees)


                                    inputId <- UI.input #. "input" # set UI.type_ "text" 
                                    inputIdView <- UI.div #. "field" #+
                                        [ UI.label #. "label has-text-dark" # set UI.text "Nummer"
                                        , UI.div #. "control" #+ [ element inputId ] 
                                        ]


                                    inputName <- UI.input #. "input" # set UI.type_ "text" 
                                    inputNameView <- UI.div #. "field" #+
                                        [ UI.label #. "label has-text-dark" # set UI.text "Navn"
                                        , UI.div #. "control" #+ [ element inputName ] 
                                        ]

                                    (buttonAlt, buttonAltView) <- mkCreate msgs inputId inputName
                                    

                                    UI.div #+
                                        [ UI.br
                                        , UI.label #. "label has-text-dark" # set UI.text "Opret"
                                        , UI.label #. "label has-text-dark" # set UI.text "Klasse/stue"
                                        , mkColumns ["is-multiline"] 
                                            [ mkColumn ["is-4"] 
                                                [ UI.div #. "control" #+ [ element inputView
                                                                        , element inputIdView
                                                                        , element inputNameView
                                                                        , element buttonAltView
                                                                        ]
                                                ]
                                            ]
                                        , UI.br
                                        , UI.label #. "label has-text-dark" # set UI.text "Find elev"
                                        , mkColumns ["is-multiline"]
                                            ([ mkColumn ["is-4"]
                                                [ element inputView2 ]
                                            ] ++ photographeesUI )
                                        ]
                            ) grades ]


    view <- maybe 
        ( mkSection 
            [ mkColumns ["is-multiline"]
                [ mkColumn ["is-12"]
                    [ mkColumns ["is-multiline"] 
                        [ mkColumn ["is-4"] [ element inputView ]
                        ]
                    ]
                , element dumpSize
                , element grade
                ]
            ]) (\ x -> 
        if (dumpFilesCount dumpFiles) == 0 then 
            ( mkSection 
                [ mkColumns ["is-multiline"]
                    [ mkColumn ["is-12"]
                        [ mkColumns ["is-multiline"] 
                            [ mkColumn ["is-4"] [ element inputView ]
                            , mkColumn ["is-12"] [ UI.p #. "is-size-3" #+ [string ("Navn: " ++ (Photographee._name x))] ]
                            ]
                        ]
                    , element dumpSize
                    , element grade
                    ]
                ])
        else
            mkSection 
                [ mkColumns ["is-multiline"]
                    [ mkColumn ["is-12"]
                        [ mkColumns ["is-multiline"] 
                            [ mkColumn ["is-4"] [ element inputView ]
                            , mkColumn ["is-12"] [ element buildView ]
                            , mkColumn ["is-12"] [ UI.p #. "is-size-3" #+ [string ("Navn: " ++ (Photographee._name x))] ]
                            ]
                        ]
                    , element dumpSize
                    , element grade
                    ]
                ]) photographee

    let view2 = \ s -> maybe ( mkSection 
                [ mkColumns ["is-multiline"]
                    [ mkColumn ["is-12"]
                        [ mkColumns ["is-multiline"] 
                            [ mkColumn ["is-4"] [ element inputView ]
                            ]
                        ]
                    , mkColumn ["is-12"] [UI.p # set text s # set (attr "id") "result" ]
                    , element dumpSize
                    , element grade
                    ]
                ]) (\ x -> 
                    if (dumpFilesCount dumpFiles) == 0 then 
                        ( mkSection 
                            [ mkColumns ["is-multiline"]
                                [ mkColumn ["is-12"]
                                    [ mkColumns ["is-multiline"] 
                                        [ mkColumn ["is-4"] [ element inputView ]
                                        , mkColumn ["is-12"] [ UI.p #. "is-size-3" #+ [string ("Navn: " ++ (Photographee._name x))] ]
                                        ]
                                    ]
                                , mkColumn ["is-12"] [UI.p # set text s # set (attr "id") "result" ]
                                , element dumpSize
                                , element grade
                                ]
                            ])
                    else
                        mkSection 
                            [ mkColumns ["is-multiline"]
                                [ mkColumn ["is-12"]
                                    [ mkColumns ["is-multiline"] 
                                        [ mkColumn ["is-4"] [ element inputView ]
                                        , mkColumn ["is-12"] [ element buildView ]
                                        , mkColumn ["is-12"] [ UI.p #. "is-size-3" #+ [string ("Navn: " ++ (Photographee._name x))] ]
                                        ]
                                    ]
                                , mkColumn ["is-12"] [UI.p # set text s # set (attr "id") "result" ]
                                , element dumpSize
                                , element grade
                                ]
                            ]) photographee

    buildMsg <- Build.build' (element view) (\y -> view2 y) (\p s -> view2 (Photographee._name p ++ " - " ++ s))  build

    menu <- mkMenu msgs states  
    _ <- element body # set children [menu, buildMsg]

    _ <- UI.setFocus input
    _ <- element input # set value (Id.toString id)

    return ()








mainSectionSchool :: Element -> Chan.Chan Msg.Message -> ListZipper.ListZipper State.State -> Grade.Grades -> Id.Id -> DumpFiles -> Build.Build -> Maybe Photographee.Photographee -> [Photographee.Photographee] -> UI ()
mainSectionSchool body msgs states grades id dumpFiles build photographee photographees = do
    input <- UI.input #. "input" # set (attr "id") "fotoId" #  set UI.type_ "text" 

    (builderButton, buildView) <- mkBuild msgs

    action <- liftIO $ mkDebounce defaultDebounceSettings
             { debounceAction = do
                    w <- getWindow input
                    value <- runUI w (get value input)
                    when (Id.toString id /= value) $ do
                        Chan.writeChan msgs $ Msg.setId $ Id.fromString value
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    on UI.keyup input $ \keycode -> do
            liftIO $ action

    on UI.keydown input $ \keycode -> when (keycode == 13) $ do
        UI.setFocus builderButton 
        runFunction $ ffi "$('#builderButton').trigger('click')"

    inputView <- UI.div #. "field" #+
        [ UI.label #. "label has-text-dark" # set UI.text "Foto Id"
        , UI.div #. "control" #+ [ element input ] 
        ]

 
    let dumpFilesToCount x = case x of
            DumpFiles xs ->
                show $ length $ fmap fst xs
            DumpFilesError ->
                "Cr2 og jpg stemmer ikke overens"
            NoDump ->
                "Igen dump mappe"

    let dumpFilesCount x = case x of
            DumpFilesError -> 0
            NoDump -> 0
            DumpFiles xs -> length $ fmap fst xs

    label <- mkLabel "Antal billeder i dump:"

    dumpSize <- mkColumn ["is-12"] 
                    [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-12"] [ element label ]
                        , mkColumn ["is-12"] [ UI.string (dumpFilesToCount dumpFiles) #. "is-size-1 has-text-danger has-text-weight-bold" # set (attr "id") "count"]
                        ]
                    ]

    grade <- mkColumn ["is-12"] 
        [ Grade.grades ( UI.div #. "field" #+
                        [ UI.label #. "label has-text-dark" # set UI.text "Find elev. Der er ingen Stuer/Klasser"
                        , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                ]
                        ]) (\(ListZipper.ListZipper ls y rs) -> do
                                    -- list is sorted on the type level
                                    let zipper = ListZipper.sorted (ls ++ rs) (ListZipper.ListZipper [] y [])

                                    input <- UI.select # set (attr "style") "width:100%" # set (attr "id") "inputter"
                                    input2 <- UI.select # set (attr "style") "width:100%" # set (attr "id") "inputter2"
                                    
                                    gradeViews2 <- sequence $ ListZipper.iextend (\ i z -> do
                                                        opt <- UI.option # set (attr "value") (extract z) # set text (extract z)
                                                        opt' <- if (z == zipper) then
                                                                element opt # set (UI.attr "selected") "" # set (UI.attr "id") "selected"
                                                            else
                                                                return opt
 
                                                        let name = ("w" ++ (show i))
                                                        runFunction $ ffi "new CustomEvent(%1,{})" name
                                                        onEvent (domEvent name input2) $ \x -> do
                                                                liftIO $ Chan.writeChan msgs $ Msg.setGrades $ Grade.yesGrades z 

                                                        return opt
                                                    ) zipper


                                    _ <- element input2 # set children (ListZipper.toList gradeViews2)
                                    --hack create extendI
                                    gradeViews <- sequence $ ListZipper.iextend (\ i z -> do
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

                                    _ <- element input # set children (ListZipper.toList gradeViews)

                                    on UI.selectionChange input $ \ i -> do
                                        case i of
                                            Nothing -> return ()
                                            Just n -> do
                                                runFunction $ ffi "$('#inputter').trigger(%1)" ("t"++(show n))

                                    on UI.selectionChange input2 $ \ i -> do
                                        case i of
                                            Nothing -> return ()
                                            Just n -> do
                                                runFunction $ ffi "$('#inputter2').trigger(%1)" ("w"++(show n))

                                    inputView <- UI.div #. "field" #+
                                        [ UI.div # set (attr "style") "width:100%" #. "select" #+ [ element input ] 
                                        ]

                                    inputView2 <- UI.div #. "field" #+
                                        [ UI.div # set (attr "style") "width:100%" #. "select" #+ [ element input2 ] 
                                        ]


                                    let photographeesUI = fmap 
                                                (\c -> do
                                                    let ident = Photographee._ident c
                                                    let name = Photographee._name c
                                                    let s = name ++ ", " ++ ident
                                                    (button, buttonView) <- mkButton ident s
                                                    on UI.click button $ \_ -> do 
                                                        liftIO $ Chan.writeChan msgs $ Msg.setId $ Id.fromString ident
                                                    mkColumn ["is-12"] [ element buttonView ]
                                                ) (sortOn (Photographee._name) photographees)


                                    inputId <- UI.input #. "input" # set UI.type_ "text" 
                                    inputIdView <- UI.div #. "field" #+
                                        [ UI.label #. "label has-text-dark" # set UI.text "Nummer"
                                        , UI.div #. "control" #+ [ element inputId ] 
                                        ]


                                    inputName <- UI.input #. "input" # set UI.type_ "text" 
                                    inputNameView <- UI.div #. "field" #+
                                        [ UI.label #. "label has-text-dark" # set UI.text "Navn"
                                        , UI.div #. "control" #+ [ element inputName ] 
                                        ]

                                    (buttonAlt, buttonAltView) <- mkCreate msgs inputId inputName
                                    

                                    UI.div #+
                                        [ UI.br
                                        , UI.label #. "label has-text-dark" # set UI.text "Opret elev"
                                        , UI.label #. "label has-text-dark" # set UI.text "Klasse/stue"
                                        , mkColumns ["is-multiline"] 
                                            [ mkColumn ["is-4"] 
                                                [ UI.div #. "control" #+ [ element inputView
                                                                        , element inputIdView
                                                                        , element inputNameView
                                                                        , element buttonAltView
                                                                        ]
                                                ]
                                            ]
                                        , UI.br
                                        , UI.label #. "label has-text-dark" # set UI.text "Find elev"
                                        , mkColumns ["is-multiline"]
                                            ([ mkColumn ["is-4"]
                                                [ element inputView2 ]
                                            ] ++ photographeesUI )
                                        ]
                            ) grades ]


    view <- maybe 
        ( mkSection 
            [ mkColumns ["is-multiline"]
                [ mkColumn ["is-12"]
                    [ mkColumns ["is-multiline"] 
                        [ mkColumn ["is-4"] [ element inputView ]
                        ]
                    ]
                , element dumpSize
                , element grade
                ]
            ]) (\ x -> 
        if (dumpFilesCount dumpFiles) == 0 then 
            ( mkSection 
                [ mkColumns ["is-multiline"]
                    [ mkColumn ["is-12"]
                        [ mkColumns ["is-multiline"] 
                            [ mkColumn ["is-4"] [ element inputView ]
                            , mkColumn ["is-12"] [ UI.p #. "is-size-3" #+ [string ("Navn: " ++ (Photographee._name x))] ]
                            ]
                        ]
                    , element dumpSize
                    , element grade
                    ]
                ])
        else
            mkSection 
                [ mkColumns ["is-multiline"]
                    [ mkColumn ["is-12"]
                        [ mkColumns ["is-multiline"] 
                            [ mkColumn ["is-4"] [ element inputView ]
                            , mkColumn ["is-12"] [ element buildView ]
                            , mkColumn ["is-12"] [ UI.p #. "is-size-3" #+ [string ("Navn: " ++ (Photographee._name x))] ]
                            ]
                        ]
                    , element dumpSize
                    , element grade
                    ]
                ]) photographee

    let view2 = \ s -> maybe ( mkSection 
                [ mkColumns ["is-multiline"]
                    [ mkColumn ["is-12"]
                        [ mkColumns ["is-multiline"] 
                            [ mkColumn ["is-4"] [ element inputView ]
                            ]
                        ]
                    , mkColumn ["is-12"] [UI.p # set text s # set (attr "id") "result" ]
                    , element dumpSize
                    , element grade
                    ]
                ]) (\ x -> 
                    if (dumpFilesCount dumpFiles) == 0 then 
                        ( mkSection 
                            [ mkColumns ["is-multiline"]
                                [ mkColumn ["is-12"]
                                    [ mkColumns ["is-multiline"] 
                                        [ mkColumn ["is-4"] [ element inputView ]
                                        , mkColumn ["is-12"] [ UI.p #. "is-size-3" #+ [string ("Navn: " ++ (Photographee._name x))] ]
                                        ]
                                    ]
                                , mkColumn ["is-12"] [UI.p # set text s # set (attr "id") "result" ]
                                , element dumpSize
                                , element grade
                                ]
                            ])
                    else
                        mkSection 
                            [ mkColumns ["is-multiline"]
                                [ mkColumn ["is-12"]
                                    [ mkColumns ["is-multiline"] 
                                        [ mkColumn ["is-4"] [ element inputView ]
                                        , mkColumn ["is-12"] [ element buildView ]
                                        , mkColumn ["is-12"] [ UI.p #. "is-size-3" #+ [string ("Navn: " ++ (Photographee._name x))] ]
                                        ]
                                    ]
                                , mkColumn ["is-12"] [UI.p # set text s # set (attr "id") "result" ]
                                , element dumpSize
                                , element grade
                                ]
                            ]) photographee

    buildMsg <- Build.build' (element view) (\y -> view2 y) (\p s -> view2 (Photographee._name p ++ " - " ++ s))  build

    menu <- mkMenu msgs states  
    _ <- element body # set children [menu, buildMsg]

    _ <- UI.setFocus input
    _ <- element input # set value (Id.toString id)

    return ()
