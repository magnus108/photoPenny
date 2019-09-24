{-# LANGUAGE OverloadedStrings #-}
module Main
    ( mainSection 
    ) where

import Data.Maybe
import Data.List
import Data.Char
import Control.Monad

import Utils.Comonad
import qualified Utils.ListZipper as ListZipper

import qualified Control.Concurrent.Chan as Chan
import qualified Message as Msg

import qualified PhotoShake.Grade as Grade
import qualified PhotoShake.Id as Id
import qualified PhotoShake.State as State
import qualified PhotoShake.Session as Session
import qualified PhotoShake.Photographee2 as Photographee
import PhotoShake.ShakeConfig 

import qualified Utils.ListZipper as ListZipper

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements
import Menu


mkBuild :: UI (Element, Element)
mkBuild = do
    (button, view) <- mkButton "mover" "Flyt filer"
    _ <- element button # set (attr "id") "builderButton"
    on UI.click button $ \_ -> do
        --liftIO $ Chan.writeChan msgs $ Msg.build
        return ()
    return (button, view)

sessionMissing :: Element -> Chan.Chan Msg.Message -> ListZipper.ListZipper State.State -> UI ()
sessionMissing body msgs states = do
    view <- string "Manger Session"
    menu <- mkMenu msgs states  
    _ <- element body # set children [menu, view]
    return ()

mainSection :: Element -> Chan.Chan Msg.Message -> ListZipper.ListZipper State.State -> Grade.Grades -> Id.Id -> DumpFiles -> Session.Sessions -> Maybe Photographee.Photographee -> [Photographee.Photographee] -> UI ()
mainSection body msgs states grades id dumpFiles sessions photographee photographees = do
    Session.sessions (sessionMissing body msgs states)
        (\y -> Session.session 
            (\ t -> mainSectionKindergarten body msgs states grades id dumpFiles)
            ( mainSectionSchool body msgs states grades id dumpFiles photographee photographees) 
            (ListZipper.focus y) 
        ) sessions

mainSectionKindergarten :: Element -> Chan.Chan Msg.Message -> ListZipper.ListZipper State.State -> Grade.Grades -> Id.Id -> DumpFiles -> UI ()
mainSectionKindergarten body msgs states grades id dumpFiles = do
    view <- mkColumns ["is-multiline"] 
            [ mkColumn ["is-12"] [string "gg"]
            ]
    menu <- mkMenu msgs states  
    _ <- element body # set children [menu, view]
    return ()

mainSectionSchool :: Element -> Chan.Chan Msg.Message -> ListZipper.ListZipper State.State -> Grade.Grades -> Id.Id -> DumpFiles -> Maybe Photographee.Photographee -> [Photographee.Photographee] -> UI ()
mainSectionSchool body msgs states grades id dumpFiles photographee photographees = do
    input <- UI.input #. "input" # set (attr "id") "fotoId" #  set UI.type_ "text" 

    (builderButton, buildView) <- mkBuild 

    on UI.keyup input $ \keycode -> do
        value <- get value input
        when (Id.toString id /= value) $ do
            liftIO $ Chan.writeChan msgs $ Msg.setId $ Id.fromString value

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

    label <- mkLabel "Antal billeder i dump:"

    dumpSize <- mkColumn ["is-12"] 
                    [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-12"] [ element label ]
                        , mkColumn ["is-12"] [ UI.string (dumpFilesToCount dumpFiles) #. "is-size-1 has-text-danger has-text-weight-bold" ]
                        ]
                    ]

    grade <- mkColumn ["is-4"] 
        [ Grade.grades ( UI.div #. "field" #+
                        [ UI.label #. "label has-text-dark" # set UI.text "Find elev. Der er ingen Stuer/Klasser"
                        , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                ]
                        ]) (\(ListZipper.ListZipper ls y rs) -> do
                                    -- list is sorted on the type level
                                    let zipper = ListZipper.sorted (ls ++ rs) (ListZipper.ListZipper [] y [])

                                    input <- UI.select # set (attr "style") "width:100%" # set (attr "id") "inputter"
                                    
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

                                    inputView <- UI.div #. "field" #+
                                        [ UI.div # set (attr "style") "width:100%" #. "select" #+ [ element input ] 
                                        ]


                                    photographees <- UI.div #+ 
                                        fmap (\c -> do
                                                --(button, buttonView) <- mkButton "es s
                                                --on UI.click button $ \_ -> do 
                                                            --- SET
                                                            --PHOTOGRAPHEEE
                                                            --liftIO $ setIdSelection conf (Photographee.Idd tea)
                                                            --hack <- liftIO $ getDagsdato conf
                                                            --setDagsdato conf hack
                                                ---return buttonView
                                                
                                                UI.div #+ [string (Photographee._name c)]
                                            ) (sortOn (Photographee._name) photographees)
                                        --setNumber config' input' (Photographee._ident c) (Photographee._name c ++ ", " ++ Photographee._ident c)])


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


                                    UI.div #. "field" #+
                                        [ UI.label #. "label has-text-dark" # set UI.text "Klasse/stue"
                                        , UI.div #. "control" #+ [ element inputView
                                                                 , element inputIdView
                                                                 , element inputNameView
                                                                 ]
                                        , element photographees
                                        ]
                            ) grades ]

    view <- maybe 
        ( mkSection 
            [ mkColumns ["is-multiline"]
                [ mkColumn ["is-4"] [ element inputView ]
                , mkColumn ["is-12"] [ element buildView ]
                , element dumpSize
                , element grade
                ]
            ]) (\ x -> 
        mkSection 
            [ mkColumns ["is-multiline"]
                [ mkColumn ["is-4"] [ element inputView ]
                , mkColumn ["is-12"] [ element buildView ]
                , mkColumn ["is-12"] [ UI.p #. "is-size-3" #+ [string ("Navn: " ++ (Photographee._name x))] ]
                , element dumpSize
                , element grade
                ]
            ]) photographee

    menu <- mkMenu msgs states  
    _ <- element body # set children [menu, view]

    _ <- UI.setFocus input
    _ <- element input # set value (Id.toString id)

    return ()

{-

    built <- liftIO $ withMVar config' $ (\conf -> getBuilt conf)

    let isBuilding = case built of
                        NoBuilt -> False
                        NoFind s -> False
                        Building _ _ -> True
                        Built _ _ -> False
                        
    (builderButton, buildView) <- mkBuild config'
    
    (Photographee.Idd identi) <- liftIO $ withMVar config' $ (\conf -> getIdSelection conf)

    input <- UI.input #. "input" # set (attr "id") "fotoId" #  set UI.type_ "text" # set (attr "value") identi
    input' <- if (not isBuilding) then return input else (element input) # set (attr "disabled") "" 

    inputView <- UI.div #. "field" #+
        [ UI.label #. "label has-text-dark" # set UI.text "Foto Id"
        , UI.div #. "control" #+ [ element input' ] 
        ]

    on UI.keydown inputView $ \keycode -> when (keycode == 13) $ do
        UI.setFocus (builderButton) 
        runFunction $ ffi "$('#builderButton').trigger('click')"
        return ()

    (Photographee.Idd val) <- liftIO $ withMVar config' $ (\conf -> getIdSelection conf)

    idenName <- liftIO $ do
        locationFile <- try $ withMVar config' $ (\conf -> do
                getLocationFile conf 
                ) :: IO (Either ShakeError Location.Location)
        case locationFile of 
            Left e -> newIORef ""
            Right loc -> do
                Location.location (newIORef "") (\xxx -> do
                            liftIO $ withMVar config' $ (\conf -> do
                                    what <- Photographee.findPhotographee3 xxx val
                                    case what of
                                        Nothing -> newIORef ""
                                        Just iddd ->  newIORef (Photographee._name iddd))) loc

    on UI.keyup input $ \keycode -> when (keycode /= 13) $ do
        val <- get value input
        liftIO $ withMVar config' $ (\conf -> do
                setIdSelection conf (Photographee.Idd val))
        locationFile <- liftIO $ try $ withMVar config' $ (\conf -> do
                getLocationFile conf 
                ) :: UI (Either ShakeError Location.Location)
        case locationFile of 
            Left e -> return ()
            Right loc -> do
                Location.location (return ()) (\ xxx -> do
                            wuba <-liftIO $ withMVar config' $ (\conf -> do
                                    Photographee.findPhotographee3 xxx val)
                            case wuba of
                                Nothing -> do
                                    waba <- liftIO $ readIORef idenName
                                    case waba of
                                        "" -> return ()
                                        zzzzz -> do
                                            hack <- liftIO $ withMVar config' $ (\conf -> getDagsdato conf)
                                            liftIO $ withMVar config' $ (\conf -> setDagsdato conf hack)
                                            return ()
                                Just iddd -> do 
                                    hack <- liftIO $ withMVar config' $ (\conf -> getDagsdato conf)
                                    liftIO $ withMVar config' $ (\conf -> setDagsdato conf hack)

                                    liftIO $ modifyIORef idenName (\x -> (Photographee._name iddd))
                                    return ()) loc
        



    builtMsg <- case built of
                        NoBuilt -> UI.div
                        NoFind s -> mkColumn ["is-12"] [UI.p # set text s # set (attr "id") "result" ]
                        Built _ s ->mkColumn ["is-12"] [ UI.p # set text s # set (attr "id") "result" ]
                        Building _ s -> mkColumn ["is-12"] [UI.p # set text s # set (attr "id") "result"]

    msg <- case built of
                    NoBuilt -> UI.div 
                    NoFind _ -> UI.div
                    Built p _ -> mkColumn ["is-12"] [UI.p # set text (Photographee._name p)]
                    Building p _ -> mkColumn ["is-12"] [UI.p # set text (Photographee._name p)]



    --(_, viewReset)<- mkReset config'

    --viewReset2 <- mkSection $ 
      --              [ mkColumns ["is-multiline"]
        --                [ mkColumn ["is-4"] [element viewReset] ]
          --          ]
    

    sessions_ <- liftIO $ withMVar config' $ (\conf -> getSessions conf)


    nameIden <- liftIO $ readIORef idenName

    viewSchool <- mkColumns ["is-multiline"]
                [ mkColumn ["is-12"] [element inputView]
                , mkColumn ["is-12"] [element buildView]
                , if nameIden == "" then UI.div else mkColumn ["is-12"] [UI.p #. "is-size-3" #+ [string ("Navn: " ++ nameIden)]]
                ]

    identKinder <- liftIO $ newIORef ""
    inputKinder <- UI.input #. "input" # set UI.type_ "text" 
    inputKinder' <- if (not isBuilding) then return inputKinder else (element inputKinder) # set (attr "disabled") ""
    inputViewKinder <- UI.div #. "field" #+
        [ UI.label #. "label has-text-dark" # set UI.text "Nummer"
        , UI.div #. "control" #+ [ element inputKinder' ] 
        ]

    on UI.keyup inputKinder' $ \_ -> liftIO . writeIORef identKinder =<< get value inputKinder'

    identKinderName <- liftIO $ newIORef ""
    inputKinderName <- UI.input #. "input" # set UI.type_ "text" 
    inputKinderName' <- if (not isBuilding) then return inputKinderName else (element inputKinderName) # set (attr "disabled") ""
    inputViewKinderName <- UI.div #. "field" #+
        [ UI.label #. "label has-text-dark" # set UI.text "Navn"
        , UI.div #. "control" #+ [ element inputKinderName' ] 
        ]

    on UI.keyup inputKinderName' $ \_ -> liftIO . writeIORef identKinderName =<< get value inputKinderName'

    --BAD can throw error
    grades <- liftIO $ withMVar config' $ (\conf -> getGrades conf)

    identKinderClass <- Grade.grades (liftIO $ newIORef "Ingen valg") (\(ListZipper _ x _ ) -> liftIO $ newIORef x) grades


    --badness 3thousand
    inputViewKinderClass <- 
        Grade.grades (UI.div #. "field" #+
                        [ UI.label #. "label has-text-dark" # set UI.text "Ingen stuer/klasser"
                        , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                ]
                        ])( \zipper -> do
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
                                    [ UI.label #. "label has-text-dark" # set UI.text "Stue/Klasser"
                                    , UI.div # set (attr "style") "width:100%" #. "select" #+ [ element inputKinderClass' ] 
                                    ]

                                on UI.selectionChange inputKinderClass' $ \xxxx -> do
                      
                                    case xxxx of
                                        Nothing -> error "this is bad"
                                        Just n -> do
                                            let val = (sort $ toList zipper) !! n
                                            liftIO $  writeIORef identKinderClass val

                                return inputViewKinderClass'
                            ) grades

    --BAD can throw error
    gradeSelection <- liftIO $ withMVar config' $ (\conf -> getGradeSelection conf)

    inputViewKinderClasssCopy <- 
            Grade.grades (UI.div #. "field" #+
                        [ UI.label #. "label has-text-dark" # set UI.text "Find elev. Der er ingen stuer/klasser"
                        , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                ]
                        ]) (\zipper -> do
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
                    

    let wats = (\zipper item -> do

                    input' <- UI.button # set UI.type_ "button" # set UI.name "sessions"
                        
                    let label = type_ "Enkelt" "Gruppe" item
                    
                    button <- UI.button #. "button" #+ [string label] 

                    button' <- if (not isBuilding) then return button else (element button) # set (attr "disabled") ""
                    
                    on UI.click button' $ \_ -> do
                        _ <- liftIO $ withMVar config' $ (\conf -> setSession conf $ yesSessions zipper )-- det her må man ik?
                        idd <- liftIO $ readIORef identKinder
                        clas <- liftIO $ readIORef identKinderClass

                        name <- liftIO $ readIORef identKinderName
                        _ <- liftIO $ withMVar config' $ (\conf -> do
                                locationFile <- getLocationFile conf
                                -- kinda bad here could cause errorr
                                Location.location (return (Left LocationConfigFileMissing))
                                    (\xxx -> liftIO $ try $ Photographee.insertPhotographee xxx idd clas name)
                                        locationFile 

                                )
                        liftIO $ modifyIORef identKinder (\x -> "SYS_" ++ x)
                        liftIO $ funci2 config' (identKinder)

                    return button'
            ) 

    -- badness 3000
    let s = sessions (UI.div #+ [ string "Session ikke angivet" ]) (\y -> 
                UI.div #. "buttons has-addons" #+ (fmap snd $ filter (\xxx ->
                                                        session (\_ -> session 
                                                                    (\_ -> True)
                                                                    (False)
                                                                    (focus y)
                                                                )
                                                        (school == focus y)
                                                        (fst xxx)
                                                    ) $ toList $ y =>> (\zipper ->
                            session (\t -> (kindergarten t, wats zipper t))
                                (school, element viewSchool)
                            (focus zipper) ))) sessions_

    buttonAlt <- UI.button #. "button" #+ [string "Opret og flyt"] 

    buttonAlt' <- if (not isBuilding) then return buttonAlt else (element buttonAlt ) # set (attr "disabled") ""
    
    on UI.click buttonAlt' $ \_ -> do
        --_ <- liftIO $ setSession config $ Sessions zipper
        idd <- liftIO $ readIORef identKinder
        clas <- liftIO $ readIORef identKinderClass
        name <- liftIO $ readIORef identKinderName
        case (idd /= "" || name /= "") of
            True -> do
                    _ <- liftIO $ withMVar config' $ (\conf -> do
                                    locationFile <- getLocationFile conf
                                    Location.location (return (Left LocationConfigFileMissing))
                                        (\ xxx -> liftIO $ try $ Photographee.insertPhotographee xxx idd clas name)
                                        locationFile 
                            )
                    liftIO $ modifyIORef identKinder (\x -> "SYS_" ++ x)
                    liftIO $ funci2 config' identKinder
            False -> return ()
    

    locationFile <- liftIO $ try $ withMVar config' $ (\conf -> getLocationFile conf) :: UI (Either ShakeError Location.Location)
            -- kinda bad here could cause errorr

    kidsInGrade <- case locationFile of 
        Left e -> return []
        Right loc ->
                Location.location (return [] ) (\xxx -> do
                    liftIO $ withMVar config' $ (\conf -> do
                        val <- liftIO $ try $ getGradeSelection conf :: IO (Either ShakeError Photographee.GradeSelection)
                        case val of
                            Left e -> return []
                            Right vv ->
                                liftIO $ Photographee.parsePhotographees xxx vv)) loc


    kidsInGradeView <- mkColumn ["is-12"] $ fmap 
            (\c -> UI.div #+ 
                [setNumber config' input' (Photographee._ident c) (Photographee._name c ++ ", " ++ Photographee._ident c)]
            ) $ sortBy (\x y -> compare (Photographee._name x) (Photographee._name y)) kidsInGrade 

    -- antal billeder
    dumpies <- liftIO $ withMVar config' $ (\conf -> getDump conf)
    dumps <- liftIO $ try $ withMVar config' $ (\conf -> getDumpFiles dumpies) :: UI (Either ShakeError [(FilePath, FilePath)])
    let dumps' = case dumps of 
            Left e -> show e 
            Right x -> show $ length $ fmap fst x

    label <- mkLabel "Antal billeder i dump:"
    dumpSize <- mkColumn ["is-12"] 
                    [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-12"] [ element label ]
                        , mkColumn ["is-12"] [ UI.string dumps' #. "is-size-1 has-text-danger has-text-weight-bold" ]
                        ]
                    ]

    let ss = sessions (UI.div) (\y ->
                session (\ t -> mkColumns ["is-multiline"] 
                                            [ mkColumn ["is-3"] [element inputViewKinderClass]
                                            , mkColumn ["is-3"] [element inputViewKinder]
                                            , mkColumn ["is-3"] [element inputViewKinderName]
                                            , mkColumn ["is-12"] [s] 
                                            , element dumpSize
                                            , element msg
                                            , element builtMsg
                                            ]) (mkColumns ["is-multiline"] 
                                            [ mkColumn ["is-12"] [s] 
                                            , element msg
                                            , element builtMsg
                                            , element dumpSize
                                            , mkColumn ["is-3"] [element inputViewKinderClass]
                                            , mkColumn ["is-3"] [element inputViewKinder]
                                            , mkColumn ["is-3"] [element inputViewKinderName]
                                            , mkColumn ["is-12"] [element buttonAlt']
                                            , mkColumn ["is-12"] [UI.br]-- ffs
                                            , mkColumn ["is-9"] [element inputViewKinderClasssCopy]-- ffs
                                            , element kidsInGradeView
                                            ]) (focus y) ) sessions_


    inputView2 <- mkSection $ 
                   [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-12"] [ss] 
                        ]
                    ]

    contents <- UI.div #+ [ element inputView2] --, element viewReset2]
    
    return (contents, input)




setNumber :: MVar ShakeConfig -> Element -> String -> String -> UI Element
setNumber config' input' tea s = do
    (button, view) <- mkButton s s
    on UI.click button $ \_ -> do 
        liftIO $ withMVar config' $ (\conf -> do
                liftIO $ setIdSelection conf (Photographee.Idd tea)
                hack <- liftIO $ getDagsdato conf
                setDagsdato conf hack
                )
    return view


mkBuild :: MVar ShakeConfig -> UI (Element, Element)
mkBuild config = do
    --- with pattern
    (button, view) <- mkButton "mover" "Flyt filer"
    set (attr "id") "builderButton" (element button)
    on UI.click button $ \_ -> do
        (Photographee.Idd idd) <- liftIO $ withMVar config $ (\conf -> getIdSelection conf)
        liftIO $ withMVar config $ (\conf -> setIdSelection conf (Photographee.Idd ""))
        case (idd == "" ) of
            False -> liftIO $ withMVar config $ (\conf -> funci conf (Photographee.Idd idd))
            True -> return ()
    return (button, view)

funci :: ShakeConfig -> Photographee.Idd -> IO ()
funci config (Photographee.Idd idd2) = do
    putStrLn "ol"
    --have to look this up from config
    locationFile <- getLocationFile config
    -- kinda bad here
    -- kinda bad here could cause errorr
    find <- Location.location (return (Left LocationConfigFileMissing))
            (\xxx -> try $ Photographee.findPhotographee xxx idd2 :: IO (Either ShakeError Photographee.Photographee))
                locationFile

    case find of
            Left errMsg -> do
                    setBuilt' config (NoFind (show errMsg))

            Right photographee -> do
                    time <- getCurrentTime
                    -- wtf????
                    Location.location (setBuilt' config (NoFind (show LocationConfigFileMissing)))
                        (\xxx -> do
                            build <- try $ myShake config photographee (takeBaseName xxx) time True :: IO (Either ShakeError ())
                            case build of
                                    Left errMsg -> do
                                        setBuilt' config (NoFind (show errMsg))
                                    Right _ -> do
                                        setBuilt' config (Built  photographee "Færdig")
                                        return () ) locationFile 



funci2 :: MVar ShakeConfig -> (IORef String) -> IO ()
funci2 config idd = do
    --have to look this up from config
    idd2 <- readIORef idd
    locationFile <- withMVar config $ (\conf -> getLocationFile conf)
    -- kinda bad here
    -- kinda bad here could cause errorr
    find <- 
        Location.location (return (Left LocationConfigFileMissing)) (\xxx -> do
            try $ Photographee.findPhotographee2 xxx idd2 :: IO (Either ShakeError Photographee.Photographee))
            locationFile 

    case find of
            Left errMsg -> do
                    withMVar config $ (\conf -> setBuilt' conf (NoFind (show errMsg)))

            Right photographee -> do
                    time <- getCurrentTime
                    -- wtf????
                    Location.location (withMVar config $ (\conf -> setBuilt' conf (NoFind (show LocationConfigFileMissing))))
                        (\xxx -> do
                            build <- try $ withMVar config (\conf -> myShake conf photographee (takeBaseName xxx) time True) :: IO (Either ShakeError ())
                            case build of
                                    Left errMsg -> do
                                        withMVar config (\conf -> setBuilt' conf (NoFind (show errMsg))  )
                                    Right _ -> do
                                        withMVar config (\conf -> setBuilt' conf (Built  photographee "Færdig"))
                                        return () ) locationFile
-}
