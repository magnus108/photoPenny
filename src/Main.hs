{-# LANGUAGE OverloadedStrings #-}
module Main
    ( mainSection 
    ) where
import Elements
import PhotoShake.Dagsdato
import Control.Concurrent.MVar
import Data.List
import Control.Monad

import Data.List

import PhotoShake
import PhotoShake.ShakeConfig
import PhotoShake.Doneshooting
import PhotoShake.Shooting
import PhotoShake.Session hiding (getSessions)
import PhotoShake.Location
import PhotoShake.Photographer
import PhotoShake.Dump
import qualified PhotoShake.Photographee as Photographee
import PhotoShake.Built

import Data.Time.Clock
import Control.Exception
import System.FilePath

import PhotoShake.ShakeError

import Data.IORef

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Utils.ListZipper
import Utils.Comonad



mainSection :: FilePath -> FilePath -> ShakeConfig -> MVar ShakeConfig -> Window -> UI (Element, Element)
mainSection _ _ config config' w = do

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
                ) :: IO (Either ShakeError Location)
        case locationFile of 
            Left e -> newIORef ""
            Right loc -> do
                case loc of
                    NoLocation -> newIORef ""
                    Location xxx -> do
                            liftIO $ withMVar config' $ (\conf -> do
                                    what <- Photographee.findPhotographee3 xxx val
                                    case what of
                                        Nothing -> newIORef ""
                                        Just iddd ->  newIORef (Photographee._name iddd))

    on UI.keyup input $ \keycode -> when (keycode /= 13) $ do
        val <- get value input
        liftIO $ withMVar config' $ (\conf -> do
                setIdSelection conf (Photographee.Idd val))
        locationFile <- liftIO $ try $ withMVar config' $ (\conf -> do
                getLocationFile conf 
                ) :: UI (Either ShakeError Location)
        case locationFile of 
            Left e -> return ()
            Right loc -> do
                case loc of
                    NoLocation -> return () --return (Left LocationConfigFileMissing)
                    Location xxx -> do
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
                                    return ()
        



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

    identKinderClass <- case grades of 
                Photographee.NoGrades -> liftIO $ newIORef "Ingen valg"
                Photographee.Grades (ListZipper _ x _) ->
                        liftIO $ newIORef x


    --badness 3thousand
    inputViewKinderClass <- case grades of 
            Photographee.NoGrades -> do
                UI.div #. "field" #+
                        [ UI.label #. "label has-text-dark" # set UI.text "Ingen stuer/klasser"
                        , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                ]
                        ]
            Photographee.Grades zipper -> do
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

    --BAD can throw error
    gradeSelection <- liftIO $ withMVar config' $ (\conf -> getGradeSelection conf)

    inputViewKinderClasssCopy <- case grades of 
            Photographee.NoGrades -> do
                UI.div #. "field" #+
                        [ UI.label #. "label has-text-dark" # set UI.text "Find elev. Der er ingen stuer/klasser"
                        , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                ]
                        ]
            Photographee.Grades zipper -> do
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
                                    if gradeSelection == (Photographee.GradeSelection val) then
                                            return ()
                                    else
                                        liftIO $ withMVar config' $ (\conf -> do
                                                liftIO $ setGradeSelection conf (Photographee.GradeSelection val)
                                                liftIO $ setGrades conf (Photographee.Grades $ ListZipper [] val (sort $ toList zipper))
                                        )
        

                    return inputViewKinderClass'
                    

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
                                case locationFile of 
                                    NoLocation -> return (Left LocationConfigFileMissing)
                                    Location xxx -> do
                                        liftIO $ try $ Photographee.insertPhotographee xxx idd clas name --- SUCHBAD

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
                                    case locationFile of 
                                        NoLocation -> return (Left LocationConfigFileMissing)
                                        Location xxx -> do
                                            liftIO $ try $ Photographee.insertPhotographee xxx idd clas name
                            )
                    liftIO $ modifyIORef identKinder (\x -> "SYS_" ++ x)
                    liftIO $ funci2 config' identKinder
            False -> return ()
    

    locationFile <- liftIO $ try $ withMVar config' $ (\conf -> getLocationFile conf) :: UI (Either ShakeError Location)
            -- kinda bad here could cause errorr

    kidsInGrade <- case locationFile of 
        Left e -> return []
        Right loc ->
            case loc of 
                NoLocation -> return [] 
                Location xxx -> do
                    liftIO $ withMVar config' $ (\conf -> do
                        val <- liftIO $ try $ getGradeSelection conf :: IO (Either ShakeError Photographee.GradeSelection)
                        case val of
                            Left e -> return []
                            Right vv ->
                                liftIO $ Photographee.parsePhotographees xxx vv)


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
    find <- case locationFile of 
        NoLocation -> return (Left LocationConfigFileMissing)
        Location xxx -> do
            try $ Photographee.findPhotographee xxx idd2 :: IO (Either ShakeError Photographee.Photographee)

    case find of
            Left errMsg -> do
                    setBuilt' config (NoFind (show errMsg))

            Right photographee -> do
                    time <- getCurrentTime
                    -- wtf????
                    case locationFile of 
                        NoLocation -> do 
                            setBuilt' config (NoFind (show LocationConfigFileMissing))
                    
                        Location xxx -> do
                            build <- try $ myShake config photographee (takeBaseName xxx) time True :: IO (Either ShakeError ())
                            case build of
                                    Left errMsg -> do
                                        setBuilt' config (NoFind (show errMsg))
                                    Right _ -> do
                                        setBuilt' config (Built  photographee "Færdig")
                                        return () 



funci2 :: MVar ShakeConfig -> (IORef String) -> IO ()
funci2 config idd = do
    --have to look this up from config
    idd2 <- readIORef idd
    locationFile <- withMVar config $ (\conf -> getLocationFile conf)
    -- kinda bad here
    -- kinda bad here could cause errorr
    find <- case locationFile of 
        NoLocation -> return (Left LocationConfigFileMissing)
        Location xxx -> do
            try $ Photographee.findPhotographee2 xxx idd2 :: IO (Either ShakeError Photographee.Photographee)

    case find of
            Left errMsg -> do
                    withMVar config $ (\conf -> setBuilt' conf (NoFind (show errMsg)))

            Right photographee -> do
                    time <- getCurrentTime
                    -- wtf????
                    case locationFile of 
                        NoLocation -> do 
                            withMVar config $ (\conf -> setBuilt' conf (NoFind (show LocationConfigFileMissing)))
                    
                        Location xxx -> do
                            build <- try $ withMVar config (\conf -> myShake conf photographee (takeBaseName xxx) time True) :: IO (Either ShakeError ())
                            case build of
                                    Left errMsg -> do
                                        withMVar config (\conf -> setBuilt' conf (NoFind (show errMsg))  )
                                    Right _ -> do
                                        withMVar config (\conf -> setBuilt' conf (Built  photographee "Færdig"))
                                        return () 
