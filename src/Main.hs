{-# LANGUAGE OverloadedStrings #-}
module Main
    ( mainSection 
    ) where
import Elements
import PhotoShake.Dagsdato
import Control.Concurrent.MVar

import Data.List

import PhotoShake
import PhotoShake.ShakeConfig
import PhotoShake.Doneshooting
import PhotoShake.Shooting
import PhotoShake.Session
import PhotoShake.Location
import PhotoShake.Photographer
import PhotoShake.Dump
import PhotoShake.Photographee
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


mainSection :: FilePath -> FilePath -> ShakeConfig -> MVar ShakeConfig -> Window -> UI Element
mainSection _ _ config config' _ = do

    built <- liftIO $ withMVar config' $ (\conf -> getBuilt conf)

    let isBuilding = case built of
                        NoBuilt -> False
                        NoFind s -> False
                        Building _ _ -> True
                        Built _ _ -> False
                        
    ident <- liftIO $ newIORef ""
    (_, buildView) <- mkBuild config' ident

    input <- UI.input #. "input" # set UI.type_ "text" 
    input' <- if (not isBuilding) then return input else (element input) # set (attr "disabled") ""

    inputView <- UI.div #. "field" #+
        [ UI.label #. "label has-text-info" # set UI.text "Nummer"
        , UI.div #. "control" #+ [ element input' ] 
        ]

    on UI.keyup input $ \_ -> liftIO . writeIORef ident =<< get value input

    builtMsg <- UI.p # set text (case built of
                                    NoBuilt -> ""
                                    NoFind s -> s
                                    Built _ s -> s
                                    Building _ s -> s)

    msg <- UI.p # set text (case built of
                                    NoBuilt -> ""
                                    NoFind _ -> ""
                                    Built p _ -> _name p
                                    Building p _ -> _name p)



    (_, viewReset)<- mkReset config'

    viewReset2 <- mkSection $ 
                    [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-4"] [element viewReset] ]
                    ]
    

    sessions <- liftIO $ withMVar config' $ (\conf -> getSessions conf)


    viewSchool <- mkColumns ["is-multiline"]
                [ mkColumn ["is-12"] [element inputView]
                , mkColumn ["is-12"] [element buildView]
                ]

    identKinder <- liftIO $ newIORef ""
    inputKinder <- UI.input #. "input" # set UI.type_ "text" 
    inputKinder' <- if (not isBuilding) then return inputKinder else (element inputKinder) # set (attr "disabled") ""
    inputViewKinder <- UI.div #. "field" #+
        [ UI.label #. "label has-text-info" # set UI.text "Nummer"
        , UI.div #. "control" #+ [ element inputKinder' ] 
        ]

    on UI.keyup inputKinder' $ \_ -> liftIO . writeIORef identKinder =<< get value inputKinder'

    identKinderName <- liftIO $ newIORef ""
    inputKinderName <- UI.input #. "input" # set UI.type_ "text" 
    inputKinderName' <- if (not isBuilding) then return inputKinderName else (element inputKinderName) # set (attr "disabled") ""
    inputViewKinderName <- UI.div #. "field" #+
        [ UI.label #. "label has-text-info" # set UI.text "Navn"
        , UI.div #. "control" #+ [ element inputKinderName' ] 
        ]

    on UI.keyup inputKinderName' $ \_ -> liftIO . writeIORef identKinderName =<< get value inputKinderName'

    --BAD can throw error
    grades <- liftIO $ withMVar config' $ (\conf -> getGrades conf)

    identKinderClass <- case grades of 
                NoGrades -> liftIO $ newIORef "Ingen valg"
                Grades (ListZipper _ x _) ->
                        liftIO $ newIORef x


    --badness 3thousand
    inputViewKinderClass <- case grades of 
            NoGrades -> do
                UI.div #. "field" #+
                        [ UI.label #. "label has-text-info" # set UI.text "Ingen stuer/klasser"
                        , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                ]
                        ]
            Grades zipper -> do
                    inputKinderClass <- UI.select # set (attr "style") "width:100%" #+ (fmap (\x -> UI.option # set (attr "value") x # set text x) (toList zipper))
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

    let wats = (\zipper item -> do

                    input' <- UI.button # set UI.type_ "button" # set UI.name "sessions"
                        
                    let label = case item of
                                    Group -> "Gruppe"
                                    Single -> "Enkelt"
                    
                    button <- UI.button #. "button" #+ [string label] 

                    button' <- if (not isBuilding) then return button else (element button) # set (attr "disabled") ""
                    
                    on UI.click button' $ \_ -> do
                        liftIO $ putStrLn "ffgag"
                        _ <- liftIO $ withMVar config' $ (\conf -> setSession conf $ Sessions zipper )-- det her må man ik?
                        idd <- liftIO $ readIORef identKinder
                        clas <- liftIO $ readIORef identKinderClass

                        name <- liftIO $ readIORef identKinderName
                        locationFile <- liftIO $ withMVar config' $ (\conf -> getLocationFile conf)
                        -- kinda bad here
                        -- kinda bad here could cause errorr
                        find <- case locationFile of 
                            NoLocation -> return (Left LocationConfigFileMissing)
                            Location xxx -> do
                                liftIO $ try $ insertPhotographee xxx idd clas name --- SUCHBAD
                        liftIO $ funci config' identKinder

                    return button'
            ) 

    -- badness 3000
    let s = case sessions of
            NoSessions -> UI.div #+ [ string "Session ikke angivet" ]
            Sessions y ->
                UI.div #. "buttons has-addons" #+ (fmap snd $ filter (\xxx -> case (fst xxx) of 
                                                        Kindergarten _ -> case focus y of
                                                                Kindergarten _ -> True
                                                                School -> False
                                                        School -> School == focus y
                                                    ) $ toList $ y =>> (\zipper ->
                            case (focus zipper) of
                                Kindergarten t -> (Kindergarten t, wats zipper t)
                                School -> (School, element viewSchool)
                            ) )

    buttonAlt <- UI.button #. "button" #+ [string "Indsæt ny elev"] 

    buttonAlt' <- if (not isBuilding) then return buttonAlt else (element buttonAlt ) # set (attr "disabled") ""
    
    on UI.click buttonAlt' $ \_ -> do
        --_ <- liftIO $ setSession config $ Sessions zipper
        idd <- liftIO $ readIORef identKinder
        clas <- liftIO $ readIORef identKinderClass
        name <- liftIO $ readIORef identKinderName
        locationFile <- liftIO $ withMVar config' $ (\conf -> getLocationFile conf)
        -- kinda bad here
        -- kinda bad here could cause errorr
        find <- case locationFile of 
            NoLocation -> return (Left LocationConfigFileMissing)
            Location xxx -> do
                liftIO $ try $ insertPhotographee xxx idd clas name --- SUCHBAD
        liftIO $ funci config' identKinder
    
    let ss = case sessions of 
            NoSessions -> UI.div 
            Sessions y ->
                    case (focus y) of
                            School -> mkColumns ["is-multiline"] 
                                            [ mkColumn ["is-12"] [s] 
                                            , mkColumn ["is-12"] [UI.br]-- ffs
                                            , mkColumn ["is-3"] [element inputViewKinderClass]
                                            , mkColumn ["is-3"] [element inputViewKinder]
                                            , mkColumn ["is-3"] [element inputViewKinderName]
                                            , mkColumn ["is-12"] [element buttonAlt']
                                            ]
                            Kindergarten t -> mkColumns ["is-multiline"] 
                                            [ mkColumn ["is-3"] [element inputViewKinderClass]
                                            , mkColumn ["is-3"] [element inputViewKinder]
                                            , mkColumn ["is-3"] [element inputViewKinderName]
                                            , mkColumn ["is-12"] [UI.br]-- ffs
                                            , mkColumn ["is-12"] [s] 
                                            ]

    -- antal billeder
    dumps <- liftIO $ withMVar config' $ (\conf -> getDumpFiles conf)
    let dumps' = length $ fmap fst dumps
    label <- mkLabel "Antal billeder i dump:"
    dumpSize <- mkSection 
                    [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-12"] [ element label ]
                        , mkColumn ["is-12"] [ UI.string (show dumps') #. "is-size-1 has-text-danger has-text-weight-bold" ]
                        ]
                    ]


    inputView2 <- mkSection $ 
                   [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-12"] [ss] 
                        , mkColumn ["is-12"] [element msg] 
                        , mkColumn ["is-12"] [element builtMsg]
                        ]
                    ]

    UI.div #+ [ element inputView2, element dumpSize, element viewReset2]


mkReset :: MVar ShakeConfig -> UI (Element, Element)
mkReset config = do
    (button, view) <- mkButton "reset" "Reset konfiguration"
    on UI.click button $ \_ -> liftIO $ resetIt config
    return (button, view)


resetIt :: MVar ShakeConfig -> IO ()
resetIt config = 
        (withMVar config $ (\conf -> setDump conf NoDump))
        >> (withMVar config $ (\conf -> setDagsdato conf NoDagsdato))
        >> (withMVar config $ (\conf -> setPhotographers conf NoPhotographers))
        >> (withMVar config $ (\conf -> setLocation conf NoLocation))
        >> (withMVar config $ (\conf -> setSession conf NoSessions))
        >> (withMVar config $ (\conf -> setShooting conf NoShootings))
        >> (withMVar config $ (\conf -> setDoneshooting conf NoDoneshooting))
        >> (withMVar config $ (\conf -> setBuilt' conf NoBuilt))
        >> (withMVar config $ (\conf -> setGrades conf NoGrades))

mkBuild :: MVar ShakeConfig -> IORef String -> UI (Element, Element)
mkBuild config idd = do
    --- with pattern
    (button, view) <- mkButton "mover" "Flyt filer"
    on UI.click button $ \_ -> liftIO $ funci config idd
    return (button, view)


funci :: MVar ShakeConfig -> (IORef String) -> IO ()
funci config idd = do
    --have to look this up from config
    idd2 <- readIORef idd
    locationFile <- withMVar config $ (\conf -> getLocationFile conf)
    -- kinda bad here
    -- kinda bad here could cause errorr
    find <- case locationFile of 
        NoLocation -> return (Left LocationConfigFileMissing)
        Location xxx -> do
            try $ findPhotographee xxx idd2 :: IO (Either ShakeError Photographee)

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
                            build <- try $ withMVar config (\conf -> myShake conf photographee (takeBaseName xxx) time ) :: IO (Either ShakeError ())
                            case build of
                                    Left errMsg -> do
                                        withMVar config (\conf -> setBuilt' conf (NoFind (show errMsg))  )
                                    Right _ -> do
                                        withMVar config (\conf -> setBuilt' conf (Built  photographee "Færdig"))
                                        return () 
