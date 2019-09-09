{-# LANGUAGE OverloadedStrings #-}
module Main2
    ( mainSection2
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



mainSection2 :: FilePath -> FilePath -> ShakeConfig -> MVar ShakeConfig -> Window -> UI (Element, Element)
mainSection2 root _ config config' w = do

    built <- liftIO $ withMVar config' $ (\conf -> getBuilt conf)

    let isBuilding = case built of
                        NoBuilt -> False
                        NoFind s -> False
                        Building _ _ -> True
                        Built _ _ -> False

    (builderButton, buildView) <- mkBuild config'
    
    (Idd identi) <- liftIO $ withMVar config' $ (\conf -> getIdSelection conf)


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

    on UI.keyup input $ \keycode -> when (keycode /= 13) $ do
        val <- get value input
        liftIO $ withMVar config' $ (\conf -> do
                setIdSelection conf (Idd val))

    builtMsg <- case built of
                        NoBuilt -> UI.div
                        NoFind s -> mkColumn ["is-12"] [UI.p # set text s # set (attr "id") "result" ]
                        Built _ s ->mkColumn ["is-12"] [ UI.p # set text s # set (attr "id") "result" ]
                        Building _ s -> mkColumn ["is-12"] [UI.p # set text s # set (attr "id") "result"]

    msg <- case built of
                    NoBuilt -> UI.div 
                    NoFind _ -> UI.div
                    Built p _ -> mkColumn ["is-12"] [UI.p # set text (_name p)]
                    Building p _ -> mkColumn ["is-12"] [UI.p # set text (_name p)]



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







    (Idd val) <- liftIO $ withMVar config' $ (\conf -> getIdSelection conf)

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
                                    what <- findPhotographee3 xxx val
                                    case what of
                                        Nothing -> newIORef ""
                                        Just iddd ->  newIORef (_name iddd))

    on UI.keyup input' $ \keycode -> when (keycode /= 13) $ do
        val <- get value input
        liftIO $ withMVar config' $ (\conf -> do
                setIdSelection conf (Idd val))
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
                                    findPhotographee3 xxx val)
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

                                    liftIO $ modifyIORef idenName (\x -> (_name iddd))
                                    return ()



    nameIden <- liftIO $ readIORef idenName

    viewSchool <- mkColumns ["is-multiline"]
                [ mkColumn ["is-12"] [element inputView]
                , mkColumn ["is-12"] [element buildView]
                , if nameIden == "" then UI.div else mkColumn ["is-12"] [UI.p #. "is-size-3" #+ [string ("Navn: " ++ nameIden)]]
                ]
    
    inputView2 <- mkSection $ 
                   [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-12"] [element viewSchool] 
                        , mkColumn ["is-12"] [element msg] 
                        , mkColumn ["is-12"] [element builtMsg] 
                        , mkColumn ["is-12"] [element dumpSize] 
                        ]
                    ]

    contents <- UI.div #+ [ element inputView2] --, element viewReset2]
    
    return (contents, input)


setNumber :: MVar ShakeConfig -> Element -> String -> String -> UI Element
setNumber config' input' tea s = do
    (button, view) <- mkButton s s
    on UI.click button $ \_ -> do 
        liftIO $ withMVar config' $ (\conf -> do
                liftIO $ setIdSelection conf (Idd tea)
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
        (Idd idd) <- liftIO $ withMVar config $ (\conf -> getIdSelection conf)
        liftIO $ withMVar config $ (\conf -> setIdSelection conf (Idd ""))
        liftIO $ putStrLn "olll"
        liftIO $liftIO $  putStrLn idd
        case (idd == "" ) of
            False -> liftIO $ withMVar config $ (\conf -> funci conf (Idd idd))
            True -> return ()
    return (button, view)


funci :: ShakeConfig -> Idd -> IO ()
funci config (Idd idd2) = do
    putStrLn "ol"
    --have to look this up from config
    locationFile <- getLocationFile config
    -- kinda bad here
    -- kinda bad here could cause errorr
    find <- case locationFile of 
        NoLocation -> return (Left LocationConfigFileMissing)
        Location xxx -> do
            try $ findPhotographee xxx idd2 :: IO (Either ShakeError Photographee)

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
            try $ findPhotographee2 xxx idd2 :: IO (Either ShakeError Photographee)

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
