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
import qualified PhotoShake.Id as Id
import PhotoShake.Doneshooting
import PhotoShake.Shooting
import PhotoShake.Session
import qualified PhotoShake.Location as Location
import PhotoShake.Photographer
import PhotoShake.Dump
import qualified PhotoShake.Photographee as Photographee
import PhotoShake.Build

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


    let isBuilding = False

    (builderButton, buildView) <- mkBuild config'
    
    identi <- liftIO $ withMVar config' $ (\conf -> getId conf)


    input <- UI.input #. "input" # set (attr "id") "fotoId" #  set UI.type_ "text" # set (attr "value") (Id.toString identi)
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
                setId conf (Id.yesId val))

    builtMsg <- UI.div

    msg <- UI.div 




    label <- mkLabel "Antal billeder i dump:"
    dumpSize <- mkColumn ["is-12"] 
                    [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-12"] [ element label ]
                        , mkColumn ["is-12"] [ UI.string "gg" #. "is-size-1 has-text-danger has-text-weight-bold" ]
                        ]
                    ]







    val <- liftIO $ withMVar config' $ (\conf -> getId conf)

    idenName <- liftIO $ do
        locationFile <- try $ withMVar config' $ (\conf -> do
                getLocationFile conf 
                ) :: IO (Either ShakeError Location.Location)
        case locationFile of 
            Left e -> newIORef ""
            Right loc -> do
                Location.location (newIORef "") (\xxx -> do
                            liftIO $ withMVar config' $ (\conf -> do
                                    what <- Id.id (return Nothing) (\zzz -> Photographee.findPhotographee3 xxx zzz) val
                                    case what of
                                        Nothing -> newIORef ""
                                        Just iddd ->  newIORef (Photographee._name iddd))
                            ) loc

    on UI.keyup input' $ \keycode -> when (keycode /= 13) $ do
        val <- get value input
        liftIO $ withMVar config' $ (\conf -> do
                setId conf (Id.yesId val))
        locationFile <- liftIO $ try $ withMVar config' $ (\conf -> do
                getLocationFile conf 
                ) :: UI (Either ShakeError Location.Location)
        case locationFile of 
            Left e -> return ()
            Right loc -> do
                Location.location (return () )--return (Left LocationConfigFileMissing)
                    (\ xxx -> do
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
                liftIO $ setId conf (Id.yesId tea)
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
        id <- liftIO $ withMVar config $ (\conf -> getId conf)
        liftIO $ withMVar config $ (\conf -> setId conf (Id.noId))
        Id.id (liftIO $ withMVar config $ (\conf -> funci conf id)) (\x -> return ()) id
    return (button, view)


funci :: ShakeConfig -> Id.Id -> IO ()
funci config x = do
    return ()
    {-
    Id.id (setBuilt' config (NoFind (show "Elev ikke fundet"))) 
        (\i -> do
        --have to look this up from config
        locationFile <- getLocationFile config
        -- kinda bad here
        -- kinda bad here could cause errorr
        find <- 
            Location.location (return (Left LocationConfigFileMissing)) (\ xxx -> do
                
                --try $ Photographee.findPhotographee xxx i :: IO (Either ShakeError Photographee.Photographee)) locationFile 

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
        ) x
-}

