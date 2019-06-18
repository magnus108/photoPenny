{-# LANGUAGE OverloadedStrings #-}
module Main
    ( mainSection 
    ) where
import Elements
import PhotoShake.Dagsdato

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


mainSection :: FilePath -> FilePath -> ShakeConfig -> Window -> UI Element
mainSection _ _ config _ = do

    ident <- liftIO $ newIORef ""
    (_, buildView) <- mkBuild config ident

    (input, inputView) <- mkInput "Elev nr:"
    on UI.keyup input $ \_ -> liftIO . writeIORef ident =<< get value input

    built <- liftIO $ getBuilt config
    builtMsg <- UI.p # set text (case built of
                                    NoBuilt -> ""
                                    NoFind s -> s
                                    Built _ s -> s)

    msg <- UI.p # set text (case built of
                                    NoBuilt -> ""
                                    NoFind _ -> ""
                                    Built p _ -> _name p)



    (_, viewReset)<- mkReset config

    viewReset2 <- mkSection $ 
                    [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-4"] [element viewReset] ]
                    ]
    

    sessions <- liftIO $ getSessions config

    let wats = (\zipper item -> do
                    input' <- UI.button # set UI.type_ "button" # set UI.name "sessions"
                        
                    let label = case item of
                                    Group -> "Gruppe"
                                    Single -> "Enkelt"
                    
                    button <- UI.button #. "button" #+ [string label]
                    
                    on UI.click button $ \_ -> liftIO $ setSession config $ Sessions zipper

                    return button
            ) 

    -- badness 3000
    let s = case sessions of
            NoSessions -> UI.div #+ [ string "session ikke angivet" ]
            Sessions y ->
                UI.div #. "buttons has-addons" #+ (fmap snd $ filter (\xxx -> case (fst xxx) of 
                                                        Kindergarten _ -> case focus y of
                                                                Kindergarten _ -> True
                                                                School -> False
                                                        School -> School == focus y
                                                    ) $ toList $ y =>> (\zipper ->
                            case (focus zipper) of
                                Kindergarten t -> (Kindergarten t, wats zipper t) --her skal noget andet på.
                                School -> 
                                    (School, element buildView)
                            ) )
    
    

    -- antal billeder
    dumps <- liftIO $ getDumpFiles config
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
                        [ mkColumn ["is-4"] [element inputView]
                        , mkColumn ["is-12"] [s] 
                        , mkColumn ["is-12"] [element msg] 
                        , mkColumn ["is-12"] [element builtMsg]
                        ]
                    ]

    UI.div #+ [ element inputView2, element dumpSize, element viewReset2]


mkReset :: ShakeConfig -> UI (Element, Element)
mkReset config = do
    (button, view) <- mkButton "reset" "Reset konfiguration"
    on UI.click button $ \_ -> liftIO $ resetIt config
    return (button, view)


resetIt :: ShakeConfig -> IO ()
resetIt config = 
        setDump config NoDump
        >> setDagsdato config NoDagsdato
        >> setPhotographers config NoPhotographers
        >> setLocation config NoLocation
        >> setSession config NoSessions
        >> setShooting config NoShootings
        >> setDoneshooting config NoDoneshooting

mkBuild :: ShakeConfig -> IORef String -> UI (Element, Element)
mkBuild config idd = do
    --- with pattern
    (button, view) <- mkButton "mover" "Flyt filer"
    on UI.click button $ \_ -> liftIO $ funci config idd
    return (button, view)


funci :: ShakeConfig -> (IORef String) -> IO ()
funci config idd = do
    --have to look this up from config
    idd2 <- readIORef idd
    locationFile <- getLocationFile config
    -- kinda bad here
    -- kinda bad here could cause errorr
    find <- case locationFile of 
        NoLocation -> return (Left LocationConfigFileMissing)
        Location xxx -> do
            try $ findPhotographee xxx idd2 :: IO (Either ShakeError Photographee)

    case find of
            Left errMsg -> do
                    setBuilt config (NoFind (show errMsg))

            Right photographee -> do
                    time <- getCurrentTime
                    -- wtf????
                    case locationFile of 
                        NoLocation -> do 
                            setBuilt config (NoFind (show LocationConfigFileMissing))
                    
                        Location xxx -> do
                            build <- try $ myShake config photographee (takeBaseName xxx) time :: IO (Either ShakeError ())
                            case build of
                                    Left errMsg -> do
                                        setBuilt config (NoFind (show errMsg))  
                                    Right _ -> do
                                        return () 
