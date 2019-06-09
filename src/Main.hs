{-# LANGUAGE OverloadedStrings #-}
module Main
    ( mainSection 
    ) where
import Elements
import PhotoShake.Dagsdato
import Control.Monad 

import PhotoShake
import PhotoShake.ShakeConfig
import PhotoShake.Doneshooting
import PhotoShake.Shooting
import PhotoShake.Session
import PhotoShake.Location
import PhotoShake.Photographer
import PhotoShake.Dump
import PhotoShake.Photographee

import Data.Time.Clock
import Control.Exception
import System.FilePath

import PhotoShake.ShakeError

import Data.IORef

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Utils.ListZipper
import Utils.Comonad


mainSection :: FilePath -> ShakeConfig -> Window -> UI Element
mainSection root config w = do

    err <- UI.p 
    msg <- UI.p # set (attr "id") "result"
    ident <- liftIO $ newIORef ""
    (_, buildView) <- mkBuild config ident w err msg

    (input, inputView) <- mkInput "Elev nr:"
    on UI.keyup input $ \_ -> liftIO . writeIORef ident =<< get value input

    inputView2 <- mkSection $ 
                   [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-4"] [element inputView]
                        , mkColumn ["is-12"] [element buildView]
                        , mkColumn ["is-12"] [element err, element msg] 
                        ]
                    ]


    (_, viewReset)<- mkReset config

    viewReset2 <- mkSection $ 
                    [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-4"] [element viewReset] ]
                    ]
    
    sessions <- liftIO $ getSessions config


    let wats = (\zipper items -> do
                    input' <- UI.input # set UI.type_ "radio" # set UI.name "sessions"
                    input <- if (zipper == items) then
                            set (UI.attr "checked") "" (element input')
                        else
                            return input'
                        
                    on UI.checkedChange input $ \_ -> liftIO $ setSession config $ Sessions zipper

                    label <- UI.string $ case (focus zipper) of
                                        School -> "FA"
                                        Kindergarten y ->
                                            case y of 
                                                    Group -> "Gruppe"
                                                    Single -> "Enkelt"

                    view <- UI.label #. "radio" #+ [element input, element label]
                    return view
            ) 

    -- badness 3000
    let s = case sessions of
            NoSessions -> ListZipper [] UI.div []
            Sessions y ->
                y =>> (\zipper ->
                        case (focus zipper) of
                            Kindergarten x -> wats zipper y
                            School -> UI.div
                        )

    let ss = case sessions of
            NoSessions -> ListZipper [] UI.div []
            Sessions y ->
                case (focus y) of
                        Kindergarten x -> s 
                        School -> ListZipper [] UI.div []

        
    
    ssss <- mkSection 
                    [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-12"] [UI.div #. "control" #+ (toList ss) ] ]
                    ]
    
    UI.div #+ [element ssss, element inputView2, element viewReset2]


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

mkBuild :: ShakeConfig -> IORef String -> Window -> Element -> Element -> UI (Element, Element)
mkBuild config idd w err msg = do
    --- with pattern
    (button, view) <- mkButton "mover" "Flyt filer"
    on UI.click button $ \_ -> liftIO $ funci config idd w err msg
    return (button, view)


funci :: ShakeConfig -> (IORef String) -> Window -> Element -> Element -> IO ()
funci config idd w err msg = do
    --have to look this up from config
    idd2 <- readIORef idd
    locationFile <- getLocationFile config
    -- kinda bad here
    -- kinda bad here
    -- kinda bad here
    -- kinda bad here
    -- kinda bad here
    -- kinda bad here
    -- kinda bad here
    -- kinda bad here could cause errorr
    find <- case locationFile of 
        NoLocation -> return (Left LocationConfigFileMissing)
        Location xxx -> do
            try $ findPhotographee xxx idd2 :: IO (Either ShakeError Photographee)

    case find of
            Left errMsg -> do
                    _ <- runUI w $ element err # set text (show errMsg)
                    return ()

            Right photographee -> do
                    time <- getCurrentTime
                    -- wtf????
                    case locationFile of 
                        NoLocation -> 
                            void $ runUI w (element err # set text (show LocationConfigFileMissing))
                    
                        Location xxx -> do
                            build <- try $ myShake config photographee (takeBaseName xxx) time :: IO (Either ShakeError ())
                            let ans = case build of
                                    Left errMsg -> element err # set text (show errMsg)
                                    Right _ -> element msg # set text ("Byg færdigt for" ++ " " ++ (_name photographee)) --- OVERVEJ en trie
                            -- reset
                            _ <- runUI w (element err # set text "")
                            _ <- runUI w (element msg # set text "")
                            _ <- runUI w ans
                            return ()
