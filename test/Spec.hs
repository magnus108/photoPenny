{-# LANGUAGE OverloadedStrings #-}

import Test.WebDriver hiding (setLocation)
import Control.Monad.Base
import Test.WebDriver.Commands.Wait
import Utils.FP
import qualified Control.Concurrent.Chan as Chan

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import qualified Lib2 as L

import System.Directory


import Test.WebDriver.Common.Keys (enter)


import Message (block, setDump)
import State

import PhotoShake.ShakeConfig hiding (setDump)
import PhotoShake.Photographee
import PhotoShake.Built


import Control.Monad

import Utils.ListZipper
import qualified Utils.Actions as A
import qualified Model.E as A
import Utils.Env


import Control.Concurrent

import Utils.ListZipper
import Utils.FP
import State 

import qualified PhotoShake.Dump as D

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

runSessionThenClose :: WD a -> IO a
runSessionThenClose action = runSession chromeConfig . finallyClose $ action

main :: IO ()
main = do
    config <- toShakeConfig Nothing "test/config.cfg" -- Bad and unsafe
    -- dangerous difference between these params
    app <- newMVar $ A.app $ env A.production (A.model Nothing D.noDump "test/config" (fp $ start "") config)
    messages <- Chan.newChan

    race_ (L.main 9000 messages app )
        (runSessionThenClose $ do                      
            openPage "http://localhost:9000"
            empty <- liftBase newEmptyMVar

            --fuckthis
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty
            waitUntil 10000000 $ findElem ( ById "tabDump" ) >>= click
            --fuckthis

            forM_ [1..60] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setDump $ D.yesDump "/home/magnus/Downloads/Magnus Renamed/what"

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "dumpPath" ) >>= getText >>= \x -> expect (x == "/home/magnus/Downloads/Magnus Renamed/what")

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setDump $ D.noDump 

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "dumpMissing" )
                )
        )

    race_ (L.main 9000 messages app )
        (runSessionThenClose $ do                      
            openPage "http://localhost:9000"

            empty <- liftBase newEmptyMVar
            forM_ [1..600] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 100000000 $ findElem ( ById "tabDump" ) >>= click

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                waitUntil 100000000 $ findElem ( ById "tabPhotographer" ) >>= click
                )
            return True
        )




            --fmap and $  forM [1..40] (\iter -> do
                    --_ <- liftBase $ putMVar app' app''
{-
                    app'' <- liftBase $ takeMVar app'
                    empty <- liftBase newEmptyMVar
                    liftBase $ writeChan messages (block empty)
                    liftBase $ readMVar empty
                    waitUntil 10000000 $ findElem ( ById "tabDoneshooting" ) >>= click
                    _ <- liftBase $ putMVar app' app''
                    app'' <- liftBase $ takeMVar app'
                    empty <- liftBase newEmptyMVar
                    liftBase $ writeChan messages (block empty)
                    liftBase $ readMVar empty
                    waitUntil 10000000 $ findElem ( ById "tabShooting" ) >>= click
                    _ <- liftBase $ putMVar app' app''
                    app'' <- liftBase $ takeMVar app'
                    empty <- liftBase newEmptyMVar
                    liftBase $ writeChan messages (block empty)
                    liftBase $ readMVar empty
                    waitUntil 10000000 $ findElem ( ById "tabLocation" ) >>= click
                    _ <- liftBase $ putMVar app' app''
                    app'' <- liftBase $ takeMVar app'
                    empty <- liftBase newEmptyMVar
                    liftBase $ writeChan messages (block empty)
                    liftBase $ readMVar empty
                    waitUntil 10000000 $ findElem ( ById "tabMain" ) >>= click
                    liftBase $ putMVar app' app''
                    -}
           --         )
                

                {-
                liftBase $ forM_ [1..40] (\x -> do
                    writeFile ("test/files/input" ++ (show x) ++ ".CR2") (show [1..8000000]) -- create the source file
                    writeFile ("test/files/input" ++ (show x) ++ ".JPG") (show [1..8000000])-- create the source file
                    )

                liftBase $ setIdSelection config (Idd "")
                liftBase $ setBuilt' config (NoBuilt)

                liftBase $ threadDelay 4000000
                searchInput <- waitUntil 100000 $ findElem ( ById "fotoId" )
                sendKeys "1234" searchInput

                liftBase $ threadDelay 400000 -- vent for text alfredvestved
                --mover <- waitUntil 100000 $ findElem ( ById "builderButton" )
                --click mover
                searchInput2 <- waitUntil 10000 $ findElem ( ById "fotoId" )
                sendKeys enter searchInput2 

                liftBase $ putStrLn "gg2"
                liftBase $ threadDelay 30000000
                liftBase $ putStrLn "gg"
                liftBase $ threadDelay 100000
                liftBase $ putStrLn "gg"
                -}
                --msg <- waitUntil 1000000 $ findElem ( ById "result" )
                --result <- getText msg
                --expect (result == "Færdig")

                --WWHAT???
                --liftBase $ removeDirectoryRecursive "/home/magnus/Documents/projects/photoPenny/test/doneshooting/"
                --liftBase $ removeDirectoryRecursive "/home/magnus/Documents/projects/photoPenny/test/dagsdato/"


        {-
            _ <- liftBase $ setDump config $ NoDump -- reset or i will get stales
            _ <- liftBase $ setDagsdato config $ NoDagsdato -- reset or i will get stales
            _ <- liftBase $ setPhotographers config $ NoPhotographers -- reset or i will get stales
            _ <- liftBase $ setDoneshooting config $ NoDoneshooting -- reset or i will get stales
            _ <- liftBase $ setDagsdato config $ NoDagsdato -- reset or i will get stales
            _ <- liftBase $ setShooting config $ NoShootings -- reset or i will get stales
            _ <- liftBase $ setSession config $ NoSessions -- reset or i will get stales
            _ <- liftBase $ setLocation config $ NoLocation -- reset or i will get stales

            openPage "http://localhost:9000"
            --DUMP
            --DUMP
            tabDump <- waitUntil 100000 $ findElem ( ById "tabDump" ) 
            _ <- click tabDump
            _ <- liftBase $ setDump config $ Dump "testMappe" -- maybe i should inject this service
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "dumpOK")

            --DAGSDATO
            --DAGSDATO
            tabDagsdato <- waitUntil 100000 $ findElem ( ById "tabDagsdato" ) 
            _ <- click tabDagsdato
            _ <- liftBase $ setDagsdato config $ Dagsdato "testMappe" -- maybe i should inject this service
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "dagsdatoOK")

            --PHOTOGRAPHER
            --PHOTOGRAPHER
            tabPhotographer <- waitUntil 100000 $ findElem ( ById "tabPhotographer" ) 
            _ <- click tabPhotographer
            _ <- liftBase $ setPhotographers config $ Photographers (ListZipper [] (Photographer "Joe Test" "JT") []) -- maybe i should inject this service
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "photographerOK")
            
            --DONESHOOTING
            --DONESHOOTING
            tabDoneshooting <- waitUntil 100000 $ findElem ( ById "tabDoneshooting" ) 
            _ <- click tabDoneshooting
            _ <- liftBase $ setDoneshooting config $ Doneshooting "testMappe"
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "doneshootingOK")

            --SHOOTING
            --SHOOTING
            tabShooting <- waitUntil 100000 $ findElem ( ById "tabShooting" ) 
            _ <- click tabShooting
            _ <- liftBase $ setShooting config $ Shootings  (ListZipper [] Normal [])
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "shootingOK")
            
            --SESSION
            --SESSION
            tabSession <- waitUntil 100000 $ findElem ( ById "tabSession" ) 
            _ <- click tabSession
            _ <- liftBase $ setSession config $ Sessions (ListZipper [] School [])
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "sessionOK")

            --LOCATION
            --LOCATION
            tabLocation <- waitUntil 100000 $ findElem ( ById "tabLocation" ) 
            _ <- click tabLocation
            _ <- liftBase $ setLocation config $ Location "testlocation.csv"
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "locationOK")



            tabMain <- waitUntil 100000 $ findElem ( ById "tabMain" ) 
            _ <- click tabMain 
            searchInput <- waitUntil 10000 $ findElem ( ByCSS "input[type='text']" )  
            sendKeys "1234" searchInput
            mover <- waitUntil 100000 $ findElem ( ById "mover" )
            click mover
            msg <- findElem ( ById "result" )
            result <- getText msg
            expect (result == "Byg færdigt")
            _ <- liftBase $ putStrLn "complete"
            expect True
        )
        -}
