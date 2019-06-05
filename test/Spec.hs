{-# LANGUAGE OverloadedStrings #-}

import Test.WebDriver
import Test.WebDriver.Commands.Wait

import Control.Concurrent.Async
import Lib

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

runSessionThenClose :: WD a -> IO a
runSessionThenClose action = runSession chromeConfig . finallyClose $ action

main :: IO ()
main = do
    race_ (setup 9000 "test")
        (runSessionThenClose $ do                      
            _ <- setImplicitWait 10000
            openPage "http://localhost:9000"
            _ <- setImplicitWait 10000
            searchInput <- waitUntil 10000 $ findElem ( ByCSS "input[type='text']" )  
            sendKeys "1234" searchInput
            mover <- findElem ( ById "mover" )
            click mover
            _ <- setImplicitWait 10000
            msg <- findElem ( ById "result" )
            result <- getText msg
            expect (result == "Byg færdigt")
        )

