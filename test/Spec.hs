{-# LANGUAGE OverloadedStrings #-}

import Test.WebDriver

import Control.Concurrent.Async
import Lib

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

runSessionThenClose :: WD a -> IO a
runSessionThenClose action = runSession chromeConfig . finallyClose $ action

main :: IO ()
main = do
    race_ (setup 9000 "")
        (runSessionThenClose $ do                      
            _ <- setImplicitWait 1230023
            openPage "http://localhost:9000"
            _ <- setImplicitWait 1234
            searchInput <- findElem ( ByCSS "input[type='text']" )  
            sendKeys "1234x!" searchInput)

