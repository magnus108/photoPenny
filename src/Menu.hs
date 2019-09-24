module Menu
    ( mkMenu
    ) where

import qualified Control.Concurrent.Chan as Chan

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified PhotoShake.State as S

import Utils.Comonad
import Utils.ListZipper
import qualified Message as Msg

import Elements


setStates :: Chan.Chan Msg.Message -> S.States -> UI ()
setStates msgChan states = liftIO $ Chan.writeChan msgChan (Msg.setStates states) 


mkMenu :: Chan.Chan Msg.Message -> ListZipper S.State -> UI Element
mkMenu msgs states = do
    let buttons = states =>> (\ states' -> do
            button <- UI.button # set (attr "id") ("tab" ++ show (focus states')) #. "button" #+ [string (show (focus states'))]

            button' <- if (states' == states) then
                set (UI.attr  "class") "button is-dark is-selected" (element button)
            else
                return button

            on UI.click button' $ \_ -> do
                setStates msgs (S.States states')

            return button'
            )

    mkSection [UI.div #. "buttons has-addons" #+ (toList buttons)]


