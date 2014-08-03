{-# LANGUAGE OverloadedStrings #-}
module Message.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Snaplet.Heist
import Opaleye
import qualified Data.Text as T
import Text.Digestive
import Text.Digestive.Snap
import Text.Digestive.Heist
import Data.Time.Clock
import Application

import Message.Types
import Message.Forms
import Message.Splices
import qualified List.Types as L

top :: L.List -> AppHandler ()
top list = route [("new", newH list)]

top' :: AppHandler ()
top' = route [("messages/:id", routeMessage)]
  where routeMessage = do i <- getParam' "id"
                          message <- require $ getMessageById i
                          route [("edit", editH message)]

newH :: L.List -> AppHandler ()
newH list = do r <- runForm "new" (newForm list)
               case r of
                 (v, Nothing) -> renderWithSplices "message/form" (digestiveSplices v)
                 (_, Just m) -> do void $ newMessage m
                                   redirect $ L.listPath list

editH :: Message -> AppHandler ()
editH message = do r <- runForm "edit" (editForm message)
                   case r of
                     (v, Nothing) -> renderWithSplices "message/form" (digestiveSplices v)
                     (_, Just m) -> do updateMessage m
                                       list <- require $ L.getListById (listId m)
                                       redirect $ L.listPath list
