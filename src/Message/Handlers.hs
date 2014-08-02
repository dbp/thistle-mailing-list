{-# LANGUAGE OverloadedStrings #-}
module Message.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Snaplet.Heist
import Opaleye
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

newH :: L.List -> AppHandler ()
newH list = do r <- runForm "new" (newForm list)
               case r of
                 (v, Nothing) -> renderWithSplices "message/new" (digestiveSplices v)
                 (_, Just m) -> do void $ newMessage m
                                   redirect $ L.listPath list
