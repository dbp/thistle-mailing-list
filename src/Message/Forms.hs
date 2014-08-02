{-# LANGUAGE OverloadedStrings #-}
module Message.Forms where

import Snap.Plus
import Opaleye
import Text.Digestive
import Text.Digestive.Snap
import Text.Digestive.Heist
import Snap.Plus.Forms
import Application

import Message.Types
import qualified List.Types as L

newForm :: L.List -> Form Text AppHandler MessageNew
newForm list = Message' () <$> "subject" .: nonEmptyTextForm
                           <*> "body" .: nonEmptyTextForm
                           <*> pure (L.id list)
                           <*> pure ()
