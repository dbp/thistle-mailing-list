{-# LANGUAGE OverloadedStrings #-}
module List.Forms where

import Snap.Plus
import Opaleye
import Text.Digestive
import Text.Digestive.Snap
import Text.Digestive.Heist
import Snap.Plus.Forms
import Application

import List.Types

newForm :: Form Text AppHandler ListNew
newForm = List' () <$> "name" .: nonEmpty (slugForm Nothing)
                   <*> pure ()
