{-# LANGUAGE OverloadedStrings #-}
module Member.Forms where

import Snap.Plus
import Opaleye
import Text.Digestive
import Text.Digestive.Snap
import Text.Digestive.Heist
import Snap.Plus.Forms
import Application

import Member.Types
import qualified List.Types as L

newForm :: L.List -> Form Text AppHandler MemberNew
newForm list = Member' () <$> "email" .: requiredForm "Email is required." (emailFormSingle Nothing)
                          <*> pure (L.id list)
                          <*> pure ()
                          <*> pure ()
                          <*> pure ()
