{-# LANGUAGE OverloadedStrings #-}
module Member.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Snaplet.Heist
import Opaleye
import Text.Digestive
import Text.Digestive.Snap
import Text.Digestive.Heist
import Data.Time.Clock
import Application

import Member.Types
import Member.Forms
import Member.Splices
import qualified List.Types as L

top :: L.List -> AppHandler ()
top list = route [("new", newH list)]

top' :: AppHandler ()
top' = route [ ("unsubscribe/:id/:token", unsubscribeH)
             , ("resubscribe/:id/:token", resubscribeH)
             ]

newH :: L.List -> AppHandler ()
newH list = do r <- runForm "new" (newForm list)
               case r of
                 (v, Nothing) -> renderWithSplices "member/new" (digestiveSplices v)
                 (_, Just m) -> do void $ newMember m
                                   redirect $ L.listPath list

unsubscribeH :: AppHandler ()
unsubscribeH = do i <- getParam' "id"
                  tok <- getParam' "token"
                  m' <- require $ getMemberByIdAndToken i tok
                  now <- liftIO getCurrentTime
                  let m = m' { unsubscribedAt = Just now }
                  updateMember m
                  renderWithSplices "member/unsubscribe" (splices m)

resubscribeH :: AppHandler ()
resubscribeH = do i <- getParam' "id"
                  tok <- getParam' "token"
                  m' <- require $ getMemberByIdAndToken i tok
                  now <- liftIO getCurrentTime
                  let m = m' { unsubscribedAt = Nothing }
                  updateMember m
                  renderWithSplices "member/resubscribe" (splices m)
