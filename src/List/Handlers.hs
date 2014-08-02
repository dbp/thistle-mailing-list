{-# LANGUAGE OverloadedStrings #-}
module List.Handlers where

import Prelude hiding ((++))
import Snap.Plus
import Snap.Snaplet.Heist
import Opaleye
import Text.Digestive
import Text.Digestive.Snap
import Text.Digestive.Heist
import Application

import List.Types
import List.Forms
import List.Splices
import qualified Member.Handlers
import qualified Message.Handlers

top :: AppHandler ()
top = route [("new", newH)
            ,(":name/:token", routeList)]
  where routeList = do nm <- getParam' "name"
                       token <- getParam' "token"
                       list <- require $ getListByNameToken nm token
                       route [("", ifTop $ showH list)
                             ,("members", Member.Handlers.top list)
                             ,("messages", Message.Handlers.top list)
                             ]


newH :: AppHandler ()
newH = do r <- runForm "new" newForm
          case r of
            (v, Nothing) -> renderWithSplices "list/new" (digestiveSplices v)
            (_, Just l) -> do l' <- newList l
                              redirect $ maybe "/" listPath l'

showH :: List -> AppHandler ()
showH l = renderWithSplices "list/show" (splices l)
