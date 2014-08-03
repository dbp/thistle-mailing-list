{-# LANGUAGE OverloadedStrings #-}
module List.Splices where

import Snap.Plus
import Heist
import Heist.Interpreted

import Application
import qualified Member.Types as M
import qualified Member.Splices as M
import qualified Message.Types as S
import qualified Message.Splices as S
import List.Types

splices :: List -> Splices (Splice AppHandler)
splices list@(List' i n t) = do "id" ## textSplice $ tshow i
                                "name" ## textSplice n
                                "token" ## textSplice t
                                "path" ## textSplice $ listPath list
                                "addMemberPath" ## textSplice $ addMemberPath list
                                "addMessagePath" ## textSplice $ addMessagePath list
                                "members" ## do ms <- lift $ M.getMembersByList list
                                                mapSplices (runChildrenWith . M.splices) ms
                                "messages" ## do ms <- lift $ S.getMessagesByList list
                                                 mapSplices (runChildrenWith . S.splices) ms
