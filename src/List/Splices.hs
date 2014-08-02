{-# LANGUAGE OverloadedStrings #-}
module List.Splices where

import Snap.Plus
import Heist
import Heist.Interpreted

import Application
import qualified Member.Types as M
import qualified Member.Splices as M
import List.Types

splices :: List -> Splices (Splice AppHandler)
splices list@(List' i n t) = do "id" ## textSplice $ tshow i
                                "name" ## textSplice n
                                "token" ## textSplice t
                                "addMemberPath" ## textSplice $ addMemberPath list
                                "members" ## do ms <- lift $ M.getMembersByList list
                                                mapSplices (runChildrenWith . M.splices) ms
