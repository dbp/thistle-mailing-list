{-# LANGUAGE OverloadedStrings #-}
module Member.Splices where

import Snap.Plus
import Heist
import Heist.Splices
import Heist.Interpreted
import Data.Maybe

import Application
import Member.Types

splices :: Member -> Splices (Splice AppHandler)
splices member@(Member' i e li fa ua t) =
  do "id" ## textSplice $ tshow i
     "email" ## textSplice e
     "token" ## textSplice t
     "active" ## ifISplice (isNothing fa && isNothing ua)
     "is-subscribed" ## ifISplice (isNothing ua)
     "is-failing" ## ifISplice (isNothing fa)
     "unsubscribePath" ## textSplice $ unsubscribePath member
     "resubscribePath" ## textSplice $ resubscribePath member
