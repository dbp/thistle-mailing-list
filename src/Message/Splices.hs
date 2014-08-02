{-# LANGUAGE OverloadedStrings #-}
module Message.Splices where

import Snap.Plus
import Heist
import Heist.Splices
import Heist.Interpreted
import Data.Maybe

import Application
import Message.Types

splices :: Message -> Splices (Splice AppHandler)
splices message@(Message' i s b li sa) =
  do "id" ## textSplice $ tshow i
     "subject" ## textSplice s
     "body" ## textSplice b
     "list-id" ## textSplice $ tshow li
     "is-sent" ## ifISplice (isJust sa)
     "not-sent" ## ifISplice (isNothing sa)
     "editPath" ## textSplice $ editMessagePath message
