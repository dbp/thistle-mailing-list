{-# LANGUAGE OverloadedStrings #-}
module Member.Splices where

import Snap.Plus
import Heist
import Heist.Interpreted

import Application
import Member.Types

splices :: Member -> Splices (Splice AppHandler)
splices (Member' i e li fa ua t) = do "id" ## textSplice $ tshow i
                                      "email" ## textSplice e
                                      "token" ## textSplice t
