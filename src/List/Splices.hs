{-# LANGUAGE OverloadedStrings #-}
module List.Splices where

import Snap.Plus
import Heist
import Heist.Interpreted

import Application
import List.Types

splices :: List -> Splices (Splice AppHandler)
splices (List' i n t) = do "id" ## textSplice $ tshow i
                           "name" ## textSplice n
                           "token" ## textSplice t
