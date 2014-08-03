{-# LANGUAGE OverloadedStrings #-}
module Message.Forms where

import Snap.Plus
import Opaleye
import qualified Data.Text as T
import Text.Digestive
import Text.Digestive.Snap
import Text.Digestive.Heist
import Snap.Plus.Forms
import Application

import Message.Types
import qualified List.Types as L

newForm :: L.List -> Form Text AppHandler MessageNew
newForm list = Message' () <$> "subject" .: nonEmptyTextForm
                           <*> "body" .: nonEmptyTextForm
                           <*> pure (L.id list)
                           <*> pure ()

editForm :: Message -> Form Text AppHandler Message
editForm m@(Message' _ _ _ _ (Just _)) = pure m
editForm (Message' i s b li sa) = Message' i <$> "subject" .: nonEmpty (text (Just s))
                                             <*> "body" .: nonEmpty (text (Just b))
                                             <*> pure li
                                             <*> pure sa
