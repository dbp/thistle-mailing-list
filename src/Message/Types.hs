{-# LANGUAGE Arrows, TemplateHaskell, GADTs, QuasiQuotes, FlexibleInstances,
             DeriveDataTypeable, FlexibleInstances,
             TypeFamilies, MultiParamTypeClasses,OverloadedStrings, LiberalTypeSynonyms #-}
module Message.Types where

import Snap.Plus
import Data.Maybe
import Prelude hiding (id, (++))
import Data.Text (Text)
import Data.Time.Clock
import Opaleye
import Application

import qualified List.Types as L

data Message' a b c d e = Message' { id :: a
                                   , subject :: b
                                   , body :: c
                                   , listId :: d
                                   , sentAt :: e
                                   }

type Message'' f = Message' (f Int) (f Text) (f Text) (f Int) (f (Maybe UTCTime))
type Message = Message'' I
type MessageMaybe = Message'' Maybe
type MessageSpec = Message'' (Con (Wire String))
type MessageWire = Message'' Wire
type MessageMaybeWire = Message'' MaybeWire
type MessageNew = Message' () Text Text Int ()

$(makeAdaptorAndInstance "pMessage" ''Message')

editMessagePath :: Message -> Text
editMessagePath (Message' i _ _ _ _ ) = "/message/" ++ tshow i ++ "/edit"

messagesTable :: Table MessageWire
messagesTable = Table "messages" (Message' (Wire "id") (Wire "subject") (Wire "body") (Wire "list_id") (Wire "sent_at"))

allMessages :: Query MessageWire
allMessages = queryTable messagesTable

messagesByList :: L.List -> Query MessageWire
messagesByList list = proc () -> do message <- allMessages -< ()
                                    i' <- constant (L.id list) -< ()
                                    restrict <<< eq -< (listId message, i')
                                    returnA -< message

getMessagesByList :: L.List -> AppHandler [Message]
getMessagesByList list = runO (messagesByList list)

messagesById :: Int -> ExprArr MessageWire (Wire Bool)
messagesById i = proc message -> do i' <- econstant i -< ()
                                    eeq -< (i', id message)

messagesById' :: Int  -> Query MessageWire
messagesById' i = proc () -> do message <- allMessages -< ()
                                i' <- constant i -< ()
                                restrict <<< eq -< (id message, i')
                                returnA -< message

getMessageById :: Int -> Text -> AppHandler (Maybe Message)
getMessageById i t = listToMaybe <$> runO (messagesById' i)

newMessage :: MessageNew -> AppHandler (Maybe Message)
newMessage (Message' _ s b li _) = listToMaybe <$> insOR messagesTable insE retE
  where insE :: Expr MessageMaybeWire
        insE = makeMaybeExpr (Message' Nothing (Just s) (Just b) (Just li) Nothing :: MessageMaybe)
        retE :: ExprArr MessageWire MessageWire
        retE = proc message -> returnA -< message

updateMessage :: Message -> AppHandler ()
updateMessage message =
  void $ updO messagesTable updExp (messagesById $ id message)
  where updExp :: ExprArr MessageWire MessageMaybeWire
        updExp = makeJustExpr message <<< arr (const ())
