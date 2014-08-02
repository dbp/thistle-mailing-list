{-# LANGUAGE Arrows, TemplateHaskell, GADTs, QuasiQuotes, FlexibleInstances,
             DeriveDataTypeable, FlexibleInstances,
             TypeFamilies, MultiParamTypeClasses,OverloadedStrings, LiberalTypeSynonyms #-}
module Member.Types where

import Snap.Plus
import Data.Maybe
import Prelude hiding (id, (++))
import Data.Text (Text)
import Data.Time.Clock
import Opaleye
import Application

import qualified List.Types as L

data Member' a b c d e f = Member' { id :: a
                                   , email :: b
                                   , listId :: c
                                   , failedAt :: d
                                   , unsubscribedAt :: e
                                   , token :: f
                                   }

type Member'' f = Member' (f Int) (f Text) (f Int) (f (Maybe UTCTime)) (f (Maybe UTCTime)) (f Text)
type Member = Member'' I
type MemberMaybe = Member'' Maybe
type MemberSpec = Member'' (Con (Wire String))
type MemberWire = Member'' Wire
type MemberMaybeWire = Member'' MaybeWire
type MemberNew = Member' () Text Int () () ()

$(makeAdaptorAndInstance "pMember" ''Member')

unsubscribePath :: Member -> Text
unsubscribePath (Member' i _ li _ _ t) = "/unsubscribe/" ++ tshow i ++ "/" ++ t

resubscribePath :: Member -> Text
resubscribePath (Member' i _ li _ _ t) = "/resubscribe/" ++ tshow i ++ "/" ++ t

membersTable :: Table MemberWire
membersTable = Table "members" (Member' (Wire "id") (Wire "email") (Wire "list_id") (Wire "failed_at") (Wire "unsubscribed_at") (Wire "token"))

allMembers :: Query MemberWire
allMembers = queryTable membersTable

membersByList :: L.List -> Query MemberWire
membersByList list = proc () -> do member <- allMembers -< ()
                                   i' <- constant (L.id list) -< ()
                                   restrict <<< eq -< (listId member, i')
                                   returnA -< member

getMembersByList :: L.List -> AppHandler [Member]
getMembersByList list = runO (membersByList list)

membersById :: Int -> ExprArr MemberWire (Wire Bool)
membersById i = proc member -> do i' <- econstant i -< ()
                                  eeq -< (i', id member)

membersByIdAndToken :: Int -> Text -> Query MemberWire
membersByIdAndToken i t = proc () -> do member <- allMembers -< ()
                                        i' <- constant i -< ()
                                        t' <- constant t -< ()
                                        restrict <<< eq -< (id member, i')
                                        restrict <<< eq -< (token member, t')
                                        returnA -< member

getMemberByIdAndToken :: Int -> Text -> AppHandler (Maybe Member)
getMemberByIdAndToken i t = listToMaybe <$> runO (membersByIdAndToken i t)

newMember :: MemberNew -> AppHandler (Maybe Member)
newMember (Member' _ em li _ _ _) = listToMaybe <$> insOR membersTable insE retE
  where insE :: Expr MemberMaybeWire
        insE = makeMaybeExpr (Member' Nothing (Just em) (Just li) Nothing Nothing Nothing :: MemberMaybe)
        retE :: ExprArr MemberWire MemberWire
        retE = proc member -> returnA -< member

updateMember :: Member -> AppHandler ()
updateMember member@(Member' i em li fa ua t) =
  void $ updO membersTable updExp (membersById i)
  where updExp :: ExprArr MemberWire MemberMaybeWire
        updExp = makeJustExpr member <<< arr (const ())
