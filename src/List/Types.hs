{-# LANGUAGE Arrows, TemplateHaskell, GADTs, QuasiQuotes, FlexibleInstances,
             DeriveDataTypeable, FlexibleInstances,
             TypeFamilies, MultiParamTypeClasses,OverloadedStrings, LiberalTypeSynonyms #-}
module List.Types where

import Prelude hiding (id, (++))
import Snap.Plus
import Data.Maybe
import Data.Text (Text)
import Opaleye
import Application

data List' a b c = List' { id :: a
                         , name :: b
                         , token :: c
                         }

type List'' f = List' (f Int) (f Text) (f Text)
type List = List'' I
type ListMaybe = List'' Maybe
type ListSpec = List'' (Con (Wire String))
type ListWire = List'' Wire
type ListMaybeWire = List'' MaybeWire
type ListNew = List' () Text ()

$(makeAdaptorAndInstance "pList" ''List')

listPath :: List -> Text
listPath (List' _ nm tok) = "/" ++ nm ++ "/" ++ tok

addMemberPath :: List -> Text
addMemberPath list = listPath list ++ "/members/new"

addMessagePath :: List -> Text
addMessagePath list = listPath list ++ "/messages/new"

listsTable :: Table ListWire
listsTable = Table "lists" (List' (Wire "id") (Wire "name") (Wire "token"))

allLists :: Query ListWire
allLists = queryTable listsTable

listsByNameToken :: Text -> Text -> Query ListWire
listsByNameToken nm tok = proc () -> do list <- allLists -< ()
                                        nm' <- constant nm -< ()
                                        tok' <- constant tok -< ()
                                        restrict <<< eq -< (name list, nm')
                                        restrict <<< eq -< (token list, tok')
                                        returnA -< list

getListByNameToken :: Text -> Text -> AppHandler (Maybe List)
getListByNameToken nm tok = listToMaybe <$> runO (listsByNameToken nm tok)

newList :: ListNew -> AppHandler (Maybe List)
newList (List' _ nm _) = listToMaybe <$> insOR listsTable insE retE
  where insE :: Expr ListMaybeWire
        insE = makeMaybeExpr (List' Nothing (Just nm) Nothing :: ListMaybe)
        retE :: ExprArr ListWire ListWire
        retE = proc list -> returnA -< list
