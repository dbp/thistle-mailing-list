{-# LANGUAGE Arrows, TemplateHaskell, GADTs, QuasiQuotes, FlexibleInstances,
             DeriveDataTypeable, FlexibleInstances,
             TypeFamilies, MultiParamTypeClasses,OverloadedStrings, LiberalTypeSynonyms #-}
module List.Types where

import Prelude hiding (id)
import Data.Text (Text)
import Opaleye

data List' a b c = List' { id :: a
                         , name :: b
                         , token :: c
                         }

type List'' f = List' (f Int) (f Text) (f Text)
type List = List'' I
type ListSpec = List'' (Const (Wire String))
type ListWire = List'' Wire
type ListMaybeWire = List'' MaybeWire
