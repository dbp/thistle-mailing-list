{-# LANGUAGE Arrows, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, NoMonomorphismRestriction #-}

module Opaleye (
    module Karamaan.Opaleye.Reexports
  , module Karamaan.Opaleye.Table
  , module Karamaan.Opaleye.SQL
  , module Karamaan.Opaleye.Operators2
  , module Karamaan.Opaleye.RunQuery
  , module Control.Category
  , module Control.Arrow
  , module Database.HaskellDB.Query
  , module Data.Profunctor
  , module Data.Profunctor.Product
  , module Data.Profunctor.Product.Default
  , module Data.Profunctor.Product.TH
  , module Karamaan.Opaleye.Wire
  , module Karamaan.Opaleye.ExprArr
  , module Karamaan.Opaleye.QueryArr
  , module Karamaan.Opaleye.MakeExpr
  , module Karamaan.Opaleye.Manipulation
  , showQuery
  , restrictNullable
  , I
  , MaybeWire
  , Con
  , runO
  , delO
  , insO
  , insOR
  , updO
  ) where

import Prelude hiding (not)

import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime, localTimeToUTC, utc)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Time (ClockTime(TOD), toUTCTime)
import Control.Category ((<<<))

import Control.Arrow (returnA, second)
import Karamaan.Opaleye.Reexports
import Karamaan.Opaleye.Operators2 (not)
import Karamaan.Opaleye.Table (makeTableDef, Table(Table), queryTable)
import Karamaan.Opaleye.SQL (showSqlForPostgresDefault)
import Karamaan.Opaleye.Unpackspec (Unpackspec)
import Karamaan.Opaleye.RunQuery (QueryRunner, fieldQueryRunner)
import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.ExprArr (ExprArr, Expr)
import Karamaan.Opaleye.QueryArr (Query)
import Karamaan.Opaleye.MakeExpr (makeExpr, makeJustExpr, makeMaybeExpr)
import Karamaan.Opaleye.Manipulation (AssocerE, Assocer, TableExprRunner,
                                      executeDeleteConnDef,executeInsertReturningConnDef,
                                      executeUpdateConnDef,executeInsertConnDef,
                                       TableMaybeWrapper)


import Database.HaskellDB.Query (ShowConstant(..))
import Database.HaskellDB.PrimQuery (Literal(DateLit))

import Data.Profunctor
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Profunctor.Product
import Data.Profunctor.Product.Default (Default, def)


import Data.Pool (withResource)
import Control.Lens (view, use)
import Snap (liftIO, snapletValue)
import Database.PostgreSQL.Simple (Connection)
import Snap.Snaplet.PostgresqlSimple hiding (Query)

import Data.Int (Int64)

import Application

type I a = a
type MaybeWire a = Maybe (Wire a)
type Con s a = s

withPgConn :: (Connection -> AppHandler a) -> AppHandler a
withPgConn f  =  do sdb <- use db
                    let pool = pgPool (view snapletValue sdb)
                    withResource pool f

runO :: Default QueryRunner a b => Query a -> AppHandler [b]
runO q = withPgConn $ \con -> liftIO $ runQuery def q con

delO :: Default TableExprRunner t a =>
        Table t -> ExprArr a (Wire Bool) -> AppHandler Int64
delO t e = withPgConn $ \con -> liftIO $ executeDeleteConnDef con t e

insO :: (Default (PPOfContravariant Assocer) t' t',
                 Default TableMaybeWrapper t t')
     => Table t -> Expr t' -> AppHandler Int64
insO t e = withPgConn $ \con -> liftIO $ executeInsertConnDef con t e

insOR :: (Default (PPOfContravariant Assocer) maybeWires maybeWires,
                Default TableMaybeWrapper wires maybeWires,
                Default TableExprRunner wires wires',
                Default (PPOfContravariant AssocerE) resultWires resultWires,
                Default QueryRunner resultWires haskells) =>
               Table wires
               -> Expr maybeWires
               -> ExprArr wires' resultWires
               -> AppHandler [haskells]
insOR t e r = withPgConn $ \con -> liftIO $ executeInsertReturningConnDef con t e r


updO ::  (Default TableExprRunner t u,
          Default (PPOfContravariant Assocer) t' t',
          Default TableMaybeWrapper t t') =>
     Table t -> ExprArr u t' -> ExprArr u (Wire Bool)
       -> AppHandler Int64
updO t e e' = withPgConn $ \con -> liftIO $ executeUpdateConnDef con t e e'

instance ShowConstant Text where
  showConstant = showConstant . unpack

-- NOTE(dbp 2014-04-03): Ridiculous conversion because HaskellDB uses deprecated old-time library.
instance ShowConstant UTCTime where
  showConstant = DateLit . toUTCTime . uncurry TOD . second truncate
                         . properFraction . utcTimeToPOSIXSeconds

instance ShowConstant LocalTime where
  showConstant = DateLit . toUTCTime . uncurry TOD . second truncate
                         . properFraction . utcTimeToPOSIXSeconds . localTimeToUTC utc

showQuery :: Default (PPOfContravariant Unpackspec) a a
          => Query a
          -> IO ()
showQuery = putStrLn . showSqlForPostgresDefault

restrictNullable :: QueryArr (Wire a, Wire (Nullable a)) (Wire a)
restrictNullable = proc (d, i) -> do restrict <<< not <<< isNull -< i
                                     fromNullable -< (d, i)
