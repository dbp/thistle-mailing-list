{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Snap.Plus.Forms where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Lens
import Control.Applicative
import Text.Digestive
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Data.Char
import Network.DNS.Lookup
import Network.DNS.Resolver
import Network.DNS.Types

import Application
import Snap.Plus

requiredForm :: Text -> Form Text AppHandler (Maybe a) -> Form Text AppHandler a
requiredForm msg = validate (maybe (Error msg) Success)

nameForm :: Maybe Text -> Form Text AppHandler Text
nameForm = nonEmpty . text

emailForm :: Maybe Text -> Form Text AppHandler (Maybe Text)
emailForm t = emailDnsCheck $ fst <$>
              (matching $ (,) <$> "address" .: emailValidateSimple (optionalText t)
                              <*> "confirm" .: emailValidateSimple (optionalText t))
  where matching = check "Email addresses do not match."
                         (maybe True (uncurry (==)) . uncurry (liftM2 (,)))

emailFormSingle :: Maybe Text -> Form Text AppHandler (Maybe Text)
emailFormSingle t = emailDnsCheck $ "address" .: emailValidateSimple (optionalText t)

emailValidateSimple :: Form Text AppHandler (Maybe Text) -> Form Text AppHandler (Maybe Text)
emailValidateSimple = check "Email address not valid (missing @)." (maybe True ("@" `T.isInfixOf`))

emailDnsCheck :: Form Text AppHandler (Maybe Text) -> Form Text AppHandler (Maybe Text)
emailDnsCheck = checkM "Email address domain (after the @) not valid." $ maybe (return True) $ \e ->
                       do seed <- use dns
                          let host = T.encodeUtf8 (T.drop 1 (snd (T.breakOn "@" e)))
                          res <- liftIO $ withResolver seed (`lookupMX` host)
                          case res of
                            Left err -> case err of
                                          IllegalDomain -> return False
                                          NameError -> return False
                                          _ -> return True
                            Right [] -> return False
                            _ -> return True

passwordForm :: Form Text AppHandler Text
passwordForm = nonEmptyTextForm

nonEmpty :: Form Text AppHandler Text -> Form Text AppHandler Text
nonEmpty = check "Must not be blank" tNotNull

nonEmptyTextForm :: Form Text AppHandler Text
nonEmptyTextForm = nonEmpty (text Nothing)


slugForm :: Formlet Text AppHandler Text
slugForm t = T.toLower <$> check "Cannot have spaces" (not . T.isInfixOf " ") (text t)


deleteForm :: Text -> Form Text AppHandler Bool
deleteForm t = snd <$> ((,) <$> "prompt" .: text (Just t)
                            <*> "confirm" .: bool Nothing)

numericTextForm :: Form Text AppHandler Text
numericTextForm = check "Must be all numbers" ((all isDigit).T.unpack) (text Nothing)
