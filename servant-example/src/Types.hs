{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson

data User = User
  { userName :: String
  , userEmail :: String
  , userAge :: Int }
  deriving (Show)

instance ToJSON User where
  toJSON u = object
    [ "name" .= userName u
    , "email" .= userEmail u
    , "age" .= userAge u ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    n <- o .: "name"
    e <- o .: "email"
    a <- o .: "age"
    return $ User n e a
