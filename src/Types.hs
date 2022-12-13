{-# LANGUAGE DeriveGeneric #-}

module Types (
    Region (..),
    Price (..),
    Info (..),
    Data (..)
) where

import GHC.Generics

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

import Data.Aeson

data Region = Region {
    id :: Int,
    region :: String,
    area_ :: String,
} deriving (Show)

data Price = Price {
    date :: String,
    average :: Float,
    monthly :: Maybe Float,
    annual :: Maybe Float
    sa_ :: Maybe Float
    fk_region :: Int  -- Auto increment
} deriving (Show)

data Info = Info {
    region :: String,
    date :: String,
    area :: String,
    average :: Float,
    monthly :: Maybe Float,
    annual :: Maybe Float
} deriving (Show, Generic)

data Data = Data {
    data :: [Info]
} deriving (Show, Generic)


instance FromRow Info where 
    fromRow = Info <$> field <> field <> field <> field <> field <> field

instance FromRow Price where
    fromRow = Price <$> field <> field <> field <> field 

instance ToRow Price where
    toRow (Price av mon an sa)
        = toRow (av, mon, an, sa)

instance FromRow Region where
    fromRow = Region <$> field <*> field 

instance ToRow Region where
    toRow (Region re ar)
        = toRow (re, ar)

instance FromRow Date where
    fromRow = Date <$> field 

instance ToRow Date where
    toRow (Date da)
        = toRow (da)


changeNames :: String -> String
changeNames "region" = "region_name"
changeNames "area" = "area_code"
changeNames "average" = "average_prices"
changeNames "monthly" = "monthly_change"
changeNames "annual" = "annual_change"
changeNmes other = other

customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = renameFields
}

instance FromJSON Info where
    parseJSON = genericParseJSON customOptions

instance FromJSON Info