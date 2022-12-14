{-# LANGUAGE DeriveGeneric #-}

module Types (
    Region (..),
    Price (..),
    Record (..),
    Records (..)
) where

import GHC.Generics

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

import Data.Aeson

data Region = Region {
    id_ :: Int,
    region_ :: String,
    area_ :: String
} deriving (Show)

data Price = Price {
    date_ :: String,
    average_ :: Float,
    monthly_ :: Maybe Float,
    annual_ :: Maybe Float,
    sa_ :: Maybe Float,
    fk_region :: Int
} deriving (Show)

data Record = Record {
    region :: String,
    area :: String,
    date :: String,
    average :: Float,
    monthly :: Maybe Float,
    annual :: Maybe Float,
    sa :: Maybe Float
} deriving (Show, Generic)

data Records = Records {
    records :: [Record]
} deriving (Show, Generic)

changeNames :: String -> String
changeNames "date" = "Date"
changeNames "region" = "Region_Name"
changeNames "area" = "Area_Code"
changeNames "average" = "Average_Price"
changeNames "monthly" = "Monthly_Change"
changeNames "annual" = "Annual_Change"
changeNames "sa" = "Average_Price_SA"
changeNmes other = other

customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = changeNames
}

instance ToRow Region where
    toRow (Region reid re ar)
        = toRow (reid, re, ar)

instance FromRow Region where
    fromRow = Region <$> field <*> field <*> field 

instance FromRow Price where
    fromRow = Price <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Price where
    toRow (Price d av mon an sa fk)
        = toRow (d, av, mon, an, sa, fk)

instance FromRow Record where 
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromJSON Record where
    parseJSON = genericParseJSON customOptions

instance FromJSON Records