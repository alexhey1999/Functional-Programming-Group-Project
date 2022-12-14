{-# LANGUAGE OverloadedStrings #-}

module Database (
    create_tables,
    drop_tables,
    save_data
) where

import Types
import Database.SQLite.Simple

-- Creates all required tables if they do not exist already -> Returns nothing
create_tables :: IO ()
create_tables = do
    conn <- open "average-house-prices.sqlite"
    execute_ conn "CREATE TABLE IF NOT EXISTS regions (\
        \id INTEGER PRIMARY KEY AUTOINCREMENT,\
        \region VARCHAR(255) NOT NULL,\
        \area VARCHAR(10) NOT NULL \
        \)"
    execute_ conn "CREATE TABLE IF NOT EXISTS prices (\
        \date DATE NOT NULL, \
        \average REAL NOT NULL, \
        \monthly REAL, \
        \annual REAL, \
        \sa REAL, \
        \fk_country INTEGER\
        \)"

-- Drops all tables in the database -> returns nothing
drop_tables :: IO ()
drop_tables = do
    conn <- open "average-house-prices.sqlite"
    execute_ conn "DROP TABLE IF EXISTS regions"
    execute_ conn "DROP TABLE IF EXISTS prices "

get_or_create_region :: String -> String -> IO Region
get_or_create_region reg area = do
    conn <- open "average-house-prices.sqlite"
    database_records <- queryNamed conn "SELECT * FROM regions WHERE region=:reg AND area=:area" [":reg" := reg, ":area" := area]
    if length database_records > 0 then
        return . head $ database_records
    else do
        execute conn "INSERT INTO regions (region, area) VALUES (?, ?)" (reg, area)
        get_or_create_region reg area

-- starts process to generate records in both tables
create_data_record :: Record -> IO ()
create_data_record record = do
    conn <- open "average-house-prices.sqlite"
    region <- get_or_create_region (region record) (area record)
    let price = Price {
        average_ = average record,
        monthly_ = monthly record,
        annual_ = annual record,
        sa_ = sa record,
        fk_region = id_ region
    }
    execute conn "INSERT INTO prices VALUES (?,?,?,?,?,?)" price

save_data :: [Record] -> IO ()
save_data = mapM_ (create_data_record)

-- TO IMPLEMENT
-- =================================================================
-- All time high price per region
-- All time low price per region
-- Average annual change (by region) (by date)
-- Lowest annual change (by region) (by date)


-- BACKLOG
-- =================================================================
-- Average price per region
-- list of average price per region (by date) (over date range)
-- Average price by region by date (over date range)
-- Highest annual change (by region) (by date)
-- Total records
-- Total Locations
-- Total regions