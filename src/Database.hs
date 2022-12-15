{-# LANGUAGE OverloadedStrings #-}

module Database (
    create_tables,
    drop_tables,
    save_data,
    get_prices_by_region,
    get_prices_by_area_code,
    get_highest_average_by_region,
    get_lowest_average_by_region,
    get_total_price_record_count,
    get_total_region_record_count,
    get_average_on_date_range
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
        \fk_region INTEGER\
        \)"

-- Drops all tables in the database -> returns nothing
drop_tables :: IO ()
drop_tables = do
    conn <- open "average-house-prices.sqlite"
    execute_ conn "DROP TABLE IF EXISTS regions"
    execute_ conn "DROP TABLE IF EXISTS prices "

get_or_create_region :: String -> String -> IO Region
get_or_create_region reg ar = do
    conn <- open "average-house-prices.sqlite"
    database_records <- queryNamed conn "SELECT * FROM regions WHERE region=:reg AND area=:area" [":reg" := reg, ":area" := ar]
    if length database_records > 0 then
        return . head $ database_records
    else do
        execute conn "INSERT INTO regions (region, area) VALUES (?, ?)" (reg, ar)
        get_or_create_region reg ar

-- starts process to generate records in both tables
create_data_record :: Record -> IO ()
create_data_record record = do
    conn <- open "average-house-prices.sqlite"
    region_selected <- get_or_create_region (region record) (area record)
    let price = Price {
        date_ = date record,
        average_ = average record,
        monthly_ = monthly record,
        annual_ = annual record,
        sa_ = sa record,
        fk_region = id_ region_selected
    }
    execute conn "INSERT INTO prices VALUES (?,?,?,?,?,?)" price
    close conn

save_data :: [Record] -> IO ()
save_data = mapM_ (create_data_record)

-- Get List of prices by re
get_prices_by_region :: IO [Price]
get_prices_by_region = do
    conn <- open "average-house-prices.sqlite"
    putStr "Region Name to query: "
    region_name <- getLine
    let sql_query = "SELECT p.* FROM prices as p INNER JOIN regions as r on p.fk_region == r.id WHERE r.region=?"
    query conn sql_query [region_name]
    
get_prices_by_area_code :: IO [Price]
get_prices_by_area_code = do
    conn <- open "average-house-prices.sqlite"
    putStr "Area Name to query: "
    area_name <- getLine
    let sql_query = "SELECT p.* FROM prices as p INNER JOIN regions as r on p.fk_region == r.id WHERE r.area=?"
    query conn sql_query [area_name]

-- All time high price per region
get_highest_average_by_region :: IO ()
get_highest_average_by_region = do
    conn <- open "average-house-prices.sqlite"
    putStr "Region Name to query: "
    region_name <- getLine
    let sql_query = "SELECT region, area, date, average, monthly, annual, sa FROM prices INNER JOIN regions on fk_region == id WHERE region=?"
    records_queried <- query conn sql_query [region_name]
    let highest_average = maximum (map average records_queried)
    print $ "Highest Average Price: " ++ show(highest_average)

-- All time low price per region
get_lowest_average_by_region :: IO ()
get_lowest_average_by_region = do
    conn <- open "average-house-prices.sqlite"
    putStr "Region Name to query: "
    region_name <- getLine
    let sql_query = "SELECT region, area, date, average, monthly, annual, sa FROM prices INNER JOIN regions on fk_region == id WHERE region=?"
    records_queried <- query conn sql_query [region_name]
    let lowest_average = minimum (map average records_queried)

    print $ "Lowest Average Price: " ++ show(lowest_average)

-- Total Price records
get_total_price_record_count :: IO ()
get_total_price_record_count = do
    conn <- open "average-house-prices.sqlite"
    let sql_query = "SELECT region, area, date, average, monthly, annual, sa FROM prices INNER JOIN regions on fk_region == id"
    records_queried <- query_ conn sql_query :: IO [Record]
    let row_count = length records_queried
    print $ "Total Price Rows: " ++ show(row_count)

-- Total Region records
get_total_region_record_count :: IO ()
get_total_region_record_count = do
    conn <- open "average-house-prices.sqlite"
    let sql_query = "SELECT id, region, area FROM regions"
    records_queried <- query_ conn sql_query :: IO [Region]
    let row_count = length records_queried
    print $ "Total Record Rows: " ++ show(row_count)


-- Average price by region by date (over date range)
get_average_on_date_range :: IO ()
get_average_on_date_range = do
    conn <- open "average-house-prices.sqlite"
    putStrLn "Region Name: "
    selected_region <- getLine
    putStr "Start Date: "
    start_date <- getLine
    putStr "End Date: "
    end_date <- getLine
    let sql_query = "SELECT region, area, date, average, monthly, annual, sa FROM prices INNER JOIN regions on fk_region == id WHERE date >= ? AND date <= ? and region = ?"
    records_queried <- query conn sql_query [start_date, end_date, selected_region] :: IO [Record]
    let house_price = map average records_queried
    
    print $ "Average price over range: " ++ show(calc_average house_price)

-- Referenced from https://wiki.haskell.org/Haskell_a_la_carte#Entr.C3.A9es
calc_average xs = sum xs / (fromIntegral (length xs))

-- TO IMPLEMENT
-- =================================================================
-- Average annual change (by region) (by date)
-- Lowest annual change (by region) (by date)


-- BACKLOG
-- =================================================================
-- Average price per region
-- list of average price per region (by date) (over date range)
-- Highest annual change (by region) (by date)
-- Total Locations
-- Total regions