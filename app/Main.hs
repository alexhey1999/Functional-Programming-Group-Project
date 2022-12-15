module Main (main) where

import System.IO
import Types
import Fetch
import Parse
import Database

main :: IO ()
main = do
    putStrLn "Welcome to my Housing Prices App!"
    putStrLn "The app that tells you how much the average prices of houses has changed since 1968"
    putStrLn "Pick from the following options?"
    putStrLn "(1) Create tables"
    putStrLn "(2) Drop tables"
    putStrLn "(3) Download data"
    putStrLn "(4) Get Price Data by Region"
    putStrLn "(5) Get Price Data by Area Code"
    putStrLn "(6) Peak Average Price by Region"
    putStrLn "(7) Lowest Average Price by Region"
    putStrLn "(8) Total Price Row Count"
    putStrLn "(9) Total Region Row Count"
    putStrLn "(10) Average House Price in date range"
    putStrLn "(11) Write to json file "
    putStrLn "(12) Quit "
    hSetBuffering stdout NoBuffering
    option <- readLn :: IO Int
    case option of
        1 -> do
            print "Creating tables..."
            _ <- create_tables
            print "Done!"

        2 -> do
            print "Dropping tables will delete all data inside. Are you sure you want to continue (Y/N)"
            drop_table_confirm <- getLine
            case drop_table_confirm of
                "Y" -> do
                    _ <- drop_tables
                    print "All tables dropped"

                "y" -> do
                    _ <- drop_tables
                    print "All tables dropped"

                _ -> print "Exiting..."

        3 -> do
            let url = "https://alexhey.co.uk/files/Average%20House%20Prices.json"
            -- let url = "https://alexhey.co.uk/files/Average%20House%20Prices%20Reduced.json"
            print "Running..."
            json <- get_data_from_url url
            case (parseRecords json) of
                Left err -> print err
                Right recs -> do
                    print "Saving DB..."
                    save_data (records recs)
                    print "Saved..."

        4 -> do 
            prices_ <- get_prices_by_region
            mapM_ print prices_
            print "done"

        5 -> do 
            prices_ <- get_prices_by_area_code
            mapM_ print prices_
            print "done"
        
        6 -> do
            get_highest_average_by_region
            print "done"
        
        7 -> do
            get_lowest_average_by_region
            print "done"

        8 -> do
            get_total_price_record_count
            print "done"

        9 -> do
            get_total_region_record_count
            print "done"
        
        10 -> do
            get_average_on_date_range
            print "done"

        11 -> do
            let url = "https://alexhey.co.uk/files/Average%20House%20Prices.json"
            -- let url = "https://alexhey.co.uk/files/Average%20House%20Prices%20Reduced.json"
            json <- get_data_from_url url
            make_json_file json

        _ -> print "Exiting..."