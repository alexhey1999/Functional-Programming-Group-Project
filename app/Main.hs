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
    putStrLn "(2) Drop Databases"
    putStrLn "(3) Download data"
    putStrLn "(4)................."
    putStrLn "(5)................."
    putStrLn "(6) Quit "
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
            -- let url = "https://alexhey.co.uk/files/Average%20House%20Prices.json"
            let url = "https://alexhey.co.uk/files/Average%20House%20Prices%20Reduced.json"
            print "Running..."
            conn <- create_tables
            json <- get_data_from_url url
            case (parseRecords json) of
                Left err -> print err
                Right recs -> do
                    print "Saving DB..."
                    save_data (records recs)
                    print "Saved..."
        _ -> print "Exiting..."