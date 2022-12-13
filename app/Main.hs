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
    putStrLn "(1) Download data"
    putStrLn "(2)................."
    putStrLn "(3)................."
    putStrLn "(4)................."
    putStrLn "(5) Quit "