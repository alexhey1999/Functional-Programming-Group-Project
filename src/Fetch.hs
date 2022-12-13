module Fetch (
    get_data_from_url , get_data_from_file
) where

import qualified Data.ByteString.Lazy as B
import Network.HTTP.Simple
import System.IO

get_data_from_url :: URL -> IO B.ByteString
get_data_from_url url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response

get_data_from_file :: file_name -> IO B.ByteString
get_data_from_file file_name = do
    return $ B.readFile jsonFile


main :: IO()
main = do
    print $ get_data_from_url "https://alexhey.co.uk/files/Average%20House%20Prices.json"