module Fetch (
    get_data_from_url
) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

type URL = String

get_data_from_url :: URL -> IO L8.ByteString
get_data_from_url url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response

-- get_data_from_file :: file_name -> IO B.ByteString
-- get_data_from_file file_name = do
--     return $ B.readFile file_name