
{-# LANGUAGE DeriveGeneric #-}

module Parse (
    parseRecords,
    make_json_file,
) where

import Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

parseRecords :: L8.ByteString -> Either String Records
parseRecords j = eitherDecode j :: Either String Records

make_json_file :: L8.ByteString -> IO ()
make_json_file j = do
    print "Starting File Writing..."
    writeFile "exported_from_url.json" (L8.unpack j)
    print "Written to file... test.json"