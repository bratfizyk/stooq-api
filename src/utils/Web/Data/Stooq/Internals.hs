{-# LANGUAGE DeriveGeneric #-}

module Web.Data.Stooq.Internals where

import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Csv (decode, HasHeader(NoHeader))
import qualified Data.Vector as V

data StooqRow =
    StooqRow {
        symbol  :: !Text,
        date    :: Int,
        time    :: !Text,
        open    :: Double,
        high    :: Double,
        low     :: Double,
        close   :: Double,
        volume  :: Int
    } deriving (Show, Generic)

data StooqResponse =
    StooqResponse {
        symbols :: [StooqRow]
    } deriving (Show, Generic)

parseResponse :: ByteString -> Maybe StooqResponse
parseResponse input = 
    case decode NoHeader input of
        Left err -> Nothing
        Right v -> Just . StooqResponse . V.toList $ V.map tupleToStooqRow v

    where
        tupleToStooqRow :: (Text, Int, Text, Double, Double, Double, Double, Maybe Int, Text) -> StooqRow
        tupleToStooqRow (name, date, time, open, high, low, close, volume, _) = StooqRow name date time open high low close (fromMaybe 0 volume)