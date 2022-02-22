{-# LANGUAGE DeriveGeneric #-}

module Web.Data.Stooq.Internals where

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)

data StooqRow =
    StooqRow {
        symbol  :: !Text,
        date    :: Int,
        time    :: !Text,
        open    :: Double,
        high    :: Double,
        low     :: Double,
        close   :: Double,
        volume  :: Int,
        openint :: Int
    } deriving (Show, Generic)

data StooqResponse =
    StooqResponse {
        symbols :: [StooqRow]
    } deriving (Show, Generic)

instance FromJSON StooqRow
instance FromJSON StooqResponse

parseResponse :: ByteString -> Maybe StooqResponse
parseResponse = decode