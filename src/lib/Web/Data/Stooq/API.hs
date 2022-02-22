{-# LANGUAGE OverloadedStrings #-}

module Web.Data.Stooq.API where

import Control.Lens ((^.))
import Data.Text (Text, unpack)
import Data.Time.Calendar (fromGregorian, Day)
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime (LocalTime(LocalTime), TimeOfDay(TimeOfDay), TimeZone, localTimeToUTC, hoursToTimeZone)
import Network.Wreq (get, responseBody)

import qualified Web.Data.Stooq.Internals as Impl

newtype StooqSymbol = StooqSymbol String
    deriving (Show, Read, Eq, Ord)

data StooqPrice =
    StooqPrice {
        symbol  :: String,
        time    :: UTCTime,
        open    :: Double,
        high    :: Double,
        low     :: Double,
        close   :: Double,
        volume  :: Int,
        openint :: Int
    } deriving Show

fetchPrice :: StooqSymbol -> IO (Maybe [StooqPrice])
fetchPrice ticker = do
    r <- get (queryUrl ticker)
    return $ fmap (map toApiType . Impl.symbols) (Impl.parseResponse (r ^. responseBody))

    where
        baseUrl :: String
        baseUrl = "https://stooq.pl/q/l/?s="

        queryUrl :: StooqSymbol -> String
        queryUrl (StooqSymbol ticker) = baseUrl ++ ticker ++ "&e=json"

        toApiType :: Impl.StooqRow -> StooqPrice
        toApiType row = StooqPrice {
            symbol  = (unpack . Impl.symbol) row,
            time    = localTimeToUTC stooqTimeZone $ LocalTime ((stooqIntToDay . Impl.date) row) ((stooqStringToTime . unpack . Impl.time) row),
            open    = Impl.open row,
            high    = Impl.high row,
            low     = Impl.low row,
            close   = Impl.close row,
            volume  = Impl.volume row,
            openint = Impl.openint row
        }

        stooqIntToDay :: Int -> Day
        stooqIntToDay date = fromGregorian (toInteger $ (date `div` 10000) `mod` 10000) ((date `div` 100) `mod` 100) (date `mod` 100)

        stooqStringToTime :: String -> TimeOfDay
        stooqStringToTime [h1,h2,m1,m2,s1,s2] = TimeOfDay (read [h1,h2]) (read [m1,m2]) (read [s1,s2])
        stooqStringToTime x = error $ "Unexpected time format: " ++ x

        stooqTimeZone :: TimeZone
        stooqTimeZone = hoursToTimeZone 1

fetchPrices :: [StooqSymbol] -> IO (Maybe [StooqPrice])
fetchPrices tickers = fetchPrice (concatTickers tickers)
    where
        concatTickers :: [StooqSymbol] -> StooqSymbol
        concatTickers = foldl1 (\(StooqSymbol t1) (StooqSymbol t2) -> StooqSymbol (t1 ++ " " ++ t2))
    