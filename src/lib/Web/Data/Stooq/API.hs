{-# LANGUAGE OverloadedStrings #-}

-- | Here's a simple wrapper around API offered by Stooq.pl.
-- It's capable of returning the latest price for the given instrument.
-- For more information about tickers available, visit the service.
-- Keep in mind that in some situations their ticker convention is different to what's known e.g. from Yahoo Finance
-- e.g.
--
-- xxxx.UK: London Stock Exchange (LSE)
--
-- xxxx.US: NYSE (OTC market not available, so a lot of ADRs like "OGZPY" or "SBRCY" can't be fetched)
--
-- xxxx.DE: Deutsche Börse
--
-- xxxx.JP: Tokyo Stock Exchange
--
-- xxxx: (no exchange code after full stop) Warsaw Stock Exchange (GPW)
--
-- Use:
--
-- >>> fetch "SPY.US"
-- Just [StooqPrice {symbol = StooqSymbol "SPY.US", time = ..., ...}]
module Web.Data.Stooq.API where

import Control.Lens ((^.))
import Data.Text (Text, unpack)
import Data.Time.Calendar (fromGregorian, Day)
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime (LocalTime(LocalTime), TimeOfDay(TimeOfDay), TimeZone, localTimeToUTC, hoursToTimeZone)
import Network.Wreq (get, responseBody)

import qualified Web.Data.Stooq.Internals as Impl

-- | A single-case DU that represents a ticker.
newtype StooqSymbol = StooqSymbol String
    deriving (Show, Read, Eq, Ord)

-- | A type representing market price data returned by Stooq.
data StooqPrice =
    StooqPrice {
        symbol  :: StooqSymbol,
        time    :: UTCTime,
        open    :: Double,
        high    :: Double,
        low     :: Double,
        close   :: Double,
        volume  :: Int
    } deriving Show

-- | Sends a request for the specified ticker and returns its latest price.
-- Returns "Nothing" if the response is invalid (this is most likely due to using a non-existent ticker).
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
            symbol  = (StooqSymbol . unpack . Impl.symbol) row,
            time    = localTimeToUTC stooqTimeZone $ LocalTime ((stooqIntToDay . Impl.date) row) ((stooqStringToTime . unpack . Impl.time) row),
            open    = Impl.open row,
            high    = Impl.high row,
            low     = Impl.low row,
            close   = Impl.close row,
            volume  = Impl.volume row
        }

        stooqIntToDay :: Int -> Day
        stooqIntToDay date = fromGregorian (toInteger $ (date `div` 10000) `mod` 10000) ((date `div` 100) `mod` 100) (date `mod` 100)

        stooqStringToTime :: String -> TimeOfDay
        stooqStringToTime [h1,h2,m1,m2,s1,s2] = TimeOfDay (read [h1,h2]) (read [m1,m2]) (read [s1,s2])
        stooqStringToTime x = error $ "Unexpected time format: " ++ x

        stooqTimeZone :: TimeZone
        stooqTimeZone = hoursToTimeZone 1

-- | Sends a request for multiple tickers at once.
-- The function makes only a single HTTP call.
fetchPrices :: [StooqSymbol] -> IO (Maybe [StooqPrice])
fetchPrices tickers = fetchPrice (concatTickers tickers)
    where
        concatTickers :: [StooqSymbol] -> StooqSymbol
        concatTickers = foldl1 (\(StooqSymbol t1) (StooqSymbol t2) -> StooqSymbol (t1 ++ " " ++ t2))

-- | A shorthand around "fetchPrice" that allows to call the function using a plain String, without converting it to a `StooqSymbol` first.
fetch :: String -> IO (Maybe [StooqPrice])
fetch = fetchPrice . StooqSymbol