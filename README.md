# stooq-api

Here's a simple wrapper around API offered by Stooq.pl. It's capable of returning the latest price for the given instrument. For more information about tickers available, visit the service.

Keep in mind that in some situations their ticker convention is different to what's known e.g. from Yahoo Finance, e.g.
* `xxxx.UK`: London Stock Exchange (LSE)
* `xxxx.US`: NYSE (OTC market not available, so a lot of ADRs like "OGZPY" or "SBRCY" can't be fetched)
* `xxxx.DE`: Deutsche Börse
* `xxxx`: (no exchange code after full stop) Warsaw Stock Exchange (GPW)

Example:

```haskell
import Web.Data.Stooq.API (fetch)

symbol :: String
symbol = "SPY.US"

main :: IO ()
main = do 
    resp <- fetch symbol 
    case resp of
        Nothing -> error $ "Could not fetch data for " ++ symbol
        Just price -> print price
```

Response:

If a query succeeds, it returns an array of the following record:

```haskell
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
```

The field `close` might be a bit misleading, as it contains the last price known to stooq. So if you want to get the latest price for an instrument that's currently being traded, `close` is what you want.
