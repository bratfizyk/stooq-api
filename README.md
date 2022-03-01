# stooq-api

Here's a simple wrapper around API offered by Stooq.pl. It's capable of returning the latest price for the given instrument. For more information about tickers available, visit the service.

Keep in mind that in some situations their ticker convention is different to what's known e.g. from Yahoo Finance, e.g.
* `xxxx.UK`: London Stock Exchange (LSE)
* `xxxx.US`: NYSE (OTC market not available, so a lot of ADRs like "OGZPY" or "SBRCY" can't be fetched)
* `xxxx.DE`: Deutsche Börse
* `xxxx`: (no exchange code after full stop) Warsaw Stock Exchange (GPW)

Example:

```haskell
symbol :: String
symbol = "SPY.US"

main :: IO ()
main = do 
    resp <- fetch symbol 
    case resp of
        Nothing -> error $ "Could not fetch data for " ++ symbol
        Just price -> putStrLn $ show price
```