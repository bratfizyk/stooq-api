{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.Data.Stooq.Internals where

import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Csv (decode, HasHeader(NoHeader), FromField, parseField, runParser)
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

newtype StooqResponse =
    StooqResponse {
        symbols :: [StooqRow]
    } deriving (Show, Generic)

data TryField a = FieldValue a | NoValue

instance FromField (TryField Text) where
    parseField s = case runParser (parseField s) of
        Left err -> pure NoValue
        Right v  -> pure $ FieldValue v

instance FromField (TryField Double) where
    parseField s = case runParser (parseField s) of
        Left err -> pure NoValue
        Right v  -> pure $ FieldValue v

instance FromField (TryField Int) where
    parseField s = case runParser (parseField s) of
        Left err -> pure NoValue
        Right v  -> pure $ FieldValue v

parseResponse :: ByteString -> Either String StooqResponse
parseResponse input =
    fmap (StooqResponse . catMaybes . V.toList . V.map tupleToStooqRow) (decode NoHeader input)

    where
        tupleToStooqRow :: (Text, TryField Int, TryField Text, TryField Double, TryField Double, TryField Double, TryField Double, TryField Int, Text) -> Maybe StooqRow
        tupleToStooqRow (name, FieldValue date, FieldValue time, FieldValue open, FieldValue high, FieldValue low, FieldValue close, volume, _) =
            Just $ StooqRow name date time open high low close (defaultToZero volume)
        tupleToStooqRow (name, _, _, _, _, _, _, _, _) = Nothing

        defaultToZero :: TryField Int -> Int
        defaultToZero (FieldValue x) = x
        defaultToZero NoValue = 0