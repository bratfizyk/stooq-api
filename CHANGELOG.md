# Revision history for stooq

## 0.4.2.0 -- 2023-02-04

* Updating nixos channel to 22.11.

## 0.4.0.0 -- 2022-11-27

* Switching to XML format, which seems more reliable than JSON.

## 0.3.1.0 -- 2022-07-07

* Migrating to nixpkgs 22.05.

## 0.3.0.0 -- 2022-07-06

* Removing the field `openint` from the response, as sometimes it's missing in server's response, causing the entire call to fail.

## 0.2.0.0 -- 2022-04-08

* It makes more sense for the `StooqPrice` record to contain a field `symbol` of type `StooqSymbol` instead of `String`.

## 0.1.0.0 -- 2022-03-30

* Initial release.
