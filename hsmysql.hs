{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams

main :: IO () 
main = do
    conn <- connect
        defaultConnectInfo {ciUser = "root", ciPassword = "senhaewally", ciDatabase = "ewally_account-dvdvam"}
    (defs, is) <- query_ conn "SELECT * FROM transactions"
    print =<< Streams.toList is
