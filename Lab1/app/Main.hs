module Main where

import Table
import Students
import SQLConnection
import ValidateTables


main :: IO ()
main = do
    conn <- connectDB
    validateAllTables conn
    closeDB conn