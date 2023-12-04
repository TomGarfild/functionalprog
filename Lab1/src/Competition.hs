module Competition where 

import Database.MySQL.Base
import qualified Data.Text as T (pack)
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP (text, vcat, Doc, (<>), render)

import SQLConnection
import Table
import Converter

data CompetitionInfo = CompetitionInfo {
    tableName :: String,
    fieldNames :: [String],
    ids :: [Int32],
    names :: [String],
    timeStart :: [String],
    timeEnd :: [String]
} deriving Show

emptyCompetitionInstance :: CompetitionInfo
emptyCompetitionInstance = CompetitionInfo "Competition" ["id", "name", "time_start", "time_end"] [] [] [] []

instance Table CompetitionInfo where
    getName :: CompetitionInfo -> String
    getName = tableName

    isEmpty :: CompetitionInfo -> Bool
    isEmpty tableInfo = null (ids tableInfo) || null (names tableInfo) || null (timeStart tableInfo) || null (timeEnd tableInfo)

    getFieldNames :: CompetitionInfo -> [String]
    getFieldNames tableInfo = 
        let [idField, nameField, startTimeField, endTimeField] = fieldNames tableInfo
        in concat [
            [idField | not (null (ids tableInfo))],
            [nameField | not (null (names tableInfo))],
            [startTimeField | not (null (timeStart tableInfo))],
            [endTimeField | not (null (timeEnd tableInfo))]
           ]

    getFieldValues :: CompetitionInfo -> [MySQLValue]
    getFieldValues (CompetitionInfo _ _ ids names timeStart timeEnd) =
        map MySQLInt32 ids ++
        map (MySQLText . T.pack) names ++
        map (MySQLText . T.pack) timeStart ++
        map (MySQLText . T.pack) timeEnd

    getMainFieldTables :: CompetitionInfo -> CompetitionInfo
    getMainFieldTables tableInfo = tableInfo {
            ids = [],
            names = names tableInfo,
            timeStart = timeStart tableInfo,
            timeEnd = timeEnd tableInfo
        }

    len :: CompetitionInfo -> Int
    len tableInfo = sum $ map (fromEnum . not . null) [ids tableInfo, names tableInfo, timeStart tableInfo, timeEnd tableInfo]

    fromMySQLValues :: IO [MySQLValue] -> IO CompetitionInfo
    fromMySQLValues res = do
        vals <- res
        return CompetitionInfo {
            tableName = tableName emptyCompetitionInstance,
            fieldNames = fieldNames emptyCompetitionInstance,
            ids = map myToInt32 (genStruct vals 0),
            names = map myToString (genStruct vals 1),
            timeStart = map myToString (genStruct vals 2),
            timeEnd = map myToString (genStruct vals 3)
        }
