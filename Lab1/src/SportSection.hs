module SportSection where 

import qualified Data.Text as T (Text, pack)
import Database.MySQL.Base
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP (text, vcat, Doc, (<>), render)

import SQLConnection
import Table
import Converter

data SportSectionInfo = SportSectionInfo {
    tableName :: String,
    fieldNames :: [String],
    ids :: [Int32],
    names :: [String]
} deriving Show

emptySportSectionInstance :: SportSectionInfo
emptySportSectionInstance = SportSectionInfo "Sport_section" ["id", "name"] [] []

instance Table SportSectionInfo where
    getName :: SportSectionInfo -> String
    getName = tableName
    
    isEmpty :: SportSectionInfo -> Bool
    isEmpty tableInfo = null (ids tableInfo) || null (names tableInfo)

    getFieldNames :: SportSectionInfo -> [String]
    getFieldNames tableInfo = let [idField, nameField] = fieldNames tableInfo in
                              [idField | not (null (ids tableInfo))] ++
                              [nameField | not (null (names tableInfo))]

    getFieldValues :: SportSectionInfo -> [MySQLValue]
    getFieldValues (SportSectionInfo _ _ ids names) =
        map MySQLInt32 ids ++
        map (MySQLText . T.pack) names

    getMainFieldTables :: SportSectionInfo -> SportSectionInfo
    getMainFieldTables tableInfo = tableInfo { ids = [], names = names tableInfo }

    len :: SportSectionInfo -> Int
    len tableInfo = length (filter not [null (ids tableInfo), null (names tableInfo)])

    fromMySQLValues :: IO [MySQLValue] -> IO SportSectionInfo
    fromMySQLValues res = do
        vals <- res
        return SportSectionInfo {
            tableName = tableName emptySportSectionInstance,
            fieldNames = fieldNames emptySportSectionInstance,
            ids = map myToInt32 (genStruct vals 0),
            names = map myToString (genStruct vals 1)
        }
