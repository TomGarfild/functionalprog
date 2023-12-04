module Teachers where 

import qualified Data.Text as T (pack)
import Database.MySQL.Base
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP (text, vcat, Doc, (<>), render)

import SQLConnection
import Table
import Converter

data TeachersInfo = TeachersInfo {
    tableName :: String,
    fieldNames :: [String],
    ids :: [Int32],
    names :: [String],
    surnames :: [String]
} deriving Show

emptyTeachersInstance :: TeachersInfo
emptyTeachersInstance = TeachersInfo "Teachers" ["id", "name", "surname"] [] [] []

instance Table TeachersInfo where
    getName :: TeachersInfo -> String
    getName = tableName
    
    isEmpty :: TeachersInfo -> Bool
    isEmpty tableInfo = null (ids tableInfo) || null (names tableInfo) || null (surnames tableInfo)

    getFieldNames :: TeachersInfo -> [String]
    getFieldNames tableInfo = 
        let [idField, nameField, surnameField] = fieldNames tableInfo
        in concat [
            [idField | not (null (ids tableInfo))],
            [nameField | not (null (names tableInfo))],
            [surnameField | not (null (surnames tableInfo))]
           ]

    getFieldValues :: TeachersInfo -> [MySQLValue]
    getFieldValues (TeachersInfo _ _ ids names surnames) =
        map MySQLInt32 ids ++
        map (MySQLText . T.pack) names ++
        map (MySQLText . T.pack) surnames

    getMainFieldTables :: TeachersInfo -> TeachersInfo
    getMainFieldTables tableInfo = tableInfo { ids = [], names = names tableInfo, surnames = surnames tableInfo }

    len :: TeachersInfo -> Int
    len tableInfo = sum $ map (fromEnum . not . null) [ids tableInfo, names tableInfo, surnames tableInfo]

    fromMySQLValues :: IO [MySQLValue] -> IO TeachersInfo
    fromMySQLValues res = do
        vals <- res
        return TeachersInfo {
            tableName = tableName emptyTeachersInstance,
            fieldNames = fieldNames emptyTeachersInstance,
            ids = map myToInt32 (genStruct vals 0),
            names = map myToString (genStruct vals 1),
            surnames = map myToString (genStruct vals 2)
        }
