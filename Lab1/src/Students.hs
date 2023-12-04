module Students where 

import Database.MySQL.Base
import qualified Data.Text as T (Text, pack)
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP (text, vcat, Doc, (<>), render)

import SQLConnection
import Table
import Converter

data StudentsInfo = StudentsInfo {
    tableName :: String,
    fieldNames :: [String],
    ids :: [Int32],
    names :: [String],
    surnames :: [String]
} deriving Show

emptyStudentsInstance :: StudentsInfo
emptyStudentsInstance = StudentsInfo "Students" ["id", "name", "surname"] [] [] []

instance Table StudentsInfo where
    getName :: StudentsInfo -> String
    getName = tableName
    
    isEmpty :: StudentsInfo -> Bool
    isEmpty tableInfo = null (ids tableInfo) || null (names tableInfo) || null (surnames tableInfo)

    getFieldNames :: StudentsInfo -> [String]
    getFieldNames tableInfo = let [idField, nameField, surnameField] = fieldNames tableInfo in
                              [idField | not (null (ids tableInfo))] ++
                              [nameField | not (null (names tableInfo))] ++
                              [surnameField | not (null (surnames tableInfo))]

    getFieldValues :: StudentsInfo -> [MySQLValue]
    getFieldValues (StudentsInfo _ _ ids names surnames) =
        map MySQLInt32 ids ++
        map (MySQLText . T.pack) names ++
        map (MySQLText . T.pack) surnames

    getMainFieldTables :: StudentsInfo -> StudentsInfo
    getMainFieldTables tableInfo = tableInfo { ids = [], names = names tableInfo, surnames = surnames tableInfo }

    len :: StudentsInfo -> Int
    len tableInfo = length (filter not [null (ids tableInfo), null (names tableInfo), null (surnames tableInfo)])

    fromMySQLValues :: IO [MySQLValue] -> IO StudentsInfo
    fromMySQLValues res = do
        vals <- res
        return StudentsInfo {
            tableName = tableName emptyStudentsInstance,
            fieldNames = fieldNames emptyStudentsInstance,
            ids = map myToInt32 (genStruct vals 0),
            names = map myToString (genStruct vals 1),
            surnames = map myToString (genStruct vals 2)
        }
