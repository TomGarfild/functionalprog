module TrainingSchedule where 

import Database.MySQL.Base
import qualified Data.Text as T (pack)
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP (text, vcat, Doc, (<>), render)

import SQLConnection
import Table
import Converter

data TrainingScheduleInfo = TrainingScheduleInfo {
    tableName :: String,
    fieldNames :: [String],
    ids :: [Int32],
    sectionIds :: [Int32],
    teacherIds :: [Int32],
    weekday :: [String],
    timeStart :: [String],
    timeEnd :: [String]
} deriving Show

emptyTrainingScheduleInstance :: TrainingScheduleInfo
emptyTrainingScheduleInstance = TrainingScheduleInfo "Training_schedule" ["id", "section_id", "teacher_id", "weekday", "time_start", "time_end"] [] [] [] [] [] []

instance Table TrainingScheduleInfo where
    getName :: TrainingScheduleInfo -> String
    getName = tableName
    
    isEmpty :: TrainingScheduleInfo -> Bool
    isEmpty tableInfo = null (ids tableInfo) || null (sectionIds tableInfo) ||
                        null (teacherIds tableInfo) || null (weekday tableInfo) ||
                        null (timeStart tableInfo) || null (timeEnd tableInfo)

    getFieldNames :: TrainingScheduleInfo -> [String]
    getFieldNames tableInfo = 
        let [idField, sectionIdField, teacherIdField, weekdayField, startTimeField, endTimeField] = fieldNames tableInfo
        in concat [
            [idField | not (null (ids tableInfo))],
            [sectionIdField | not (null (sectionIds tableInfo))],
            [teacherIdField | not (null (teacherIds tableInfo))],
            [weekdayField | not (null (weekday tableInfo))],
            [startTimeField | not (null (timeStart tableInfo))],
            [endTimeField | not (null (timeEnd tableInfo))]
           ]

    getFieldValues :: TrainingScheduleInfo -> [MySQLValue]
    getFieldValues (TrainingScheduleInfo _ _ ids sectionIds teacherIds weekday timeStart timeEnd) =
        map MySQLInt32 ids ++
        map MySQLInt32 sectionIds ++
        map MySQLInt32 teacherIds ++
        map (MySQLText . T.pack) weekday ++
        map (MySQLText . T.pack) timeStart ++
        map (MySQLText . T.pack) timeEnd

    getMainFieldTables :: TrainingScheduleInfo -> TrainingScheduleInfo
    getMainFieldTables tableInfo = tableInfo { ids = [], sectionIds = sectionIds tableInfo, teacherIds = teacherIds tableInfo, weekday = weekday tableInfo, timeStart = timeStart tableInfo, timeEnd = timeEnd tableInfo }

    len :: TrainingScheduleInfo -> Int
    len tableInfo = sum $ map (fromEnum . not . null) [ids tableInfo, sectionIds tableInfo, teacherIds tableInfo, weekday tableInfo, timeStart tableInfo, timeEnd tableInfo]

    fromMySQLValues :: IO [MySQLValue] -> IO TrainingScheduleInfo
    fromMySQLValues res = do
        vals <- res
        return TrainingScheduleInfo {
            tableName = tableName emptyTrainingScheduleInstance,
            fieldNames = fieldNames emptyTrainingScheduleInstance,
            ids = map myToInt32 (genStruct vals 0),
            sectionIds = map myToInt32 (genStruct vals 1),
            teacherIds = map myToInt32 (genStruct vals 2),
            weekday = map myToString (genStruct vals 3),
            timeStart = map myToString (genStruct vals 4),
            timeEnd = map myToString (genStruct vals 5)
        }
