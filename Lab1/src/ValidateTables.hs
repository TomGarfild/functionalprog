module ValidateTables where

import Database.MySQL.Base

import Table
import Students
import Teachers
import SportSection
import TrainingSchedule
import Competition
import SQLConnection

performAndPrint :: (a -> MySQLConn -> IO b) -> a -> MySQLConn -> IO ()
performAndPrint action arg connection = do
    res <- action arg connection
    print res


validateStudents :: MySQLConn -> IO ()
validateStudents connection = do
    let studentInfo = StudentsInfo "Students" ["id", "name", "surname"] [] ["First"] ["Student"]
    performAndPrint addValue studentInfo connection
    performAndPrint readAllValues emptyStudentsInstance connection
    performAndPrint readValue studentInfo connection
    let updatedInfo = StudentsInfo "Students" ["id", "name", "surname"] [] ["Oleksii"] ["Safroniuk"]
    performAndPrint updateValue updatedInfo studentInfo connection
    performAndPrint readAllValues emptyStudentsInstance connection
    performAndPrint deleteValue updatedInfo connection
    performAndPrint readAllValues emptyStudentsInstance connection


validateTeachers :: MySQLConn -> IO ()
validateTeachers connection = do
    let teachersInfo = TeachersInfo "Teachers" ["id", "name", "surname"] [] ["First"] ["Teacher"]
    performAndPrint addValue teachersInfo connection
    performAndPrint readAllValues emptyTeachersInstance connection
    performAndPrint readValue teachersInfo connection
    let updatedInfo = TeachersInfo "Teachers" ["id", "name", "surname"] [] ["Second"] ["Teacher"]
    performAndPrint updateValue updatedInfo teachersInfo connection
    performAndPrint readAllValues emptyTeachersInstance connection
    performAndPrint deleteValue updatedInfo connection
    performAndPrint readAllValues emptyTeachersInstance connection


validateSportSection :: MySQLConn -> IO ()
validateSportSection connection = do
    let sportSectionInfo = SportSectionInfo "Sport_section" ["id", "name"] [] ["SportSection"]
    performAndPrint addValue sportSectionInfo connection
    performAndPrint readAllValues emptySportSectionInstance connection
    performAndPrint readValue sportSectionInfo connection
    let updatedInfo = SportSectionInfo "Sport_section" ["id", "name"] [] ["SportSection2"]
    performAndPrint updateValue updatedInfo sportSectionInfo connection
    performAndPrint readAllValues emptySportSectionInstance connection
    performAndPrint deleteValue updatedInfo connection
    performAndPrint readAllValues emptySportSectionInstance connection


validateTrainingSchedule :: MySQLConn -> IO ()
validateTrainingSchedule connection = do
    performAndPrint readAllValues emptyTrainingScheduleInstance connection

    let tradingSchedule = TrainingScheduleInfo "Training_schedule" 
                    ["id", "section_id", "teacher_id", "weekday", "time_start", "time_end"] 
                    [] [1] [1] ["Friday"] ["11:30:00"] ["14:00:00"]
    performAndPrint addValue tradingSchedule connection

    performAndPrint readAllValues emptyTrainingScheduleInstance connection
    performAndPrint readValue tradingSchedule connection

    let updatedData = TrainingScheduleInfo "Training_schedule" 
                        ["id", "section_id", "teacher_id", "weekday", "time_start", "time_end"] 
                        [] [1] [1] ["Tuesday"] ["13:00:00"] ["15:00:00"]
    performAndPrint updateValue updatedData tradingSchedule connection

    performAndPrint readAllValues emptyTrainingScheduleInstance connection
    performAndPrint deleteValue updatedData connection
    performAndPrint readAllValues emptyTrainingScheduleInstance connection

    
validateCompetition :: MySQLConn -> IO ()
validateCompetition connection = do
    performAndPrint readAllValues emptyCompetitionInstance connection

    let competition = CompetitionInfo "Competition" 
                    ["id", "name", "time_start", "time_end"] [] 
                    ["Competition"] ["2023-12-01 11:00:00"] ["2023-12-01 16:00:00"]
    performAndPrint addValue competition connection

    performAndPrint readAllValues emptyCompetitionInstance connection
    performAndPrint readValue competition connection

    let updatedData = CompetitionInfo "Competition" 
                        ["id", "name", "time_start", "time_end"] [] 
                        ["New Competition"] ["2023-12-13 15:00:00"] ["2023-12-13 19:00:00"]
    performAndPrint updateValue updatedData competition connection

    performAndPrint readAllValues emptyCompetitionInstance connection
    performAndPrint deleteValue updatedData connection
    performAndPrint readAllValues emptyCompetitionInstance connection



validateAllTables :: MySQLConn -> IO ()
validateAllTables connection = do 
    validateStudents connection
    validateTeachers connection
    validateSportSection connection
    validateTrainingSchedule connection
    validateCompetition connection
    