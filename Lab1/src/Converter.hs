module Converter where

import Data.Text as T (Text)
import Data.Time.Calendar as C (Day, fromGregorian)
import Database.MySQL.Base
import qualified Data.ByteString.Lazy.UTF8 as BSU
import Data.Binary.Put (runPut)
import Data.Int (Int32)
import Data.Time (parseTimeOrError, defaultTimeLocale)
import System.Console.Haskeline (defaultSettings, getInputLine, runInputT)

genStruct :: [[MySQLValue]] -> Int -> [MySQLValue]
genStruct xs ind = concatMap (\x -> [x !! ind]) xs

mergeLists :: [[a]] -> (b -> a) -> [b] -> b -> Int -> [[a]]
mergeLists ls func vals def = go ls vals
  where
    go [] [] = replicate n [func def]
    go (x:xs) [] = (x ++ [func def]) : go xs []
    go [] (v:vs) = [func v] : go [] vs
    go (x:xs) (v:vs) = (x ++ [func v]) : go xs vs
    n = length ls `max` length vals

myToString :: MySQLValue -> String
myToString val = BSU.toString (runPut (putTextField val))

myToInt32 :: MySQLValue -> Int32
myToInt32 val = maybe 0 id (readMaybe $ myToString val)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(val, "")] -> Just val
    _           -> Nothing

readStr :: String -> IO String
readStr outValue = fmap (maybe "" id) $ runInputT defaultSettings $ getInputLine $ outValue ++ "> "

readInt :: String -> IO (Maybe Int)
readInt outValue = fmap (>>= readMaybe) $ runInputT defaultSettings $ getInputLine $ outValue ++ "> "

strToInt32 :: String -> Int32
strToInt32 = maybe 0 id . readMaybe
