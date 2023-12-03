#!/usr/bin/env stack

module Main where

import Gaussian 
import GHC.Conc (numCapabilities)
import Data.Time.Clock (diffUTCTime, getCurrentTime)


main :: IO ()
main = do
  let size = 420    
  putStrLn "Parallel"
  (mat1, vec1) <- generateRandom size
  start <- getCurrentTime
  let ans = gaussian mat1 vec1 True
  let x = case ans of
        Exists vec -> vec
        _ -> []
  let y = force x
  print x
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
  putStrLn $ "number of cores: " ++ show numCapabilities

  putStrLn "Sequential"
  start <- getCurrentTime
  let ans = gaussian mat1 vec1 False
  let x = case ans of
        Exists vec -> vec
        _ -> []
  let y = force x
  print x
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
  putStrLn "number of cores: 1"