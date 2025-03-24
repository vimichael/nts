{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (loadConfig)
import Executor
import System.Environment (getArgs)

main :: IO ()
main = do
  (cfg, err) <- loadConfig
  case err of
    Just msg -> putStrLn $ "ERROR WHEN PARSING CONFIG: " ++ msg
    Nothing -> return ()

  args <- getArgs
  exeResult <- exeFromArgs cfg args
  case exeResult of
    Just exeErr -> putStrLn $ "ERROR WHEN PARSING COMMAND: " ++ show exeErr
    _ -> return ()
