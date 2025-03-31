{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (LogBuffer (Info, LogError), loadConfig)
import Executor
import Logger (fromFlags, logErr, logWriteV)
import Parser (extractBoolFlags)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  -- extract flags that are command agnostic
  -- e.g. verbose/v
  let (boolFlags, rest) = extractBoolFlags args
      logger = fromFlags boolFlags

  -- deserialize config
  (cfg, err) <- loadConfig
  case err of
    Just (LogError msg) -> logErr logger $ "ERROR WHEN PARSING CONFIG: " ++ msg
    Just (Info msg) -> logWriteV logger $ "INFO: " ++ msg
    Nothing -> return ()

  -- run command
  exeResult <- exeFromArgs cfg logger rest
  case exeResult of
    Just exeErr -> logErr logger $ "ERROR WHEN PARSING COMMAND: " ++ show exeErr
    _ -> return ()
