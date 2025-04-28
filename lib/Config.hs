{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Config where

import Control.Exception (IOException, try)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types
import Data.String (fromString)
import GHC.Generics
import Parser (matchFlag)
import Paths_nts (getDataFileName)
import System.Environment (getEnv)
import System.FilePath (joinPath)

configPath :: String
configPath = ".config/nts/nts.json"

defaultJournalPath :: String
defaultJournalPath = "notes/dailies"

defaultJournalTemplate :: String
defaultJournalTemplate = "notes/dailies"

data Config
  = Config
  { journalPath :: String,
    journalTemplate :: String,
    verbose :: Bool
  }
  deriving (Generic, Show)

data LogBuffer = Info String | LogError String

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    journalPath' <- v .:? "journal_path" .!= defaultJournalPath
    journalTemplate' <- v .:? "journal_template" .!= defaultJournalTemplate
    return
      Config
        { journalPath = journalPath',
          journalTemplate = journalTemplate',
          verbose = False
        }

defaultCfg :: String -> IO Config
defaultCfg homeDir = do
  tempPath <- getDataFileName "templates/journal-template.mustache"
  return $
    Config
      { journalPath = joinPath [homeDir, defaultJournalPath],
        journalTemplate = tempPath,
        verbose = False
      }

loadConfig :: IO (Config, Maybe LogBuffer)
loadConfig = do
  home <- getEnv "HOME"

  let configFile = joinPath [home, configPath]
  putStrLn configFile
  contentsResult <- try @IOException (readFile configFile)
  case contentsResult of
    Right contents -> do
      let mbCfg = eitherDecode (fromString contents) :: Either String Config
      case mbCfg of
        Right cfg -> do
          let updatedJournalLoc = joinPath [home, journalPath cfg]
          return (cfg {journalPath = updatedJournalLoc}, Just $ Info "using config at ~/.config/nts/nts.json")
        Left err -> do
          putStrLn err
          cfg <- defaultCfg home
          return (cfg, Just $ LogError "failed to parse config")

    -- TODO :: log the exception and notify the user
    Left exc -> do
      cfg <- defaultCfg home
      putStrLn $ show $ cfg
      return $ (cfg, Just $ Info $ show exc)

applyBoolFlags :: Config -> [String] -> Config
applyBoolFlags cfg flags = applyFlag cfg flags
  where
    applyFlag cfg' [] = cfg'
    applyFlag cfg' (x : xs)
      | matchFlag "v" x = cfg' {verbose = True}
      | matchFlag "verbose" x = cfg' {verbose = True}
      | otherwise = applyFlag cfg' xs
