{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Config where

import Control.Exception (IOException, try)
import Data.Aeson (decode)
import Data.Aeson.Types
import Data.String (fromString)
import GHC.Generics
import Parser (matchFlag)
import System.Environment (getEnv)
import System.FilePath (joinPath)

configPath :: String
configPath = ".config/nts/nts.json"

defaultJournalPath :: String
defaultJournalPath = "notes/dailies"

data Config
  = Config
  { journalPath :: String,
    verbose :: Bool
  }
  deriving (Generic, Show)

type ErrorLog = String

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    journalPath' <- v .: "journal_path"
    return
      Config
        { journalPath = journalPath',
          verbose = False
        }

defaultCfg :: String -> Config
defaultCfg homeDir =
  Config
    { journalPath = joinPath [homeDir, defaultJournalPath],
      verbose = False
    }

loadConfig :: IO (Config, Maybe ErrorLog)
loadConfig = do
  home <- getEnv "HOME"

  let configFile = joinPath [home, configPath]
  contentsResult <- try @IOException (readFile configFile)
  case contentsResult of
    Right contents -> do
      let mbCfg = decode (fromString contents) :: Maybe Config
      case mbCfg of
        Just cfg -> do
          let updatedJournalLoc = home ++ journalPath cfg
          return (cfg {journalPath = updatedJournalLoc}, Nothing)
        Nothing -> return (defaultCfg home, Just "failed to parse config")

    -- TODO :: log the exception and notify the user
    Left exc -> do
      return $ (defaultCfg home, Just $ show exc)

applyBoolFlags :: Config -> [String] -> Config
applyBoolFlags cfg flags = applyFlag cfg flags
  where
    applyFlag cfg' [] = cfg'
    applyFlag cfg' (x : xs)
      | matchFlag "v" x = cfg' {verbose = True}
      | matchFlag "verbose" x = cfg' {verbose = True}
      | otherwise = applyFlag cfg' xs
