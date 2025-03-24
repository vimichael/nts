{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Config where

import Control.Exception (IOException, try)
import Data.Aeson (decode)
import Data.Aeson.Types
import Data.String (fromString)
import GHC.Generics
import System.Environment (getEnv)
import System.FilePath (joinPath)

configPath :: String
configPath = ".config/nts/nts.json"

defaultJournalPath :: String
defaultJournalPath = "notes/dailies"

data Config
  = Config
  {journalPath :: String}
  deriving (Generic, Show)

type ErrorLog = String

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    Config
      <$> v .: "journal_path"

defaultCfg :: String -> Config
defaultCfg homeDir =
  Config
    { journalPath = joinPath [homeDir, defaultJournalPath]
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
