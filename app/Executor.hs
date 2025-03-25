{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Executor where

import Config
import Error
import File (createFilepath, toFilename)
import Formatting (formatTags, getFormattedDate, journalTitle, toMDTitle)
import Help
import Journal (JournalDesc (..))
import Logger (Logger, logWrite)
import Note (NoteDesc (..))
import Parser
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (joinPath)
import Text.Printf (printf)

-- | executes given commamnd and propagates errors
execCmd :: Config -> Logger -> Command -> IO (Maybe Error)
execCmd cfg logger (Note n) = writeNote cfg logger n
execCmd cfg logger (Journal j) = writeJournal cfg logger j
execCmd _ logger (Help [cmd]) = do
  (displayHelp (logWrite logger) cmd) >> return Nothing
execCmd _ logger (Help cmds) = do
  mapM_ (displayHelpWBreaks logger) cmds
  return Nothing
  where
    displayHelpWBreaks l cmd = do
      displayHelp (logWrite l) cmd
      logWrite logger ""

-- | runs a command from sanitized arguments
-- | bool flags should be removed and processed beforehand
exeFromArgs :: Config -> Logger -> [String] -> IO (Maybe Error)
exeFromArgs cfg logger args = do
  let parseResult = parseArgs args
  case parseResult of
    Left parseErr -> return $ Just parseErr
    Right cmd -> do
      cmdResult <- execCmd cfg logger cmd
      case cmdResult of
        Just cmdErr -> return $ Just cmdErr
        Nothing -> return Nothing

writeNote :: Config -> Logger -> NoteDesc -> IO (Maybe Error)
writeNote cfg logger NoteDesc {..} = do
  contents <- readFile "template.txt"
  formattedDate <- getFormattedDate
  let formattedTags = case tags of
        Just t -> formatTags t
        Nothing -> ""
      filepath = createFilepath location title
      renderedContents =
        printf contents title formattedDate formattedTags (toMDTitle title)
  writeFile filepath renderedContents
  appendToJournal cfg logger title

appendToJournal :: Config -> Logger -> String -> IO (Maybe Error)
appendToJournal cfg@Config {..} logger title = do
  writeJournalResult <- writeJournal cfg logger JournalDesc
  case writeJournalResult of
    Just err -> return $ Just err
    Nothing -> do
      formattedDate <- getFormattedDate
      let jTitle = journalTitle formattedDate
          path = joinPath [journalPath, toFilename jTitle]
      appendFile path $ "\n[[" ++ title ++ "]]"
      return Nothing

-- | writes a new journal with template contents it it doesn't exist
writeJournal :: Config -> Logger -> JournalDesc -> IO (Maybe Error)
writeJournal Config {journalPath} logger _ = do
  dirExists <- doesDirectoryExist journalPath
  if dirExists
    then do
      formattedDate <- getFormattedDate
      let title = journalTitle formattedDate
          path = joinPath [journalPath, toFilename title]
      fileExists <- doesFileExist path
      case fileExists of
        True -> do
          logWrite logger "journal already exists"
          return Nothing
        False -> do
          templateContents <- readFile "journal-template.txt"
          let render = printf templateContents formattedDate formattedDate formattedDate
          writeFile path render
          return Nothing
    else
      return $ Just $ JournalPathNotExists journalPath
