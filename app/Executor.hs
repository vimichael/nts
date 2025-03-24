{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Executor where

import Config
import Error
import File (createFilepath, toFilename)
import Formatting (formatTags, getFormattedDate, journalTitle, toMDTitle)
import Help
import Journal (JournalDesc (..))
import Note (NoteDesc (..))
import Parser
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (joinPath)
import Text.Printf (printf)

execCmd :: Config -> Command -> IO (Maybe Error)
execCmd cfg (Note n) = writeNote cfg n
execCmd cfg (Journal j) = writeJournal cfg j
execCmd _ (Help [cmd]) = do
  displayHelp cmd >> return Nothing
execCmd _ (Help cmds) = do
  mapM_ displayHelpWBreaks cmds
  return Nothing
  where
    displayHelpWBreaks cmd = do
      displayHelp cmd
      putStrLn ""

exeFromArgs :: Config -> [String] -> IO (Maybe Error)
exeFromArgs cfg args = do
  let parseResult = parseArgs args
  case parseResult of
    Left parseErr -> return $ Just parseErr
    Right cmd -> do
      cmdResult <- execCmd cfg cmd
      case cmdResult of
        Just cmdErr -> return $ Just cmdErr
        Nothing -> return Nothing

writeNote :: Config -> NoteDesc -> IO (Maybe Error)
writeNote cfg NoteDesc {..} = do
  contents <- readFile "template.txt"
  formattedDate <- getFormattedDate
  let formattedTags = case tags of
        Just t -> formatTags t
        Nothing -> ""
      filepath = createFilepath location title
      renderedContents =
        printf contents title formattedDate formattedTags (toMDTitle title)
  writeFile filepath renderedContents
  appendToJournal cfg title

appendToJournal :: Config -> String -> IO (Maybe Error)
appendToJournal cfg@Config {..} title = do
  writeJournalResult <- writeJournal cfg JournalDesc
  case writeJournalResult of
    Just err -> return $ Just err
    Nothing -> do
      formattedDate <- getFormattedDate
      let jTitle = journalTitle formattedDate
          path = joinPath [journalPath, toFilename jTitle]
      appendFile path $ "\n[[" ++ title ++ "]]"
      return Nothing

-- | writes a new journal with template contents it it doesn't exist
writeJournal :: Config -> JournalDesc -> IO (Maybe Error)
writeJournal Config {journalPath} _ = do
  dirExists <- doesDirectoryExist journalPath
  if dirExists
    then do
      formattedDate <- getFormattedDate
      let title = journalTitle formattedDate
          path = joinPath [journalPath, toFilename title]
      fileExists <- doesFileExist path
      case fileExists of
        True -> do
          putStrLn "journal already exists"
          return Nothing
        False -> do
          templateContents <- readFile "journal-template.txt"
          let render = printf templateContents formattedDate formattedDate formattedDate
          writeFile path render
          return Nothing
    else
      return $ Just $ JournalPathNotExists journalPath
