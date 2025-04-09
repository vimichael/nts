module Formatting where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, getTimeZone)
import Data.Time.LocalTime (utcToLocalTime)

toMDTitle :: String -> String
toMDTitle fileTitle = map repl fileTitle
  where
    repl '-' = ' '
    repl a = a

getFormattedDate :: IO String
getFormattedDate = do
  today <- getCurrentTime
  timezone <- getTimeZone today
  let localTime = utcToLocalTime timezone today
  return $ formatTime defaultTimeLocale "%Y-%m-%d" localTime

journalTitle :: String -> String
journalTitle dateStr = "journal-" ++ dateStr

formatTags :: [String] -> String
formatTags = (concatMap (\s -> "\n  - " ++ s))
