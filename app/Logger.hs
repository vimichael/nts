module Logger where

import Parser (matchFlag)

data Logger = Logger
  {verbose :: Bool}

logWrite :: Logger -> String -> IO ()
logWrite _ a' = putStrLn a'

logErr :: Logger -> String -> IO ()
logErr (Logger True) a' = putStrLn a'
logErr _ _ = return ()

defaultLogger :: Logger
defaultLogger = Logger {verbose = False}

fromFlags :: [String] -> Logger
fromFlags flags = applyFlag flags defaultLogger
  where
    applyFlag [] l = l
    applyFlag (x : xs) l
      | matchFlag "v" x = l {verbose = True}
      | matchFlag "verbose" x = l {verbose = True}
      | otherwise = applyFlag xs l
