module Logger where

import qualified Color as Q
import Parser (matchFlag)

data Logger = Logger
  {verbose :: Bool}

logWrite :: Logger -> String -> IO ()
logWrite _ a' = putStrLn a'

logWriteV :: Logger -> String -> IO ()
logWriteV (Logger True) a' = putStrLn $ Q.blue a'
logWriteV _ _ = return ()

logErr :: Logger -> String -> IO ()
logErr _ a' = putStrLn $ Q.red a'

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
