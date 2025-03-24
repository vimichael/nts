module Logger where

data Logger = Logger
  {verbose :: Bool}

log :: (Show a) => Logger -> a -> IO ()
log (Logger True) a' = putStrLn $ show a'
log _ _ = return ()
