module Help where

data Help = Help [CommandInfo]
  deriving (Show)

instance Helpable Help where
  getInfo _ =
    CommandInfo
      { title = "help",
        args = [],
        opts = [],
        desc = "get help for command(s)"
      }

data HelpOption
  = Flag String
  | FlagWArgs String String
  deriving (Show)

data CommandInfo = CommandInfo
  { title :: String,
    args :: [String],
    opts :: [HelpOption],
    desc :: String
  }
  deriving (Show)

class Helpable a where
  getInfo :: a -> CommandInfo

displayHelp :: (String -> IO ()) -> CommandInfo -> IO ()
displayHelp logFn info = do
  logFn $ "COMMAND: " ++ title info
  logFn $ "ARGUMENTS: " ++ (show . args) info
  logFn $ "OPTIONS: " ++ (show . opts) info
  logFn $ "DESC: " ++ desc info
