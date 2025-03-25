module Help where

data HelpDesc = HelpDesc
  deriving (Show, Eq)

instance Helpable HelpDesc where
  cmdTitle _ = "help"
  cmdArgs _ = []
  cmdOptions _ = []
  cmdDesc _ = "creates a new journal at configured path."

data HelpOption
  = Flag String
  | FlagWArgs String String
  deriving (Show)

class Helpable a where
  cmdTitle :: a -> String
  cmdArgs :: a -> [String]
  cmdOptions :: a -> [HelpOption]
  cmdDesc :: a -> String

  displayHelp :: (String -> IO ()) -> a -> IO ()
  displayHelp logFn cmd = do
    logFn $ "COMMAND: " ++ cmdTitle cmd
    logFn $ "ARGUMENTS: " ++ (show . cmdArgs) cmd
    logFn $ "OPTIONS: " ++ (show . cmdOptions) cmd
    logFn $ "DESC: " ++ cmdDesc cmd
