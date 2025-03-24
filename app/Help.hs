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

  displayHelp :: a -> IO ()
  displayHelp cmd = do
    putStrLn $ "COMMAND: " ++ cmdTitle cmd
    putStrLn $ "ARGUMENTS: " ++ (show . cmdArgs) cmd
    putStrLn $ "OPTIONS: " ++ (show . cmdOptions) cmd
    putStrLn $ "DESC: " ++ cmdDesc cmd
