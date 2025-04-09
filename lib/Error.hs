module Error where

data Error
  = NotEnoughArgs
  | UnknownFlag String
  | Duplicateflag String
  | UnknownCommand String
  | MissingTitle
  | MissingTags
  | MissingValue String
  | JournalPathNotExists String
  | TooManyArguments String
  deriving (Eq)

instance Show Error where
  show NotEnoughArgs = "not enough arguments provided. please provide a <cmd> <loc> <title>"
  show (UnknownFlag f) = "unknown flag: " ++ f
  show (Duplicateflag f) = "duplicate flag: " ++ f
  show (UnknownCommand c) = "unknown command: " ++ c
  show (MissingValue c) = c ++ " is missing a command"
  show MissingTitle = "please provide a title after the location"
  show MissingTags = "please provide tags if using the --tags flag"
  show (JournalPathNotExists path) = "the journal path " ++ path ++ " does not exist"
  show (TooManyArguments lastCorrect) =
    "too many arguments provided. everything after " ++ lastCorrect ++ " should be removed"
