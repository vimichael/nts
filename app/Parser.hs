module Parser where

import Data.List (partition)
import Error
import qualified Help as H
import Journal (JournalDesc (JournalDesc), defaultJournal)
import Note

-- | all available commands
data Command
  = Note NoteDesc
  | Journal JournalDesc
  | Help H.Help
  deriving (Show)

allCommands :: [H.CommandInfo]
allCommands =
  [H.getInfo defaultNote, H.getInfo defaultJournal, H.getInfo $ H.Help []]

-- | returns true if the command starts with one or more `-`
isFlag :: String -> Bool
isFlag ('-' : _) = True
isFlag _ = False

isBoolFlag :: String -> Bool
isBoolFlag s = stripFlag s `elem` ["v"]
  where
    stripFlag ('-' : '-' : rest) = rest
    stripFlag ('-' : rest) = rest
    stripFlag rest = rest

-- | checks if commmand = flag or -flag or --flag
-- where flag is the target name of the flag
matchArgOrFlag :: String -> String -> Bool
matchArgOrFlag flag arg =
  matchFlag flag arg || flag == arg

-- | checks if name matches flag, omits commands that don't have
-- `-` prepended
matchFlag :: String -> String -> Bool
matchFlag flag ('-' : '-' : xs) = flag == xs
matchFlag flag ('-' : xs) = flag == xs
matchFlag _ _ = False

-- | basically partition
takeWhileAndRemaining :: (a -> Bool) -> [a] -> ([a], [a])
takeWhileAndRemaining cond li =
  aux cond li []
  where
    aux _ [] acc = (reverse acc, [])
    aux c (x : xs) acc =
      if c x
        then
          aux c xs (x : acc)
        else
          (reverse acc, x : xs)

-- | extracts and formats the title from a list of arguments succeeding
-- the location of a note
extractTitle :: [String] -> Either Error (String, [String])
extractTitle [] = Left MissingTitle
extractTitle args =
  case takeWhileAndRemaining (not . isFlag) args of
    ([], _) -> Left MissingTitle
    (titleList, remaining) ->
      Right (init $ concatMap (\s -> s ++ "-") titleList, remaining)

-- | extracts and collects the tags from all literals succeeding
-- the `--tags` flag until another flag is reached
extractTags :: [String] -> Either Error (Maybe [String], [String])
extractTags [] = Right (Nothing, [])
extractTags args =
  case takeWhileAndRemaining (not . isFlag) args of
    ([], _) -> Left MissingTags
    (tags', remaining) -> Right (Just tags', remaining)

-- | takes all arguments and extracts boolean flags from them,
-- returning (filtered rest)
extractBoolFlags :: [String] -> ([String], [String])
extractBoolFlags args = partition (liftA2 (&&) isFlag isBoolFlag) args

-- | converts sanitized args into a commmand, where
-- sanitized means they have been stripped of boolean flags
-- propagates errors from the parsing process
parseArgs :: [String] -> Either Error Command
parseArgs [] = Left NotEnoughArgs
-- create a note
parseArgs ["note"] = Left NotEnoughArgs
parseArgs ("note" : arg0 : args)
  | matchArgOrFlag "help" arg0 = Right $ Help $ H.Help [H.getInfo defaultNote]
  | otherwise =
      case extractTitle args of
        Left err -> Left err
        Right (title', []) ->
          Right $ Note $ NoteDesc {location = arg0, title = title', tags = Nothing}
        Right (title', (_ : xs)) ->
          case extractTags xs of
            Left err -> Left err
            Right (mbTags, []) ->
              Right $ Note $ NoteDesc {location = arg0, title = title', tags = mbTags}
            Right (_, (x : _)) ->
              Left $ case matchFlag "tags" x of
                True -> Duplicateflag x
                False -> UnknownFlag x
-- create a journal
parseArgs ["journal"] = Right $ Journal JournalDesc
parseArgs ("journal" : arg0 : _)
  | matchArgOrFlag "help" arg0 = Right $ Help $ H.Help [H.getInfo defaultJournal]
  | otherwise = Left $ TooManyArguments arg0
-- handle help
parseArgs (cmd : _) =
  case matchArgOrFlag "help" cmd of
    True -> Right $ Help $ H.Help allCommands
    False -> Left $ UnknownCommand cmd
