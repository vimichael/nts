module Parser where

import Error
import Help
import Journal (JournalDesc (JournalDesc), defaultJournal)
import Note

data Command
  = Note NoteDesc
  | Journal JournalDesc
  | Help [Command]
  deriving (Show, Eq)

instance Helpable Command where
  cmdTitle (Help _) = "help"
  cmdTitle (Note n) = cmdTitle n
  cmdTitle (Journal j) = cmdTitle j

  cmdArgs (Help _) = []
  cmdArgs (Note n) = cmdArgs n
  cmdArgs (Journal j) = cmdArgs j

  cmdOptions (Help _) = []
  cmdOptions (Note n) = cmdOptions n
  cmdOptions (Journal j) = cmdOptions j

  cmdDesc (Help _) = "get help!"
  cmdDesc (Note n) = cmdDesc n
  cmdDesc (Journal j) = cmdDesc j

allCommands :: [Command]
allCommands = [Note defaultNote, Journal defaultJournal, Help []]

isFlag :: String -> Bool
isFlag ('-' : _) = True
isFlag _ = False

matchArgOrFlag :: String -> String -> Bool
matchArgOrFlag flag arg =
  matchFlag flag arg || flag == arg

matchFlag :: String -> String -> Bool
matchFlag flag ('-' : '-' : xs) = flag == xs
matchFlag flag ('-' : xs) = flag == xs
matchFlag _ _ = False

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

extractTitle :: [String] -> Either Error (String, [String])
extractTitle [] = Left MissingTitle
extractTitle args =
  case takeWhileAndRemaining (not . isFlag) args of
    ([], _) -> Left MissingTitle
    (titleList, remaining) ->
      Right (init $ concatMap (\s -> s ++ "-") titleList, remaining)

extractTags :: [String] -> Either Error (Maybe [String], [String])
extractTags [] = Right (Nothing, [])
extractTags args =
  case takeWhileAndRemaining (not . isFlag) args of
    ([], _) -> Left MissingTags
    (tags', remaining) -> Right (Just tags', remaining)

parseArgs :: [String] -> Either Error Command
parseArgs [] = Left NotEnoughArgs
parseArgs ["note"] = Left NotEnoughArgs
-- create a note
parseArgs ("note" : arg0 : args)
  | matchArgOrFlag "help" arg0 = Right $ Help [Note defaultNote]
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
parseArgs ["journal"] = Right $ Journal JournalDesc
parseArgs ("journal" : arg0 : _)
  | matchArgOrFlag "help" arg0 = Right $ Help [Journal defaultJournal]
  | otherwise = Left $ TooManyArguments arg0
parseArgs (cmd : _) =
  case matchArgOrFlag "help" cmd of
    True -> Right $ Help allCommands
    False -> Left $ UnknownCommand cmd
