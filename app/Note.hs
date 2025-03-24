{-# LANGUAGE RecordWildCards #-}

module Note where

import Help

noteTemplate :: String
noteTemplate = "template.txt"

data NoteDesc = NoteDesc
  { location :: String,
    title :: String,
    tags :: Maybe [String]
  }
  deriving (Show, Eq)

instance Helpable NoteDesc where
  cmdTitle _ = "note"
  cmdArgs _ = ["PATH", "TITLE"]
  cmdOptions _ = [(FlagWArgs "--tags" "tags as args")]
  cmdDesc _ = "creates a new note and adds an entry to the daily journal."

defaultNote :: NoteDesc
defaultNote =
  NoteDesc
    { location = ".",
      title = "",
      tags = Nothing
    }
