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
  getInfo _ =
    CommandInfo
      { title = "note",
        args = ["PATH", "TITLE"],
        opts = [(FlagWArgs "--tags" "tags as args")],
        desc = "creates a new note and adds an entry to the daily journal."
      }

defaultNote :: NoteDesc
defaultNote =
  NoteDesc
    { location = ".",
      title = "",
      tags = Nothing
    }
