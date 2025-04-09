{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Journal where

import Help

data JournalDesc = JournalDesc
  deriving (Show, Eq)

instance Helpable JournalDesc where
  getInfo _ =
    CommandInfo
      { opts = [],
        desc = "get help for commands",
        args = [],
        title = "help"
      }

defaultJournal :: JournalDesc
defaultJournal = JournalDesc
