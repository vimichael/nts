{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Journal where

import Help

data JournalDesc = JournalDesc
  deriving (Show, Eq)

instance Helpable JournalDesc where
  cmdTitle _ = "journal"
  cmdArgs _ = []
  cmdOptions _ = []
  cmdDesc _ = "creates a new journal at configured path."

defaultJournal :: JournalDesc
defaultJournal = JournalDesc
