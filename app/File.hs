module File where

import System.FilePath (joinPath)

toFilename :: String -> String
toFilename = (++ ".md")

createFilepath :: String -> String -> String
createFilepath loc title = joinPath [loc, toFilename title]
