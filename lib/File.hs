{-# LANGUAGE OverloadedStrings #-}

module File where

import System.Environment (getEnv)
import System.FilePath (joinPath)

toFilename :: String -> String
toFilename = (++ ".md")

createFilepath :: String -> String -> String
createFilepath loc title = joinPath [loc, toFilename title]

expandDir :: String -> IO String
expandDir ('~' : '/' : rest) = do
  home <- getEnv "HOME"
  return $ joinPath [home, rest]
expandDir ('$' : rest) = do
  let (var, rest') = span (/= '/') rest
  env <- getEnv var
  return $ joinPath [env, rest']
expandDir path = return path
