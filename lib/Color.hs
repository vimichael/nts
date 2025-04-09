{-# LANGUAGE OverloadedStrings #-}

module Color where

wrapAnsi :: String -> String -> String
wrapAnsi ansi s = ansi ++ s ++ "\ESC[0m"

reset :: String -> String
reset s = wrapAnsi "\ESC[0m" s

red :: String -> String
red s = wrapAnsi "\ESC[31m" s -- ANSI code 31 = Red

green :: String -> String
green s = wrapAnsi "\ESC[32m" s -- ANSI code 32 = Green

blue :: String -> String
blue s = wrapAnsi "\ESC[34m" s -- ANSI code 34 = Blue
