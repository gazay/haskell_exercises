{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseLog :: String -> LogMessage
parseLog line = let (t,rest) = parseType line
                    (stamp,msg) = break (== ' ') . tail $ rest
                    numStamp = read stamp :: TimeStamp
                in
                  LogMessage t numStamp (tail msg)

parseType :: String -> (MessageType, String)
parseType str = let (t,rest) = break (== ' ') str
                    (errLevel,errRest) = break (== ' ') . tail $ rest
                in
                  case t of
                    "I" -> (Info,rest)
                    "W" -> (Warning,rest)
                    "E" -> (Error errLevel,rest)
                    _   -> error "Wrong input"
