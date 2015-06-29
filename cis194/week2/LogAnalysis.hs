{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

parseMessage :: String -> LogMessage
parseMessage line = let (t,rest) = parseType line
                        (stamp,msg) = break (== ' ') . tail $ rest
                        numStamp = read stamp :: TimeStamp
                    in
                      LogMessage t numStamp (tail msg)

parseType :: String -> (MessageType, String)
parseType str = let (t,rest) = break (== ' ') str
                    (errLevel,errRest) = break (== ' ') . tail $ rest
                    numErrLevel = read errLevel :: Int
                in
                  case t of
                    "I" -> (Info,rest)
                    "W" -> (Warning,rest)
                    "E" -> (Error numErrLevel,errRest)
                    _   -> error "Wrong input"
