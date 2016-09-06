{-
CIS-194 (Spring 2013)

Source: http://www.cis.upenn.edu/~cis194/spring13/hw/01-intro.pdf
-}

{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log


-- Exercise 1
-- Parse a String into a LogMessage
parseMessage :: String -> LogMessage
parseMessage = parseMessageHelper . words

parseMessageHelper :: [String] -> LogMessage
parseMessageHelper m = case m of
  ("I":x:xs)    -> LogMessage Info (stringToTS x) (unwords xs)
  ("W":x:xs)    -> LogMessage Warning (stringToTS x) (unwords xs)
  ("E":c:x:xs)  -> LogMessage (Error (read c::Int )) (stringToTS x) (unwords xs)
  _             -> Unknown (unwords m)

-- Converts a string into a TimeStamp
stringToTS :: String -> TimeStamp
stringToTS str = read str :: TimeStamp


parse :: String -> [LogMessage]
parse = map parseMessage . lines




-- Exercise 2
-- data MessageTree = Leaf
--   | Node MessageTree LogMessage MessageTree

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _)  tree                  = tree
insert lm           Leaf                  = Node Leaf lm Leaf
insert lm           (Node left msg right) =
        if tsForLog lm < tsForLog msg
          then Node (insert lm left) msg right
          else Node left msg (insert lm right)


tsForLog :: LogMessage -> TimeStamp
tsForLog (Unknown _)         = error "Should not happen"
tsForLog (LogMessage _ ts _) = ts



-- Exercise 3
-- Builds a message tree containing the messages in the list
build :: [LogMessage] -> MessageTree
{- Start with an empty tree, and 'accumulate' the nodes into the tree-}
build = foldl (\acc x -> insert x acc) Leaf


-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node Leaf msg Leaf)  = [msg]
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right


-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map stringFromError .  filter isSevereError . inOrder . build


stringFromError :: LogMessage -> String
stringFromError lm = case lm of
  (LogMessage _ _ msg)  -> msg
  (Unknown _)           -> ""

isSevereError :: LogMessage -> Bool
isSevereError lm = case lm of
  (LogMessage (Error num) _ _) -> num >= 0
  _                            -> False



-- Exercise 6
-- I've used this to sort all messages by timestamp
-- testParse (inOrder . build . parse) 10000 "error.log"







