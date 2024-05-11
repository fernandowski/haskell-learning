{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log


parseMessage :: String -> LogMessage
parseMessage message = case (words message) of
            ("I" : timestamp : message)
                                        -> LogMessage (Info) (read timestamp) (unwords message)
            ("W" : timestamp : message)
                                        -> LogMessage (Warning) (read timestamp) (unwords message)
            ("E" : errorNumber : timestamp : message)
                                        -> LogMessage (Error $ read errorNumber) (read timestamp) (unwords message)
            unknownLine@(_:_)           -> Unknown $ unwords unknownLine
            []                          -> Unknown ""


parse :: String -> [LogMessage]
parse contents = map parseMessage $ lines contents

insert :: LogMessage -> MessageTree -> MessageTree
insert _ tree@(Node _ (Unknown _) _)
            = tree
insert (Unknown _) tree
            = tree
insert logMessage Leaf
            = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ newTime _) (Node leftTree originalMessage@(LogMessage _ nodeTime _) rightTree)
            = if (newTime < nodeTime)
                    then Node (insert logMessage leftTree) originalMessage rightTree
                    else Node leftTree originalMessage (insert logMessage rightTree)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (message : messages) = insert message (build messages)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf
		= []
inOrder (Node leftTree logMessage rightTree)
		= (inOrder leftTree) ++ [logMessage] ++ (inOrder rightTree)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = [ message | LogMessage _ _ message <- inOrder (build filteredMessages) ]
		where
			filteredMessages = [x | x@(LogMessage (Error severity) _ _) <- messages, severity >= 50]





