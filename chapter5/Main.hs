module Main
    where

import SimpleJSON

main :: IO ()
main = print (JObject [("fool", JNumber 1), ("bar", JBool False)])
