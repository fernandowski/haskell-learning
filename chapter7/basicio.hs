main = do
       putStrLn "Wahts your name"
       inputString <- getLine
       putStrLn $ "Welcome " ++ inputString