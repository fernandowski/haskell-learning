import System.IO
import Data.Char(toUpper)

main :: IO()
main = do
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh

mainloop :: Handle -> Handle -> IO([Char])
mainloop inh outh =
    do ineof <- hIsEOF inh
       if ineof then return("hello the end")
       else do inpStr <- hGetLine inh
               hPutStrLn outh (map toUpper inpStr)
               mainloop inh outh