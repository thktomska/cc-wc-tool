module Main (main) where

import System.Environment
import qualified Data.Text.IO as TIO

import WordCount

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath : options) -> do
            text <- TIO.readFile filePath
            let result = wordCount text options
            TIO.putStrLn result
        _ -> putStrLn  "Invalid arguments"
