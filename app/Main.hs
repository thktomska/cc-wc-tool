module Main (main) where

import System.Environment
import qualified Data.Text.Lazy.IO as TLIO

import WordCount

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath : options) -> do
            text <- TLIO.readFile filePath
            let result = wordCount text options
            TLIO.putStrLn result
        _ -> putStrLn  "Invalid arguments"
