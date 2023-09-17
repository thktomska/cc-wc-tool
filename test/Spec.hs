import WordCount
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    text <- TIO.readFile "test.txt"

    putStrLn $ assert
        "Check no options: "
        (wordCount text [""] == "words: 58164")
    putStrLn $ assert
        "Check -c option: "
        (wordCount text ["c"] == "bytes: 342143")
    putStrLn $ assert
        "Check -l option: "
        (wordCount text ["l"] == "lines: 7143")
    putStrLn $ assert
        "Check -m option: "
        (wordCount text ["m"] == "chars: 339245")
    putStrLn $ assert
        "Check -w option: "
        (wordCount text ["w"] == "words: 58164")
    putStrLn $ assert
        "Check -cl option: "
        (wordCount text ["cl"] == "bytes: 342143; lines: 7143")
    putStrLn $ assert
        "Check -clm option: "
        (wordCount text ["clm"] == "bytes: 342143; lines: 7143; chars: 339245")
    putStrLn $ assert
        "Check -clmw option: "
        (wordCount text ["clmw"] == "bytes: 342143; lines: 7143; chars: 339245; words: 58164")
  where
    assert :: String -> Bool -> String
    assert testLabel assertion = testLabel ++ (show assertion)
