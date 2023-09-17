module WordCount
    ( wordCount
    ) where

import qualified Data.Text as T
import Data.Text.Encoding(encodeUtf8)
import qualified Data.ByteString as B

type RawOption = String
data WCOption = CountBytes | CountLines | CountChars | CountWords
    deriving (Eq, Show)

wordCount :: T.Text -> [RawOption] -> T.Text
wordCount text rawOptions =
    T.intercalate "; " $
    map (wordCount' text) (prepareOptions rawOptions)

wordCount' :: T.Text -> WCOption -> T.Text
wordCount' text wcOption =
    case wcOption of
        CountWords -> wrap "words:" countWords
        CountLines -> wrap "lines:" countLines
        CountChars -> wrap "chars:" countChars
        CountBytes -> wrap "bytes:" countBytes
  where
    wrap prefix calc = T.pack $ prefix ++ " " ++ (show calc)
    countWords = length $ T.words text
    countLines = length $ T.lines text
    countChars = T.length text
    countBytes = B.length $ encodeUtf8 text

prepareOptions :: [RawOption] -> [WCOption]
prepareOptions rawOptions =
    case readWCOption of
        []      -> [CountWords]
        options -> options
  where
    readWCOption = [wcOption | (flag, wcOption) <- flags, flag `elem` (concat rawOptions)]
    flags = [ ('c', CountBytes)
            , ('l', CountLines)
            , ('m', CountChars)
            , ('w', CountWords)
            ]
