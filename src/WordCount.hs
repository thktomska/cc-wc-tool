module WordCount
    ( wordCount
    ) where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

type RawOption = String
data WCOption = NumberOfBytes | NumberOfLines | NumberOfChars | NumberOfWords
    deriving (Eq, Show)

wordCount :: T.Text -> [RawOption] -> T.Text
wordCount text rawOptions =
    T.intercalate "; " $
    map (wordCount' text) (prepareOptions rawOptions)

wordCount' :: T.Text -> WCOption -> T.Text
wordCount' text wcOption =
    case wcOption of
        NumberOfWords -> wrap "words:" numberOfWords
        NumberOfLines -> wrap "lines:" numberOfLines
        NumberOfChars -> wrap "chars:" numberOfChars
        NumberOfBytes -> wrap "bytes:" numberOfBytes
  where
    wrap prefix calc = T.pack $ prefix ++ " " ++ (show calc)
    numberOfWords = L.length $ T.words text
    numberOfLines = L.length $ T.lines text
    numberOfChars = T.length text
    numberOfBytes = B.length $ TE.encodeUtf8 text

prepareOptions :: [RawOption] -> [WCOption]
prepareOptions rawOptions =
    case readWCOption of
        []      -> [NumberOfWords]
        options -> options
  where
    readWCOption = [wcOption | (flag, wcOption) <- flags, flag `elem` (concat rawOptions)]
    flags = [ ('c', NumberOfBytes)
            , ('l', NumberOfLines)
            , ('m', NumberOfChars)
            , ('w', NumberOfWords)
            ]
