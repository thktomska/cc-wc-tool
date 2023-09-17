module WordCount
    ( wordCount
    ) where

import qualified Data.List as L
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL

type RawOption = String
data WCOption = NumberOfBytes | NumberOfLines | NumberOfChars | NumberOfWords
    deriving (Eq, Show)

wordCount :: TL.Text -> [RawOption] -> TL.Text
wordCount text rawOptions =
    TL.intercalate "; " $
    map (wordCount' text) (prepareOptions rawOptions)

wordCount' :: TL.Text -> WCOption -> TL.Text
wordCount' text wcOption =
    case wcOption of
        NumberOfWords -> wrap "words:" numberOfWords
        NumberOfLines -> wrap "lines:" numberOfLines
        NumberOfChars -> wrap "chars:" numberOfChars
        NumberOfBytes -> wrap "bytes:" numberOfBytes
  where
    wrap prefix calc = TL.pack $ prefix ++ " " ++ (show calc)
    numberOfWords = L.length $ TL.words text
    numberOfLines = L.length $ TL.lines text
    numberOfChars = TL.length text
    numberOfBytes = BL.length $ TLE.encodeUtf8 text

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
