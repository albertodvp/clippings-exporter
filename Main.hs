import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as L
import Main.Utf8
import Data.List
import Data.Maybe

clipSep :: L.Text
clipSep = L.pack "=========="

inputFileName :: String
inputFileName = "clippings.txt"

outputFileName :: String
outputFileName = "clippings.tsv"

type Book     = L.Text
type ClipText = L.Text
type Metadata = L.Text

data Clip = Clip Book ClipText Metadata

instance Show Clip where
  show (Clip b ct _) = intercalate "\t" (L.unpack <$> [b, ct])

rowToClip :: L.Text -> Maybe Clip
rowToClip t = let  ts = filter (not . L.null) $ L.replace (L.pack "\r\r") L.empty <$> (L.lines t)
              in case ts of
                   b:m:cts -> Just $ Clip b (L.intercalate (L.pack "\n") cts) m
                   _       -> Nothing
                   _         -> error $ "Cannot process input file"

main :: IO ()
main = withUtf8 $ do
  clipRowTexts <- L.splitOn clipSep <$> T.readFile inputFileName  
  T.writeFile outputFileName $ L.unlines $ (L.pack . show) <$> (rowToClip `mapMaybe` clipRowTexts)
  
