import Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as L
import Main.Utf8
import Text.Regex.TDFA 
import Data.Tuple.Extra

clipSep :: L.Text
clipSep = L.pack "=========="

-- TODO: Pass as argument
fileName :: String
fileName = "clippings.txt"

type Book = L.Text
type ClipText = L.Text
type Date = L.Text -- TODO: change?

data Clip = Clip Book ClipText Date

rowToClip :: L.Text -> Clip
rowToClip t = uncurry3 Clip $ extract t
  where
    extract :: L.Text -> (L.Text, L.Text, L.Text)
    extract = undefined

clipToText :: Clip -> L.Text
clipToText (Clip b ct d) = L.intercalate (L.pack ",") [b, ct, d]


main :: IO ()
main = withUtf8 $ do
  clipRowTexts <- L.splitOn clipSep <$> TIO.readFile fileName
  TIO.putStrLn $ head clipRowTexts
--  TIO.putStrLn $ head $ (clipToText . rowToClip) <$> clipRowTexts


