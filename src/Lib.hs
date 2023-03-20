module Lib
    ( someFunc
    ) where
--import System.IO
import System.IO.Unsafe
import Data.Aeson
import DataTypes
import Data.ByteString.Lazy.Internal
import Data.ByteString
someFunc :: IO ()
someFunc = do
    let fjson = fromStrict (unsafePerformIO (Data.ByteString.readFile "test.json"))
    putStrLn (show (decode fjson :: Maybe Value))
    --putStrLn (show (decode json :: Maybe [Int]))
