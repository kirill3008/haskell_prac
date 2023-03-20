module Lib
    ( someFunc
    ) where
--import System.IO
import System.IO.Unsafe
import Data.Aeson

someFunc :: IO ()
someFunc = putStrLn (unsafePerformIO (readFile "test.json"))
