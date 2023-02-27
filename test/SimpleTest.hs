module Main where

import Cpu
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as DL
import Data.List.Split
import Data.Int
import Memory
import System.Directory
import System.Exit

main = do
  dirs <- listDirectory "test/bins"
  testLoop dirs

data Test = Test { dir :: String, expected :: Int32 }

testLoop [] = do return ()
testLoop (path : t) = do
  let cpu = initCpu
  mem <- loadTest $ "test/bins/" ++ path
  printExecution cpu mem
  testLoop t

loadTest :: FilePath -> IO Memory
loadTest test = do
  bin <- DL.readFile test
  return (Memory.make (Prelude.zip (instructionNums [0 .. fromIntegral $ DL.length bin - 1]) (Prelude.map word32FromChars (chunkBs 4 bin))))

instructionNums = filter (\x -> rem x 4 == 0)

chunkBs n bs
  | DL.null bs = []
  | otherwise = DL.take n bs : chunkBs n (DL.drop n bs)

word32FromChars = B.runGet B.getWord32le
