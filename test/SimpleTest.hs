module Main where

import Control.Monad
import Cpu
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as DL
import Data.Int
import Data.List.Split
import qualified Data.Vector as V
import Memory
import Test.QuickCheck
import System.Exit

main = do
  let tests =
        [ Test "test/bins/add.bin" (V.replicate 32 0),
          Test "test/bins/add.bin" (V.replicate 32 0)
        ]
  testLoop tests

data Test = Test {path :: String, expected :: V.Vector Register}

testLoop [] = do return ExitSuccess
testLoop ((Test path expected): tail) = do
  let cpu = initCpu
  mem <- loadTest path
  let finalState = runProgram cpu mem
--   print finalState
  check $ regs finalState == expected
  testLoop tail

isPass :: Result -> Bool
isPass Success {} = True
isPass _ = False

check p = do
  r <- quickCheckResult p
  unless (isPass r) exitFailure

runProgram state mem =
  let (s', m') = runInstruction state mem
   in if halted s'
        then s'
        else runProgram s' m'

loadTest :: FilePath -> IO Memory
loadTest test = do
  bin <- DL.readFile test
  return (Memory.make (Prelude.zip (instructionNums [0 .. fromIntegral $ DL.length bin - 1]) (Prelude.map word32FromChars (chunkBs 4 bin))))

instructionNums = filter (\x -> rem x 4 == 0)

chunkBs n bs
  | DL.null bs = []
  | otherwise = DL.take n bs : chunkBs n (DL.drop n bs)

word32FromChars = B.runGet B.getWord32le
