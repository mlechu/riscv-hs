module Main where

import Memory
import Cpu


main :: IO ()
main = do
  let mem = Memory.make [(0, 1), (4, 1), (8, 2)]
      cpu = initCpu
  printExecution cpu mem
