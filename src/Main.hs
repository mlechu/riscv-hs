module Main where

import Data.Bits
import Data.Int
import qualified Data.Map as M
-- import qualified Data.ByteString.Lazy as BSL
import Data.List (foldl')
import qualified Memory
import Numeric (showHex)

type Register = Int32

data State = State { pc :: Register
                   , regs :: [Register]
                   , mem :: Memory.Memory
                   , halted :: Bool
                   }

instance Show State where
  show (State pc regs mem _) =
    "pc: 0x" ++ showHex pc "" ++ "\n"
    ++ "regs:\n" ++ concatMap (\(rname, v) -> "x"
                               ++ show rname
                               ++ ": "
                               ++ showHex v ""
                               ++ " ") (zip regs [1..])
    ++ "\nmemory: " ++ show mem ++ "\n"


runInstruction :: State -> State
runInstruction state =
  let inst = Memory.read32 (mem state) (pc state)
  in state { halted = inst == 0, pc = pc state + 4 }


runCpu :: State -> IO ()
runCpu s = do
  let s2 = runInstruction s
  if halted s2 then print "done" else runCpu s2
  print s


main :: IO ()
main = do
  let mem = Memory.make [(0, 1), (4, 1), (8, 2)]
      cpu = State 0 [0..32] mem False
  runCpu cpu
