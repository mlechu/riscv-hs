module Cpu where

import Data.Int
import Instruction
import Memory
import Numeric (showHex)
import Util

type Register = Int32

data State = State
  { pc :: Register,
    regs :: [Register],
    halted :: Bool
  }

instance Show State where
  show (State pc' regs' _) =
    "pc: 0x" ++ showHex pc' "" ++ "\n"
      ++ "regs:\n"
      ++ concatMap
        ( \(rname, v) ->
            "x"
              ++ show rname
              ++ ": "
              ++ showHex v ""
              ++ " "
        )
        (zip [1 :: Int ..] regs')
      ++ "\n"

initCpu :: State
initCpu = State 0 [0 .. 32] False

printExecution :: State -> Memory -> IO ()
printExecution s m = do
  print s
  let s2 = runInstruction s m
  if halted s2
    then putStrLn "done"
    else printExecution s2 m

runInstruction :: State -> Memory -> State
runInstruction state mem =
  let inst = Memory.read32 mem (pc state)
   in state {halted = inst == 0, pc = pc state + 4}

-- decode :: Int32 -> Instruction
-- decode =
