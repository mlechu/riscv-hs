{-# LANGUAGE BinaryLiterals #-}
module Cpu where

import Data.Int
import Data.Bits
import Instruction
import Memory
import Numeric (showHex)
import Util

type Register = Int32

data State = State
  { pc :: Register
  , regs :: [Register]
  , halted :: Bool
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

-- decodeOp :: Int -> Instruction
decodeOp i =
  case bitsI 6 0 i of
    0b0110111 -> InstructionLUI (getRd i) (getUImm i)
    0b0010111 -> InstructionAUIPC (getRd i) (getUImm i)
    0b1101111 -> InstructionJAL (getRd i) (getJImm i)
    0b1100111 -> InstructionJALR (getFn3 i) (getRd i) (getRs1 i) (getIImm i)
    0b1100011 -> InstructionBRANCH (getFn3 i) (getRs1 i) (getRs2 i) (getBImm i)
    0b0000011 -> InstructionLOAD (getFn3 i) (getRd i) (getRs1 i) (getIImm i)
    0b0100011 -> InstructionSTORE (getFn3 i) (getRs1 i) (getRs2 i) (getSImm i)
    0b0010011 -> InstructionOP_IMM (getFn3 i) (getRd i) (getRs1 i) (getIImm i)
    0b0110011 -> InstructionOP_RR (bitsI 31 25 i) (getFn3 i) (getRd i) (getRs1 i) (getRs2 i)
    0b0001111 -> InstructionMISC_MEM (bitsI 31 28 i) (getFn3 i) (getRd i) (getRs1 i)
      (bitI 27 i) (bitI 26 i) (bitI 25 i) (bitI 24 i) (bitI 23 i) (bitI 22 i) (bitI 21 i) (bitI 20 i)
    0b1110011 -> InstructionSYSTEM (bitsI 31 20 i) (getFn3 i) (getRd i) (getRs1 i)
    _ -> error "not implemented"


-- execute :: State -> Memory -> Instruction -> State
-- execute s m (Instruction )
