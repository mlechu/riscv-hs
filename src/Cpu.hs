{-# LANGUAGE BinaryLiterals #-}
module Cpu where

import Data.Int
import Data.Bits
import qualified Data.Vector as V
import Data.Word
import Instruction
import Memory
import Numeric (showHex)
import Util

type Register = Int32

data State = State
  { pc :: Register
  , regs :: V.Vector Register
  , halted :: Bool
  }

-- modify register file. Note x0 is wired to 0, and writes are no-ops.
regset :: State -> Regcode -> Register -> State
regset s 0 _ = s
-- regset rs i v = V.update rs (V.singleton (fromIntegral i, v))
regset s i v =
  s { regs = V.update (regs s) (V.singleton (fromIntegral i, v)) }

regget :: State -> Regcode -> Register
regget s 0 = 0
regget s i = (regs s) V.! (fromIntegral i)

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
        (zip [1 :: Int ..] (V.toList regs'))
      ++ "\n"

initCpu :: State
initCpu = State 0 (V.replicate 32 0) False

printExecution :: State -> Memory -> IO ()
printExecution s m = do
  print s
  let s2 = runInstruction s m
  if halted s2
    then putStrLn "done"
    else printExecution s2 m

runInstruction :: State -> Memory -> State
runInstruction state mem =
  let inst = Memory.read32 mem $ fromIntegral (pc state)
  -- halt when we see a zero instruction
  -- (this is okay for now; our uninitialized memory will default to 0)
  in state {halted = inst == 0, pc = pc state + 4}

decodeOp :: Int32 -> Instruction
decodeOp i =
  case bitsI 6 0 i of
    0b0110111 -> OpLUI (getRd i) (getUImm i)
    0b0010111 -> OpAUIPC (getRd i) (getUImm i)
    0b1101111 -> OpJAL (getRd i) (getJImm i)
    0b1100111 -> OpJALR (getFn3 i) (getRd i) (getRs1 i) (getIImm i)
    0b1100011 -> OpBRANCH (getFn3 i) (getRs1 i) (getRs2 i) (getBImm i)
    0b0000011 -> OpLOAD (getFn3 i) (getRd i) (getRs1 i) (getIImm i)
    0b0100011 -> OpSTORE (getFn3 i) (getRs1 i) (getRs2 i) (getSImm i)
    0b0010011 -> OpOP_IMM (getFn3 i) (getRd i) (getRs1 i) (getIImm i)
    0b0110011 -> OpOP_RR (bitsI 31 25 i) (getFn3 i) (getRd i) (getRs1 i) (getRs2 i)
    0b0001111 -> OpMISC_MEM (bitsI 31 28 i) (getFn3 i) (getRd i) (getRs1 i)
      (bitsI 27 24 i) (bitsI 23 20 i)
    0b1110011 -> OpSYSTEM (bitsI 31 20 i) (getFn3 i) (getRd i) (getRs1 i)
    _ -> error "not implemented"


execute :: State -> Instruction -> State
execute s (OpLUI rd imm) =
  regset s rd imm

execute s (OpAUIPC rd imm) =
  regset s rd (pc s + imm)

execute s (OpJAL rd imm) =
  let pc' = pc s + 4 in
    (regset s rd pc') { pc = pc s + imm }

execute s (OpJALR fn3 rd rs1 imm ) =
  let pc' = pc s + 4 in
    (regset s rd pc') { pc = regget s rs1 + imm }

execute s (OpBRANCH fn3 rs1 rs2 imm ) =

  s

execute s (OpLOAD fn3 rd rs1 imm ) =
  s -- TODO: memory

execute s (OpSTORE fn3 rs1 rs2 imm ) =
  s -- TODO: memory

execute s (OpOP_IMM fn3 rd rs1 imm ) =
  s

execute s (OpOP_RR fn7 fn3 rd rs1 rs2 ) =
  -- case fn7 of
  s

execute s (OpMISC_MEM fm fn3 rd rs1 pred succ) =
  s

execute s (OpSYSTEM fn12 fn3 rd rs1) =
  s
