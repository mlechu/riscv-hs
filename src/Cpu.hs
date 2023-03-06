{-# LANGUAGE BinaryLiterals #-}

module Cpu where

import Data.Bits
import Data.Int
import qualified Data.Vector as V
import Data.Word
import Instruction
import Memory
import Numeric (showHex)
import Util
import Debug.Trace

type Register = MemWord

data State = State
  { pc :: Register,
    regs :: V.Vector Register,
    halted :: Bool
  }

-- data ExecutionEnv = ExecutionEnv
--   { xlen :: Xlen,
--     ecalls :: Map
--   }

-- data Xlen = Xlen32 | Xlen64 | Xlen128

-- modify register file. Note x0 is wired to 0, and writes are no-ops.
regset :: State -> Regcode -> Register -> State
regset s 0 _ = s
regset s i v =
  s
    { regs =
        V.update
          (regs s)
          (V.singleton (i, v))
    }

regget :: State -> Regcode -> Register
regget _ 0 = 0
regget s i = regs s V.! i

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
        (zip [0 :: Int ..] (V.toList regs'))
      ++ "\n"

-- It's OK to have sp (x2) point to 0x0 at initialization, since it
-- points to "used" memory and grows towards smaller addresses
-- i.e. sp = 0x0 pushes its first word the "bottom" (highest address) in memory
initCpu :: State
initCpu = State 0 (V.replicate 32 0) False

printExecution :: State -> Memory -> IO ()
printExecution s m = do
  print s
  print $ decodeOp $ Memory.read32 m $ fromIntegral (pc s)
  let (s', m') = runInstruction s m
  if halted s'
    then do
      print m'
      putStrLn "halted"
    else printExecution s' m'

runInstruction :: State -> Memory -> (State, Memory)
runInstruction s m =
  let i = decodeOp $ Memory.read32 m $ fromIntegral (pc s)
      m' = updateMemory s m i
      s' = execute s m i
      s'' = updatePc s' i
   in (s'', m')

-- halt when we see a zero instruction
-- (this is okay for now; our uninitialized memory will default to 0
-- and 0 instructions are purposefully undefined)
-- will also probably reject compressed instructions

-- decode: int -> ins
-- execute: state -> ins -> state
-- mem: mem -> ins -> mem
-- pc: state -> ins -> state

decodeOp :: Word32 -> Instruction
decodeOp i =
  case (bitsI 6 0 i) :: Word32 of
    0b0110111 -> OpLUI (getRd i) (getUImm i)
    0b0010111 -> OpAUIPC (getRd i) (getUImm i)
    0b1101111 -> OpJAL (getRd i) (getJImm i)
    0b1100111 -> OpJALR (getFn3 i) (getRd i) (getRs1 i) (getIImm i)
    0b1100011 -> OpBRANCH (getFn3 i) (getRs1 i) (getRs2 i) (getBImm i)
    0b0000011 -> OpLOAD (getFn3 i) (getRd i) (getRs1 i) (getIImm i)
    0b0100011 -> OpSTORE (getFn3 i) (getRs1 i) (getRs2 i) (getSImm i)
    0b0010011 -> OpOP_IMM (getFn3 i) (getRd i) (getRs1 i) (getIImm i)
    0b0110011 -> OpOP_RR (bitsI 31 25 i) (getFn3 i) (getRd i) (getRs1 i) (getRs2 i)
    0b0001111 ->
      OpMISC_MEM
        (bitsI 31 28 i)
        (getFn3 i)
        (getRd i)
        (getRs1 i)
        (bitsI 27 24 i)
        (bitsI 23 20 i)
    0b1110011 -> OpSYSTEM (bitsI 31 20 i) (getFn3 i) (getRd i) (getRs1 i)
    _ -> OpUNIMP i

slt :: (Ord a) => a -> a -> Register
slt r1 r2 = if r1 < r2 then 1 else 0

-- Decode full instruction, then execute the instruction and update registers
execute :: State -> Memory -> Instruction -> State
execute s _ (OpLUI rd imm) = regset s rd imm
execute s _ (OpAUIPC rd imm) = regset s rd (pc s + imm)
execute s _ (OpJAL rd imm) = regset s rd (pc s + 4)
execute s m (OpJALR fn3 rd rs1 imm) = regset s rd (pc s + 4)
execute s m (OpLOAD fn3 rd rs1 imm) =
  regset s rd $ fromIntegral val
  where
    offset = regget s rs1 + imm
    val = case fn3 of
      0b000 -> -- LB
        signExt 1 $ readUnaligned m (regget s rs1 + imm) 1
      0b001 -> -- LH
        signExt 1 $ readUnaligned m (regget s rs1 + imm) 2
      0b010 -> -- LW
        readUnaligned m (regget s rs1 + imm) 4
      0b100 -> -- LBU
        readUnaligned m (regget s rs1 + imm) 1
      0b101 -> -- LHU
        readUnaligned m (regget s rs1 + imm) 2
      _ -> error "not implemented"
execute s m (OpOP_IMM fn3 rd rs1 imm) =
  regset s rd $ op (regget s rs1) imm
  where
    op = case fn3 of
      0b000 -> (+) -- ADDI
      0b010 -> signedOp slt -- SLTI
      0b011 -> slt -- SLTIU
      0b100 -> xor -- XORI
      0b110 -> (.|.) -- ORI
      0b111 -> (.&.) -- ANDI
      0b001 -> shL -- SLLI
      0b101 ->
        let shamt = bitsI 4 0 imm
        in case bitsI 11 5 imm of
          0b0000000 -> \r _ -> shRLog r shamt  -- SRLI c
          0b0100000 -> \r _ -> shRAri r shamt -- SRAI c
          _ -> error "not implemented"
      _ -> error "not implemented"
execute s m (OpOP_RR fn7 fn3 rd rs1 rs2) =
  regset s rd $ op (regget s rs1) (regget s rs2)
  where
    op = case fn3 of
      0b000 -> case fn7 of
        0b0000000 -> (+) -- ADD
        0b0100000 -> (-) -- SUB
        _ -> error "not implemented"
      0b001 -> shL -- SLL
      0b010 -> signedOp slt -- SLT
      0b011 -> slt -- SLTU
      0b100 -> xor  -- XOR
      0b101 ->
        let shamt = bitsI 4 0 rs2
        in case fn7 of
          0b0000000 -> \r1 _ -> shRLog r1 shamt -- SRL
          0b0100000 -> \r1 _ -> shRAri r1 shamt -- SRA
          _ -> error "not implemented"
      0b110 -> (.|.) -- OR
      0b111 -> (.&.) -- AND
      _ -> error "not implemented"
-- execute s m (OpMISC_MEM fm fn3 rd rs1 pred succ) =
--   s -- I don't think this needs to be implemented; only one thread!
execute s m (OpSYSTEM fn12 fn3 rd rs1) =
  case fn12 of
    0 -> -- ECALL
      s
    1 -> -- EBREAK
      s
    _ -> error "not implemented"
execute s m _ = s

updateMemory :: State -> Memory -> Instruction -> Memory
updateMemory s m (OpSTORE fn3 rs1 rs2 imm) =
  let ea = regget s rs1 + imm
      val = fromIntegral $ regget s rs2
  in case fn3 of
      0b000 -> -- SB
        write8 m ea (fromIntegral $ regget s rs2)
  -- write8 :: Memory -> Address -> Word8 -> Memory
      0b001 -> -- SH
        writeUnaligned m ea 2 val
      0b010 -> -- SW
        writeUnaligned m ea 4 val
      _ -> error "not implemented"
updateMemory _ m _ = m

-- update program counter on jumps/branches
-- halt on unimplemented instructions
-- otherwise just add 4
updatePc :: State -> Instruction -> State
updatePc s (OpJAL rd imm) =
  s {pc = pc s + imm}
updatePc s (OpJALR fn3 rd rs1 imm) =
  s {pc = regget s rs1
      - mod (regget s rs1) 1
      + imm}
updatePc s (OpBRANCH fn3 rs1 rs2 imm) =
  s {pc = pc s + if f (regget s rs1) (regget s rs2) then imm else 4}
  where
    f = case fn3 of
      0b000 -> (==) -- BEQ
      0b001 -> (/=) -- BNE
      0b100 -> signedOp (<) -- BLT
      0b101 -> signedOp (>=) -- BGE
      0b110 -> (<) -- BLTU
      0b111 -> (>=) -- BGEU
      _ -> error "not implemented"
-- Treat unimplemented instruction or uninitialized memory as halt
updatePc s (OpUNIMP i) =
  s {halted = True}
-- Treat syscall as halt
updatePc s (OpSYSTEM _ _ _ _) =
  s {halted = True}
updatePc s _ = s {pc = pc s + 4}
