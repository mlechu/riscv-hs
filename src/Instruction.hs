module Instruction where

import Data.Int
import Data.Bits
import Util

type Regcode = Int
type Imm = Int
type InsBody = Int

type RegDest = Regcode
type RegSrc1 = Regcode
type RegSrc2 = Regcode

-- data Instruction = Instruction Opcode InsBody
  -- = InsTypeR -- register-register
  -- | InsTypeI -- register-immediate, loads, JALR, SYSTEM
  -- | InsTypeS -- stores
  -- | InsTypeU -- LUI, AUIPC
  -- | InsTypeB -- conditional branch
  -- | InsTypeJ -- unconditional jump except JALR (I)

data Instruction
  = InstructionLUI { rd::Regcode, imm::Int }
  | InstructionAUIPC { rd::Regcode, imm::Int }
  | InstructionJAL { rd::Regcode, imm::Int }
  | InstructionJALR { funct3::Int, rd::Regcode, rs1::Regcode, imm::Int }
  | InstructionBRANCH { funct3::Int, rs1::Regcode, rs2::Regcode, imm::Int }
  | InstructionLOAD { funct3::Int, rd::Regcode, rs1::Regcode, imm::Int }
  | InstructionSTORE { funct3::Int, rs1::Regcode, rs2::Regcode, imm::Int }
  | InstructionOP_IMM { funct3::Int, rd::Regcode, rs1::Regcode, imm::Int }
  | InstructionOP_RR { funct7::Int, funct3::Int, rd::Regcode, rs1::Regcode, rs2::Regcode }
  | InstructionMISC_MEM { fm::Int, funct3::Int, rd::Regcode, rs1::Regcode,
                          pi::Int, po::Int, pr::Int, pw::Int,
                          si::Int, so::Int, sr::Int, sw::Int }
  | InstructionSYSTEM { funct12::Int, funct3::Int, rd::Regcode, rs1::Regcode }


getRd :: Int -> Int
getRd = bitsI 11 7

getRs1 :: Int -> Int
getRs1 = bitsI 19 15

getRs2 :: Int -> Int
getRs2 = bitsI 24 20

getIImm i =
  sign 12
  $ bitsI 31 20 i

getSImm i =
  sign 12
  $ shiftL (bitsI 31 25 i) 5
  .|. bitsI 11 7 i

getBImm i =
  sign 13
  $ shiftL (bitI 31 i) 12
  .|. shiftL (bitI 7 i) 11
  .|. shiftL (bitsI 30 25 i) 5
  .|. shiftL (bitsI 11 8 i) 1

getUImm i =
  sign 32
  $ shiftL (bitsI 31 12 i) 12

getJImm i =
  sign 21
  $ shiftL (bitI 31 i) 20
  .|.shiftL (bitsI 19 12 i) 12
  .|.shiftL (bitI 20 i) 11
  .|.shiftL (bitsI 30 25 i) 5
  .|.shiftL (bitsI 24 21 i) 1

getFn3 :: Int -> Int
getFn3 = bitsI 14 12


-- OCRed from the pdf lol
-- 0110111 LUI

-- 0010111 AUIPC

-- 1101111 JAL

-- 1100111 JALR

-- 1100011 BEQ
-- 1100011 BNE
-- 1100011 BLT
-- 1100011 BGE
-- 1100011 BLTU
-- 1100011 BGEU

-- 0000011 LB
-- 0000011 LH
-- 0000011 LW
-- 0000011 LBU
-- 0000011 LHU

-- 0100011 SB
-- 0100011 SH
-- 0100011 SW

-- 0010011 ADDI
-- 0010011 SLTI
-- 0010011 SLTIU
-- 0010011 XORI
-- 0010011 ORI
-- 0010011 ANDI
-- 0010011 SLLI
-- 0010011 SRLI
-- 0010011 SRAI

-- 0110011 ADD
-- 0110011 SUB
-- 0110011 SLL
-- 0110011 SLT
-- 0110011 SLTU
-- 0110011 XOR
-- 0110011 SRL
-- 0110011 SRA
-- 0110011 OR
-- 0110011 AND

-- 0001111 FENCE

-- 1110011 ECALL
-- 1110011 EBREAK



-- -- rv64
-- -- shared with 32:
-- 0000011 LWU
-- 0000011 LD

-- 0100011 SD

-- 0010011 SLLI
-- 0010011 SRLI
-- 0010011 SRAI

-- -- new:
-- 0011011 ADDIW
-- 0011011 SLLIW
-- 0011011 SRLIW
-- 0011011 SRAIW

-- 0111011 ADDW
-- 0111011 SUBW
-- 0111011 SLLW
-- 0111011 SRLW
-- 0111011 SRAW
