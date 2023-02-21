module Instruction where

type Regcode = Integer
type Imm = Integer
type InsBody = Integer

data Instruction = Instruction Opcode InsBody
  -- = InsTypeR -- register-register
  -- | InsTypeI -- register-immediate, loads, JALR, SYSTEM
  -- | InsTypeS -- stores
  -- | InsTypeU -- LUI, AUIPC
  -- | InsTypeB -- conditional branch
  -- | InsTypeJ -- unconditional jump except JALR (I)

data Opcode
  = OpcodeLUI
  | OpcodeAUIPC
  | OpcodeJAL
  | OpcodeJALR
  | OpcodeB
  | OpcodeL
  | OpcodeS
  | OpcodeRI
  | OpcodeRR
  | OpcodeFENCE
  | OpcodeSYSTEM

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
