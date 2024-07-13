module;
#include <assert.h>

export module arm7tdmi.thumb;

import arm7tdmi.instruction;

export {
  ;

struct TShiftByImm : public DataProcessing {
  TShiftByImm(gshort_t instruction)
    : DataProcessing(
        instruction,
        Opcode::MOV,
        true,
        0,
        instruction & 0x7,
        ImmShiftOperand(
          (instruction >> 6) & 0x7,
          (instruction >> 3) & 0x7,
          (BitShift) (instruction >> 11)
        )
      ) {}
};

static constexpr gshort_t OPC_MASK = flag_mask(9);

static constexpr Opcode[2] OPCODE_MAP_ADD_SUB = {
   Opcode::ADD, Opcode::SUB
};

struct TAddSubImm : public DataProcessing {
  TAddSubImm(gshort_t instruction)
    : DataProcessing(
        instruction,
        OPCODE_MASK_ADD_SUB[OPC_MASK] & instruction,
        true,
        (instruction >> 3) & 0x7,
        instruction & 0x7,
        RotateOperand(0, (instruction >> 6) & 0x7)
      ) {}
};

struct TAddSubReg : public DataProcessing {
  TAddSubReg(gshort_t instruction)
    : DataProcessing(
        instruction,
        OPCODE_MASK_ADD_SUB[OPC_MASK & instruction],
        true,
        (instruction >> 3) & 0x7,
        instruction & 0x7,
        ImmShiftOperand(0, (instruction >> 6) & 0x7, BitShift::LEFT)
      ) {}
};

struct TDataProcessingImm : public DataProcessing {
  static constexpr Opcode[4] OPCODE_MAP = {
    Opcode::MOV,
    Opcode::CMP,
    Opcode::ADD,
    Opcode::SUB
  };

  TDataProcessingImm(gshort_t instruction)
    : DataProcessing(
        instruction,
        OPCODE_MAP[(instruction >> 11) & 0b11],
        true,
        (instruction >> 8) & 0x7,
        (instruction >> 8) & 0x7,
        RotateOperand(0, instruction & 0xFF)
      ) {}
};

struct TDataProcessing : public DataProcessing {
  TDataProcessing(gshort_t instruction)
    : DataProcessing(
        instruction,
        (Opcode) ((instruction >> 6) & 0xF),
        true,
        instruction & 0x7,
        instruction & 0x7,
        ThumbOperand(instruction)
      ) {}
};

struct TDataProcessingHiReg : public DataProcessing {
  static constexpr Opcode[4] OPCODE_MAP = {
    Opcode::ADD,
    Opcode::CMP,
    Opcode::MOV
  };

  TDataProcessingHiReg(gshort_t instruction)
    : DataProcessing(
        instruction,
        OPCODE_MAP[(instruction >> 8) & 0b11],
        true,
        (instruction & 0x7) + ((instruction >> 4) & 0x8),
        0,
        ThumbOperand(((instruction >> 3) & 0x7) + ((instruction >> 3) & 0x8))
      ) {
      assert((instruction >> 8) & 0b11 < 3);
    }
};

struct TBranchExchange : public BranchExchange {
  bool l, h2;
  byte irm;
  
  TBranchExchange(gshort_t instruction)
    : BranchExchange(

  ) {}
};

struct TBranch : public Ins {
  Cond cond;
  signed_gword_t word;
  
  TBranch(gshort_t instruction)
    : Ins(instruction),
      cond((instruction >> 8) & 0xF),
      word((signed_byte) (instruction & 0xFF)) {}

  void execute(CpuState &state) override {
    if (state.evaluate_cond(state)) {
      state.get_pc() += word << 1;
    }
  }
};

struct LiteralPoolLoad : public Ins {
  byte ird, offset;
};

struct LoadStoreReg : public Ins {
  byte opcode, irm, irn, ird;
};

struct LoadStoreImmOff : public Ins {
  bool load;
  byte offset, irn, ird;

  enum { BYTE, HALFWORD, WORD } data_type;
};

struct LoadStoreMultiple : public Ins {
  bool l;
  byte ird, offset;
};

struct AddToSPPC : public Ins {
  bool sp;
  byte ird, imm;
};

struct AdjustSP : public Ins {
  bool opc;
  byte imm;
};

struct PushPopRegisterList : public Ins {
  bool l, r;
  byte register_list;
};

struct SWBreak : public Ins {
  byte imm;
};

struct ThumbInstructionDefinition {};

struct ThumbInstruction {
  typedef variant<
    UndefinedThumbInstruction
  > InsAlg;

  InsAlg instruction;

  ThumbInstruction(gword_t instruction) : instruction(UndefinedThumbInstruction(-1)) {
    Nibbles nibbles(instruction);
  }
};


}
