module;
#include <assert.h>
#include <variant>

export module arm7tdmi.thumb;

import arm7tdmi.instruction;
import arm7tdmi.arm;

using std::variant;

export typedef DataProcessing::Opcode Opcode;

constexpr byte thumb_reg(gshort_t ins, byte start) {
  return (ins >> start) & 0x7;
}

static constexpr gshort_t OPC_MASK = flag_mask(9);
static constexpr Opcode OPCODE_MAP_ADD_SUB[2] = {Opcode::ADD, Opcode::SUB};

static constexpr gshort_t LOAD_MASK = flag_mask(11);

export {
  ;

  // ASR (1)
  // LSL (1)
  // LSR (1)
  // 0b000_____________
  struct TShiftImm : public DataProcessing {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition({new TZeros(3),
                                    new TIntegralPiece(2, "opcode", 0, 3),
                                    new TIntegralPiece(5, "imm5"),
                                    new TRegPiece("Rm"), new TRegPiece("Rd")});

    TShiftImm(gshort_t instruction)
        : DataProcessing(
              instruction, Opcode::MOV, true, 0, thumb_reg(instruction, 0),
              ImmShiftOperand((instruction >> 6) & 0x1F,
                              thumb_reg(instruction, 3),
                              (BitShift)((instruction >> 11) & 0x3))) {
      // Cannot do an immediate rotate right
      assert(((instruction >> 11) & 0x3) != 0x3);
    }
  };

  // ADD (3)
  // SUB (3)
  // 0b000110__________
  struct TAddSubReg : public DataProcessing {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition(
            {new TZeros(3), new TValuePiece(0b110, 3), new TBoolPiece("opcode"),
             new TRegPiece("Rm"), new TRegPiece("Rn"), new TRegPiece("Rd")});

    TAddSubReg(gshort_t instruction)
        : DataProcessing(instruction,
                         OPCODE_MAP_ADD_SUB[bool(OPC_MASK & instruction)], true,
                         thumb_reg(instruction, 3), thumb_reg(instruction, 0),
                         ThumbOperand(thumb_reg(instruction, 6))) {}
  };

  // ADD (1)
  // MOV (2)
  // SUB (1)
  // 0b000111_________
  struct TAddSubImm : public DataProcessing {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition({new TValuePiece(0b000111, 6),
                                    new TBoolPiece("opcode"),
                                    new TIntegralPiece(3, "imm3"),
                                    new TRegPiece("Rn"), new TRegPiece("Rd")});

    TAddSubImm(gshort_t instruction)
        : DataProcessing(instruction,
                         OPCODE_MAP_ADD_SUB[bool(OPC_MASK & instruction)], true,
                         thumb_reg(instruction, 3), thumb_reg(instruction, 0),
                         RotateOperand(0, thumb_reg(instruction, 6))) {}
  };

  // ADD (2)
  // CMP (1)
  // MOV (1)
  // SUB (2)
  // 0b001_____________
  struct TDataProcessingImm : public DataProcessing {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition(
            {new TValuePiece(0b001, 3), new TIntegralPiece(2, "opcode"),
             new TRegPiece("R"), new TIntegralPiece(8, "imm8")});

    static constexpr Opcode OPCODE_MAP[4] = {Opcode::MOV, Opcode::CMP,
                                             Opcode::ADD, Opcode::SUB};

    TDataProcessingImm(gshort_t instruction)
        : DataProcessing(instruction, OPCODE_MAP[(instruction >> 11) & 0b11],
                         true, thumb_reg(instruction, 8),
                         thumb_reg(instruction, 8),
                         RotateOperand(0, instruction & 0xFF)) {}
  };

  // ADC
  // AND
  // ASR (2)
  // BIC
  // CMN
  // CMP (2)
  // EOR
  // LSL (2)
  // LSR (2)
  // MVN
  // NEG
  // ORR
  // ROR
  // SBC
  // TST
  // 0b010000__________
  // except
  // 0b0100001101
  struct TDataProcessing : public DataProcessing {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition(
            {new TValuePiece(0b010000, 6), new TIntegralPiece(4, "opcode"),
             new TRegPiece("Rm/Rs"), new TRegPiece("Rd/Rn")});

    TDataProcessing(gshort_t instruction)
        : DataProcessing(instruction, (Opcode)((instruction >> 6) & 0xF), true,
                         thumb_reg(instruction, 0), thumb_reg(instruction, 0),
                         ThumbOperand(thumb_reg(instruction, 3))) {
      // Opcodes that don't line up w/ the DataProcessing::Opcode enum
      byte ir = thumb_reg(instruction, 3);
      byte shift_type = 0xFF;

      switch (opcode) {
      case Opcode::RSC:
        shift_type = BitShift::ROR;
        break;
      case Opcode::RSB:
        shift_type = BitShift::LRIGHT;
        break;
      case Opcode::SUB: // LSL
        shift_type = BitShift::LEFT;
        break;
      case Opcode::ADD: // ASR
        shift_type = BitShift::ARIGHT;
        break;
      case Opcode::MOV:
        __builtin_unreachable();
        assert(false);
      default:
      }

      if (shift_type != 0xFF) {
        assert(shift_type < 4);
        operand = RegShiftOperand(ir, ird, (BitShift)shift_type);
        opcode = Opcode::MOV;
      }
    }
  };

  // MUL
  // 0b0100001101______
  struct TMul : public MulShort {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition({new TValuePiece(0b0100001101, 10),
                                    new TRegPiece("Rm"), new TRegPiece("Rd")});

    TMul(gshort_t instruction)
        : MulShort(instruction, false, true, thumb_reg(instruction, 0), 0,
                   thumb_reg(instruction, 0), thumb_reg(instruction, 3)) {}
  };

  // MOV (3)
  // CMP (3)
  // ADD (4)
  // 0b010001__________
  struct TDataProcessingHiReg : public DataProcessing {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition({new TValuePiece(0b010001, 6),
                                    new TIntegralPiece(2, "opcode", 0b11),
                                    new TBoolPiece("H1"), new TBoolPiece("H2"),
                                    new TRegPiece("Rm"),
                                    new TRegPiece("Rd/Rn")});

    static constexpr Opcode OPCODE_MAP[4] = {Opcode::ADD, Opcode::CMP,
                                             Opcode::MOV};

    TDataProcessingHiReg(gshort_t instruction)
        : DataProcessing(instruction, OPCODE_MAP[(instruction >> 8) & 0b11],
                         false,
                         (instruction & 0x7) | ((instruction >> 4) & 0x8),
                         (instruction & 0x7) | ((instruction >> 4) & 0x8),
                         ThumbOperand((byte)((instruction >> 3) & 0xF))) {
      assert(((instruction >> 8) & 0b11) < 3);
    }
  };

  // 0b01001___________

  // BLX (2)
  // BX
  // 0b01000111________
  struct TBranchExchange : public BranchExchange {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition({new TValuePiece(0b01000111, 8),
                                    new TBoolPiece("L"), new TBoolPiece("H2"),
                                    new TRegPiece("Rm"), new TZeros(3)});

    static constexpr gshort_t MASK_LR = flag_mask(7);

    TBranchExchange(gshort_t instruction)
        : BranchExchange(instruction, instruction & MASK_LR,
                         (instruction >> 3) & 0xF) {}
  };

  // THUMB LDR (3)
  // 0b01001___________
  struct TLiteralPoolLoad : public LoadStoreOffset {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition({new TValuePiece(0b01001, 5),
                                    new TRegPiece("Rd"),
                                    new TIntegralPiece(8, "offset")});

    TLiteralPoolLoad(gshort_t instruction)
        : LoadStoreOffset(instruction, false, true, true, false, true,
                          CpuState::INDEX_PC, thumb_reg(instruction, 8),
                          LoadStoreOffset::WORD,
                          (gword_t)((instruction & 0xFF) << 2)) {}
  };

  // LDR (2)
  // LDRB (2)
  // STR (2)
  // STRB (2)
  // 0b0101____________
  struct TLoadStoreReg : public LoadStoreOffset {
    // TODO: Need to combine a few other isntructions w/ this
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition(
            {new TValuePiece(0b0101, 4), new TIntegralPiece(3, "opcode"),
             new TRegPiece("Rm"), new TRegPiece("Rn"), new TRegPiece("Rd")});

    static constexpr gword_t B_MASK = flag_mask(10);

    TLoadStoreReg(gshort_t instruction)
        : LoadStoreOffset(
              instruction, true, true, true, true, instruction & LOAD_MASK,
              thumb_reg(instruction, 3), thumb_reg(instruction, 0),
              (LoadStoreOffset::DataType)((instruction & B_MASK) >> 10),
              ImmShiftOperand(0, thumb_reg(instruction, 6), BitShift::LEFT)) {}
  };

  // LDRSB
  // LDRSH
  // 0b0101_11
  struct TLoadSigned : public LoadStore {
    static constexpr gshort_t DATA_TYPE_MASK = flag_mask(11);

    TLoadSigned(gshort_t instruction)
        : LoadStore(instruction, true, true, false, true, true,
                    thumb_reg(instruction, 3), thumb_reg(instruction, 0),
                    (instruction & DATA_TYPE_MASK) ? LoadStore::SHORT
                                                   : LoadStore::BYTE,
                    LoadStore::REGISTER, thumb_reg(instruction, 6)) {}
  };

  // LDR (1)
  // STR (1)
  // 0b0110____________
  struct TLoadStoreImm5 : public LoadStoreOffset {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition({new TValuePiece(0b011, 3),
                                    new TBoolPiece("B"), new TBoolPiece("L"),
                                    new TIntegralPiece(5, "offset"),
                                    new TRegPiece("Rn"), new TRegPiece("Rd")});

    TLoadStoreImm5(gshort_t instruction)
        : LoadStoreOffset(instruction, false, true, true, false,
                          instruction & LOAD_MASK, thumb_reg(instruction, 3),
                          thumb_reg(instruction, 0), LoadStoreOffset::WORD,
                          ((instruction >> 6) & 0x1F) * 4) {}
  };

  // LDRB (1)
  // STRB (1)
  // 0b0111____________
  // TODO: Combine w/ TLoadStoreImm5
  struct TLoadStoreByteImm5 : public LoadStoreOffset {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition({new TValuePiece(0b011, 3),
                                    new TBoolPiece("B"), new TBoolPiece("L"),
                                    new TIntegralPiece(5, "offset"),
                                    new TRegPiece("Rn"), new TRegPiece("Rd")});

    TLoadStoreByteImm5(gshort_t instruction)
        : LoadStoreOffset(instruction, false, true, true, false,
                          instruction & LOAD_MASK, thumb_reg(instruction, 3),
                          thumb_reg(instruction, 0), LoadStoreOffset::BYTE,
                          (instruction >> 6) & 0x1F) {}
  };

  // LDRH (1)
  // LDRH (2)
  // STRH (1)
  // STRH (2)
  // 0b1000____________
  struct TLoadStoreShort : public LoadStore {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition({new TValuePiece(0b1000, 4),
                                    new TBoolPiece("L"),
                                    new TIntegralPiece(5, "offset"),
                                    new TRegPiece("Rn"), new TRegPiece("Rd")});

    TLoadStoreShort(gshort_t instruction)
        : LoadStore(instruction, true, true, false, instruction & LOAD_MASK,
                    false, thumb_reg(instruction, 3), thumb_reg(instruction, 0),
                    LoadStore::SHORT, LoadStore::REGISTER, 0) {
      // Immediate operand
      if ((instruction & 0xF000) == 0x8000) {
        offset_type = IMMEDIATE;
        operand = ((instruction >> 6) & 0x1F) * 2;
      } else {
        offset_type = REGISTER;
        operand = (instruction >> 6) & 0x7;
      }
    }
  };

  // LDR (4)
  // STR (3)
  // 0b1001____________
  struct TLoadStoreImm8 : public LoadStoreOffset {
    TLoadStoreImm8(gshort_t instruction)
        : LoadStoreOffset(instruction, false, true, true, false,
                          instruction & LOAD_MASK, CpuState::INDEX_SP,
                          thumb_reg(instruction, 8), LoadStoreOffset::WORD,
                          (instruction & 0xFF) * 4) {}
  };

  // ADD (5)
  // 0b10100___________
  struct TAddWithPC : public Ins {
    byte ird, imm;

    TAddWithPC(gshort_t instruction)
        : Ins(instruction), ird(thumb_reg(instruction, 8)),
          imm(instruction & 0xFF) {}

    void execute(CpuState &state) override {
      gword_t pc = state.read_pc() & 0xFFFFFFFC;
      state.write_register(ird, pc + (imm << 2));
    }
  };

  // ADD (7)
  // SUB (4)
  // 0b10101___________
  struct TAddToSP : public DataProcessing {
    TAddToSP(gshort_t instruction)
        : DataProcessing(instruction,
                         OPCODE_MAP_ADD_SUB[(instruction >> 7) & 1], false,
                         CpuState::INDEX_SP, CpuState::INDEX_SP,
                         RotateOperand(0xF, instruction & 0x7F)) {}
  };

  // ADD (6)
  // 0b10110000________
  struct TAddWithSP : public DataProcessing {
    TAddWithSP(gshort_t instruction)
        : DataProcessing(instruction, Opcode::ADD, false, CpuState::INDEX_SP,
                         thumb_reg(instruction, 8),
                         RotateOperand(0xF, instruction & 0xFF)) {}
  };

  // PUSH
  // POP
  // 0b1011_10_________
  struct TPushPopRegisterList : public LoadStoreMultiple {
    static constexpr gshort_t POP_FLAG = flag_mask(11);
    static constexpr gshort_t R_FLAG = flag_mask(8);

    TPushPopRegisterList(gshort_t instruction)
        : LoadStoreMultiple(instruction, false, true, false, true,
                            instruction & POP_FLAG, CpuState::INDEX_SP,
                            instruction & 0xFF) {
      register_list |= (instruction & R_FLAG) << 7;
    }
  };

  // LDMIA
  // STMIA
  // 0b1100____________
  struct TLoadStoreMultiple : public LoadStoreMultiple {
    TLoadStoreMultiple(gshort_t instruction)
        : LoadStoreMultiple(instruction, false, true, false, false,
                            instruction & LOAD_MASK, thumb_reg(instruction, 8),
                            instruction & 0xFF) {
      w = !(flag_mask(irn) & register_list);
    }
  };

  // B (1)
  // 0b1101____________
  struct TConditionalBranch : public Ins {
    Cond cond;
    signed_gword_t word;

    TConditionalBranch(gshort_t instruction)
        : Ins(instruction), cond((Cond)((instruction >> 8) & 0xF)),
          word((signed_byte)(instruction & 0xFF)) {}

    void execute(CpuState &state) override {
      if (state.evaluate_cond(cond)) {
        state.write_pc(state.read_pc() + (word << 1));
      }
    }
  };

  // 0b11101__________1
  struct UndefinedThumbInstruction : public Ins {
    UndefinedThumbInstruction(gshort_t instruction) : Ins(instruction) {}
  };

  // SWI
  // 0b11011111________
  struct TSoftwareInterrupt : public SoftwareInterrupt {
    TSoftwareInterrupt(gshort_t instruction)
        : SoftwareInterrupt(instruction, instruction & 0xFF) {}
  };

  // B (2)
  // BLX (1)
  // BL
  // 0b11100___________
  struct TBranchWithLink : public Ins {
    static constexpr gshort_t MASK_OPCODE = 0x1800;
    static constexpr gshort_t MASK_OFFSET = 0x07FF;
    static constexpr gshort_t MASK_11_BIT_SIGN = 0x0400;

    enum Opcode : byte {
      UNCOND = 0,
      BRANCH_LINK_EXCHANGE = 1,
      LINK = 2,
      BRANCH_LINK = 3,
    } opcode;

    signed_gshort_t offset;

    TBranchWithLink(gshort_t instruction)
        : Ins(instruction), opcode((Opcode)((instruction >> 11) & MASK_OPCODE)),
          offset(instruction & MASK_OFFSET) {
      if (offset & MASK_11_BIT_SIGN) {
        offset |= 0xF800;
      }
    }

    void execute(CpuState &state) override {
      gword_t pc = state.read_pc();
      switch (opcode) {
      case UNCOND:
        state.write_pc(state.read_pc() + ((signed_gword_t)offset << 1));
        break;

      case BRANCH_LINK_EXCHANGE:
        state.write_pc((state.read_lr() + ((offset & MASK_OFFSET) << 1)) &
                       0xFFFFFFFC);
        state.write_lr((pc + 2) | 1);
        state.clear_flag(CpuState::T_FLAG);
        break;

      case LINK:
        state.write_lr(pc + ((signed_gword_t)offset << 12));
        break;

      case BRANCH_LINK:
        state.write_pc(state.read_lr() + ((offset & MASK_OFFSET)));
        state.write_lr((pc + 2) | 1);
        break;
      }
    }
  };

  struct ThumbInstruction {
    typedef variant<
        TShiftImm, TAddSubReg, TAddSubImm, TDataProcessingImm, TDataProcessing,
        TMul, TDataProcessingHiReg, TBranchExchange, TLiteralPoolLoad,
        TLoadStoreReg, TLoadSigned, TLoadStoreImm5, TLoadStoreByteImm5,
        TLoadStoreShort, TLoadStoreImm8, TAddWithPC, TAddToSP, TAddWithSP,
        TPushPopRegisterList, TLoadStoreMultiple, TConditionalBranch,
        UndefinedThumbInstruction, TSoftwareInterrupt, TBranchWithLink>
        InsAlg;

    InsAlg instruction;

    ThumbInstruction(gword_t instruction)
        : instruction(UndefinedThumbInstruction(-1)) {
      Nibbles nibbles(instruction);
    }
  };
}
