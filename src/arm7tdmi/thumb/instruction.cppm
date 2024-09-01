module;
#include <format>
#include <memory>
#include <string>
#include <variant>

#include <assert.h>
#include <spdlog/spdlog.h>

export module arm7tdmi.thumb;

import arm7tdmi;
import arm7tdmi.arm;
import arm7tdmi.instruction;

using std::make_unique;
using std::unique_ptr;
using std::variant;

constexpr u8 thumb_reg(u16 ins, u8 start) { return (ins >> start) & 0x7; }

static constexpr u16 OPC_MASK = flag_mask(9);
static constexpr Opcode OPCODE_MAP_ADD_SUB[2] = {Opcode::ADD, Opcode::SUB};

static constexpr u16 LOAD_MASK = flag_mask(11);

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

    TShiftImm(u16 instruction)
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

    TAddSubReg(u16 instruction)
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

    TAddSubImm(u16 instruction)
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

    TDataProcessingImm(u16 instruction)
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

    TDataProcessing(u16 instruction)
        : DataProcessing(instruction, (Opcode)((instruction >> 6) & 0xF), true,
                         thumb_reg(instruction, 0), thumb_reg(instruction, 0),
                         ThumbOperand(thumb_reg(instruction, 3))) {
      // Opcodes that don't line up w/ the DataProcessing::Opcode enum
      u8 ir = thumb_reg(instruction, 3);
      u8 shift_type = 0xFF;

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

    TMul(u16 instruction)
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

    TDataProcessingHiReg(u16 instruction)
        : DataProcessing(instruction, OPCODE_MAP[(instruction >> 8) & 0b11],
                         false,
                         (instruction & 0x7) | ((instruction >> 4) & 0x8),
                         (instruction & 0x7) | ((instruction >> 4) & 0x8),
                         ThumbOperand((u8)((instruction >> 3) & 0xF))) {
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

    static constexpr u16 MASK_LR = flag_mask(7);

    TBranchExchange(u16 instruction)
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

    TLiteralPoolLoad(u16 instruction)
        : LoadStoreOffset(instruction, false, true, true, false, true,
                          CpuState::INDEX_PC, thumb_reg(instruction, 8),
                          LoadStoreOffset::WORD,
                          (u32)((instruction & 0xFF) << 2)) {}
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

    static constexpr u32 B_MASK = flag_mask(10);

    TLoadStoreReg(u16 instruction)
        : LoadStoreOffset(
              instruction, true, true, true, false, instruction & LOAD_MASK,
              thumb_reg(instruction, 3), thumb_reg(instruction, 0),
              (LoadStoreOffset::DataType)((instruction & B_MASK) >> 10),
              ImmShiftOperand(0, thumb_reg(instruction, 6), BitShift::LEFT)) {}
  };

  // LDRSB
  // LDRSH
  // 0b0101_11
  struct TLoadSigned : public LoadStore {
    static constexpr u16 DATA_TYPE_MASK = flag_mask(11);

    TLoadSigned(u16 instruction)
        : LoadStore(instruction, true, true, false, true, true,
                    thumb_reg(instruction, 3), thumb_reg(instruction, 0),
                    (instruction & DATA_TYPE_MASK) ? LoadStore::SHORT
                                                   : LoadStore::BYTE,
                    LoadStore::REGISTER, thumb_reg(instruction, 6)) {}
  };

  // LDR (1)
  // STR (1)
  // 0b0110____________
  // LDRB (1)
  // STRB (1)
  // 0b0111____________
  struct TLoadStoreImm5 : public LoadStoreOffset {
    static inline const InstructionDefinition *definition =
        new TInstructionDefinition({new TValuePiece(0b011, 3),
                                    new TBoolPiece("B"), new TBoolPiece("L"),
                                    new TIntegralPiece(5, "offset"),
                                    new TRegPiece("Rn"), new TRegPiece("Rd")});

    static constexpr u16 BYTE_MASK = flag_mask(12);

    TLoadStoreImm5(u16 instruction)
        : LoadStoreOffset(instruction, false, true, true, false,
                          instruction & LOAD_MASK, thumb_reg(instruction, 3),
                          thumb_reg(instruction, 0), LoadStoreOffset::WORD,
                          0u) {
      data_type = instruction & BYTE_MASK ? LoadStoreOffset::BYTE
                                          : LoadStoreOffset::WORD;

      if (data_type == LoadStoreOffset::BYTE)
        operand = (u32)((instruction >> 6) & 0x1F);
      else
        operand = (u32)(((instruction >> 6) & 0x1F) * 4);
    }
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

    TLoadStoreShort(u16 instruction)
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
    TLoadStoreImm8(u16 instruction)
        : LoadStoreOffset(instruction, false, true, true, false,
                          instruction & LOAD_MASK, CpuState::INDEX_SP,
                          thumb_reg(instruction, 8), LoadStoreOffset::WORD,
                          (u32)((instruction & 0xFF) * 4)) {}
  };

  // ADD (5)
  // 0b10100___________
  struct TAddWithPC : public Ins {
    u8 ird, imm;

    TAddWithPC(u16 instruction)
        : Ins(instruction), ird(thumb_reg(instruction, 8)),
          imm(instruction & 0xFF) {}

    void execute(CpuState &state) override {
      u32 pc = state.read_pc() & 0xFFFFFFFC;
      state.write_register(ird, pc + (imm << 2));
    }
  };

  // ADD (7)
  // SUB (4)
  // 0b10101___________
  struct TAddToSP : public DataProcessing {
    TAddToSP(u16 instruction)
        : DataProcessing(instruction,
                         OPCODE_MAP_ADD_SUB[(instruction >> 7) & 1], false,
                         CpuState::INDEX_SP, CpuState::INDEX_SP,
                         RotateOperand(0xF, instruction & 0x7F)) {}
  };

  // ADD (6)
  // 0b10110000________
  struct TAddWithSP : public DataProcessing {
    TAddWithSP(u16 instruction)
        : DataProcessing(instruction, Opcode::ADD, false, CpuState::INDEX_SP,
                         thumb_reg(instruction, 8),
                         RotateOperand(0xF, instruction & 0xFF)) {}
  };

  // PUSH = STMDB SP!, <register list>
  // POP = LDMIA SP!, <register list>
  // 0b1011_10_________
  struct TPushPopRegisterList : public LoadStoreMultiple {
    static constexpr u16 POP_FLAG = flag_mask(11);
    static constexpr u16 R_FLAG = flag_mask(8);

    // dummy enumerations to emulated "named" constructors, see usage of
    // TPushPopRegisterList constructor
    enum _POP { POP };
    enum _PUSH { PUSH };

    TPushPopRegisterList(_POP, u16 instruction)
        : LoadStoreMultiple(instruction, false, true, false, true, true,
                            CpuState::INDEX_SP, instruction & 0xFF) {
      register_list |=
          (instruction & R_FLAG) ? flag_mask(CpuState::INDEX_PC) : 0;
    }

    TPushPopRegisterList(_PUSH, u16 instruction)
        : LoadStoreMultiple(instruction, true, false, false, true, false,
                            CpuState::INDEX_SP, instruction & 0xFF) {
      register_list |=
          (instruction & R_FLAG) ? flag_mask(CpuState::INDEX_LR) : 0;
    }
  };

  // LDMIA
  // STMIA
  // 0b1100____________
  struct TLoadStoreMultiple : public LoadStoreMultiple {
    TLoadStoreMultiple(u16 instruction)
        : LoadStoreMultiple(instruction, false, true, false, true,
                            instruction & LOAD_MASK, thumb_reg(instruction, 8),
                            instruction & 0xFF) {}
  };

  // B (1)
  // 0b1101____________
  struct TConditionalBranch : public Ins {
    Cond cond;
    i32 word;

    TConditionalBranch(u16 instruction)
        : Ins(instruction), cond((Cond)((instruction >> 8) & 0xF)),
          word((i8)(instruction & 0xFF)) {}

    void execute(CpuState &state) override {
      if (state.evaluate_cond(cond)) {
        state.write_pc(state.read_pc() + (word << 1));
      }
    }
  };

  // 0b11101__________1
  struct UndefinedThumbInstruction : public Ins {
    UndefinedThumbInstruction(u16 instruction) : Ins(instruction) {}

    void execute(CpuState &state) {}

    std::string disassemble() override {
      return std::format("{:#4x} <UNSUPPORTED THUMB INSTRUCTION>",
                         nibbles.word);
    }
  };

  // SWI
  // 0b11011111________
  struct TSoftwareInterrupt : public SoftwareInterrupt {
    TSoftwareInterrupt(u16 instruction)
        : SoftwareInterrupt(instruction, instruction & 0xFF) {}
  };

  // B (2)
  // BLX (1)
  // BL
  // 0b11100___________
  struct TBranchWithLink : public Ins {
    static constexpr u16 MASK_OPCODE = 0x1800;
    static constexpr u16 MASK_OFFSET = 0x07FF;
    static constexpr u16 MASK_11_BIT_SIGN = 0x0400;

    enum Opcode : u8 {
      UNCOND = 0,
      BRANCH_LINK_EXCHANGE = 1,
      LINK = 2,
      BRANCH_LINK = 3,
    } opcode;

    i16 offset;

    TBranchWithLink(u16 instruction)
        : Ins(instruction), opcode((Opcode)((instruction >> 11) & 0b11)),
          offset(instruction & MASK_OFFSET) {
      if (offset & MASK_11_BIT_SIGN) {
        offset |= 0xF800;
      }
    }

    void execute(CpuState &state) override {
      u32 current_pc = state.read_current_pc();
      switch (opcode) {
      case UNCOND:
        state.write_pc(state.read_pc() + ((i32)offset << 1));
        break;

      case BRANCH_LINK_EXCHANGE:
        state.write_pc((state.read_lr() + ((offset & MASK_OFFSET) << 1)) &
                       0xFFFFFFFC);
        state.write_lr((current_pc + 2) | 1);
        state.clear_flag(CpuState::T_FLAG);
        break;

      case LINK: {
        u32 x = state.read_pc() + (i32)(offset << 12);
        state.write_lr(x);
        break;
      }

      case BRANCH_LINK: {
        state.write_pc(state.read_lr() + ((offset & MASK_OFFSET) << 1));
        state.write_lr((current_pc + 2) | 1);
        break;
      }
      }
    }
  };

  unique_ptr<Ins> block_1011(u16 instruction) {
    switch ((instruction >> 8) & 0xF) {
    case 0b0000: // adjust sp
      return make_unique<TAddToSP>(instruction);
    case 0b0100:
    case 0b0101:
    case 0b1100:
    case 0b1101: // push pop rl
      if (TPushPopRegisterList::POP_FLAG & instruction)
        return make_unique<TPushPopRegisterList>(TPushPopRegisterList::POP,
                                                 instruction);
      else
        return make_unique<TPushPopRegisterList>(TPushPopRegisterList::PUSH,
                                                 instruction);

    case 0b1110: // sw break / not implemented
      return make_unique<UndefinedThumbInstruction>(instruction);
    }
  }

  unique_ptr<Ins> block_010001(u16 instruction) {
    switch (instruction >> 8) {
    case 0b01000100:
    case 0b01000101:
    case 0b01000110:
      return make_unique<TDataProcessingHiReg>(instruction);
    case 0b01000111:
      return make_unique<TBranchExchange>(instruction);
    default:
      assert(false);
      return unique_ptr<Ins>();
    }
  }

  // clang-format off
  const std::function<unique_ptr<Ins>(u16)> thumb_ins_map[64] = {
      // 0b000000 - 0b000101 shift by imm
      make_unique<TShiftImm, u16>,
      make_unique<TShiftImm, u16>,
      make_unique<TShiftImm, u16>,
      make_unique<TShiftImm, u16>,
      make_unique<TShiftImm, u16>,
      make_unique<TShiftImm, u16>,
      // 0b000110 add sub reg
      make_unique<TAddSubReg, u16>,
      // 0b000111
      make_unique<TAddSubImm, u16>,
      // 0b001000 - 0b001111
      make_unique<TDataProcessingImm, u16>,
      make_unique<TDataProcessingImm, u16>,
      make_unique<TDataProcessingImm, u16>,
      make_unique<TDataProcessingImm, u16>,
      make_unique<TDataProcessingImm, u16>,
      make_unique<TDataProcessingImm, u16>,
      make_unique<TDataProcessingImm, u16>,
      make_unique<TDataProcessingImm, u16>,
      // 0b010000
      make_unique<TDataProcessing, u16>,
      // 0b010001
      block_010001,
      // 0b010010 - 0b010011 load from literal pool
      make_unique<TLiteralPoolLoad, u16>,
      make_unique<TLiteralPoolLoad, u16>,
      // 0b010100 - 0b010111 load store reg offset
      make_unique<TLoadStoreReg, u16>,
      make_unique<TLoadStoreReg, u16>,
      make_unique<TLoadStoreReg, u16>,
      make_unique<TLoadStoreReg, u16>,
      // 0b011000 - 0b011111
      make_unique<TLoadStoreImm5, u16>,
      make_unique<TLoadStoreImm5, u16>,
      make_unique<TLoadStoreImm5, u16>,
      make_unique<TLoadStoreImm5, u16>,
      make_unique<TLoadStoreImm5, u16>,
      make_unique<TLoadStoreImm5, u16>,
      make_unique<TLoadStoreImm5, u16>,
      make_unique<TLoadStoreImm5, u16>,
      // 0b100000 - 0b100011
      make_unique<TLoadStoreShort, u16>,
      make_unique<TLoadStoreShort, u16>,
      make_unique<TLoadStoreShort, u16>,
      make_unique<TLoadStoreShort, u16>,
      // 0b100100 - 0b100111
      make_unique<TLoadStoreImm8, u16>,
      make_unique<TLoadStoreImm8, u16>,
      make_unique<TLoadStoreImm8, u16>,
      make_unique<TLoadStoreImm8, u16>,
      // 0b101000 - 0b101001
      make_unique<TAddWithPC, u16>,
      make_unique<TAddWithPC, u16>,
      // 0b101010 - 0b101011
      make_unique<TAddWithSP, u16>,
      make_unique<TAddWithSP, u16>,
      // 0b101100 - 0b101111 misc
      block_1011,
      block_1011,
      block_1011,
      block_1011,
      // 0b110100 - 0b110111 load store multiple
      make_unique<TLoadStoreMultiple, u16>,
      make_unique<TLoadStoreMultiple, u16>,
      make_unique<TLoadStoreMultiple, u16>,
      make_unique<TLoadStoreMultiple, u16>,
      // 0b110100 - 0b110111 conditional branch
      make_unique<TConditionalBranch, u16>,
      make_unique<TConditionalBranch, u16>,
      make_unique<TConditionalBranch, u16>,
      make_unique<TConditionalBranch, u16>,
      // 0b11100x unconditional branch
      make_unique<TBranchWithLink, u16>,
      make_unique<TBranchWithLink, u16>,
      // 0b1101x BLX suffix
      // node that when these instructions end with 1, they are technically
      // undefined but that doesn't really matter since GBA machine code
      // will never contain that
      make_unique<TBranchWithLink, u16>,
      make_unique<TBranchWithLink, u16>,
      // 0b11110x bl/blx prefix
      make_unique<TBranchWithLink, u16>,
      make_unique<TBranchWithLink, u16>,
      // 0b11111x bl
      make_unique<TBranchWithLink, u16>,
      make_unique<TBranchWithLink, u16>,
  };

  // clang-format on

  struct ThumbInstruction {
    unique_ptr<Ins> instruction;

    ThumbInstruction(u16 instruction)
        : instruction(thumb_ins_map[instruction >> 10](instruction)) {
      this->instruction->cond = Cond::AL;
    }
  };
}
