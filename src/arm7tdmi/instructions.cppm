#ifndef GBA_ARM7TDMI_INSTRUCTIONS
#define GBA_ARM7TDMI_INSTRUCTIONS
module;
#include <iostream>
#include <variant>
#include <vector>

export module arm7tdmi.instructions;

import bitutil;
import types;

import arm7tdmi.instruction_definition;
import arm7tdmi.cpu_state;

using std::vector;
using std::variant;

export {

struct Ins {
  Nibbles nibbles;

  Ins(gword_t instruction) : nibbles(instruction) {}

  virtual void execute(CpuState &cpu_state) {
    std::cout << "Executing\n";
  }
};

struct MulShort : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new Zeros(6), new BoolPiece("A"), new BoolPiece("S"), new IntegralPiece(4, "Rd"),
    new IntegralPiece(4, "Rn"), new ValuePiece(0b1001, 4), new IntegralPiece(4, "Rm")
  });

  static inline const gword_t MASK_A = flag_mask(21);
  static inline const gword_t MASK_S = flag_mask(20);

  bool a, s;
  byte ird, irn, irs, irm;

  MulShort(gword_t instruction) 
    : Ins(instruction),
      a(MASK_A & instruction),
      s(MASK_S & instruction),
      ird(nibbles[4]),
      irn(nibbles[3]),
      irs(nibbles[2]),
      irm(nibbles[0]) {
  }
};

struct MulLong : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new Zeros(4), new Ones(1), new BoolPiece("U"), new BoolPiece("A"), new BoolPiece("S"),
    new IntegralPiece(4, "RdHi"), new IntegralPiece(4, "RdLo"), new ValuePiece(0b1001, 4), new IntegralPiece(4, "Rm")
  });

  static inline const gword_t MASK_U = flag_mask(21);
  static inline const gword_t MASK_A = flag_mask(21);
  static inline const gword_t MASK_S = flag_mask(20);

  bool u, a, s;
  byte ird_msw, ird_lsw, irn, irm;
 
  MulLong(gword_t instruction) 
    : Ins(instruction), 
      u(MASK_U & instruction),
      a(MASK_A & instruction),
      s(MASK_S & instruction),
      ird_msw(nibbles[4]),
      ird_lsw(nibbles[3]),
      irn(nibbles[2]),
      irm(nibbles[0]) { }
};

struct SingleDataSwap : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b00010, 5), new BoolPiece("B"), new Zeros(2), new IntegralPiece(4, "Rn"),
    new IntegralPiece(4, "Rd"), new Zeros(4), new ValuePiece(0b1001, 4), new IntegralPiece(4, "Rm")
  });

  static inline const gword_t MASK_B = flag_mask(22);

  bool b;
  byte irn, ird, irm;

  SingleDataSwap(gword_t instruction) 
    : Ins(instruction),
      b(MASK_B & instruction),
      irn(nibbles[4]),
      ird(nibbles[3]),
      irm(nibbles[0]) { }
};

struct Load : public Ins {
  static inline const vector<const InstructionDefinition *> definitions = {
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0, 3), new BoolPiece("P"), new BoolPiece("U"), new Zeros(1), new BoolPiece("W"), new BoolPiece("L"),
      new IntegralPiece(4, "Rn"), new IntegralPiece(4, "Rd"), new Zeros(4), new ValuePiece(0b1011, 4), new IntegralPiece(4, "Rm")
    }),
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0, 3), new BoolPiece("P"), new BoolPiece("U"), new Ones(1), new BoolPiece("W"), new BoolPiece("L"),
      new IntegralPiece(4, "Rn"), new IntegralPiece(4, "Rd"), new IntegralPiece(4, "HiOffset"), 
      new ValuePiece(0b1011, 4), new IntegralPiece(4, "LoOffset")
    }),
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0, 3), new BoolPiece("P"), new BoolPiece("U"), new Zeros(1), new BoolPiece("W"), new Zeros(1),
      new IntegralPiece(4, "Rn"), new IntegralPiece(4, "Rd"), new Zeros(4), new Ones(2), new BoolPiece("S"), new Ones(1),
      new IntegralPiece(4, "Rm")
    }),
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0, 3), new BoolPiece("P"), new BoolPiece("U"), new Zeros(1), new BoolPiece("W"), new Ones(1),
      new IntegralPiece(4, "Rn"), new IntegralPiece(4, "Rd"), new Zeros(4), new Ones(2), new BoolPiece("H"), new Ones(1),
      new IntegralPiece(4, "Rm")
    }),
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0, 3), new BoolPiece("P"), new BoolPiece("U"), new Ones(1), new BoolPiece("W"), new Zeros(1),
      new IntegralPiece(4, "Rn"), new IntegralPiece(4, "Rd"), new IntegralPiece(4, "HiOffset"), new Ones(2), new BoolPiece("S"), new Ones(1),
      new IntegralPiece(4, "LoOffset")
    }),
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0, 3), new BoolPiece("P"), new BoolPiece("U"), new Ones(1), new BoolPiece("W"), new Ones(1),
      new IntegralPiece(4, "Rn"), new IntegralPiece(4, "Rd"), new IntegralPiece(4, "HiOffset"), new Ones(2), new BoolPiece("S"), new Ones(1),
      new IntegralPiece(4, "LoOffset")
    }),
  };

  static const gword_t MASK_P = flag_mask(24);
  static const gword_t MASK_U = flag_mask(23);
  static const gword_t MASK_B = flag_mask(22);
  static const gword_t MASK_W = flag_mask(21);
  static const gword_t MASK_L = flag_mask(20);
  static const gword_t MASK_H = flag_mask(5);

  bool p, u, w, l, h;
  byte irn, ird;

  enum IntegralType { SHORT = 2, SIGNED_SHORT = 4, LONG = 8 } integral_type;
  enum OffsetType { REGISTER = 0, IMMEDIATE = 1 } offset_type;
  
  byte operand;

  Load(gword_t instruction) 
    : Ins(instruction),
      p(MASK_P & instruction),
      u(MASK_U & instruction),
      w(MASK_W & instruction),
      l(MASK_L & instruction),
      h(MASK_H & instruction),
      irn(nibbles[4]),
      ird(nibbles[3]) {

    gword_t b = MASK_B & instruction;
  
    offset_type = b ? IMMEDIATE : REGISTER;

    switch (offset_type) {
      case REGISTER:
        operand = nibbles[0];
        break;
      case IMMEDIATE:
        operand = nibbles[0] & (nibbles[2] << 4);
        break;
    }

    switch (nibbles[1]) {
      case 0b1011:
        integral_type = SHORT;
        break;
      case 0b1111:
      case 0b1101:
        if (l) {
          integral_type = SIGNED_SHORT;
        } else {
          integral_type = LONG;
        }
        break;
      default:
        break;
    }
  }

};

enum BitShift : byte {
  LEFT    = 0b00,
  LRIGHT  = 0b01,
  ARIGHT  = 0b10,
  ROR     = 0b11
};

struct ShifterOperandValue {
  gword_t value;
  gword_t carry;
};

struct ShifterOperand {
  virtual ShifterOperandValue evaluate(CpuState &state) = 0;
};

struct RotateOperand : public ShifterOperand {
  byte imm, rotate;

  RotateOperand(gword_t instruction) {
    Nibbles nibbles(instruction);
    
    rotate = nibbles[2];
    imm = instruction & 0xFF;
  }

  ShifterOperandValue evaluate(CpuState &state) override {
    constexpr gword_t MASK = flag_mask(31);

    gword_t shifter = ror<gword_t>(imm, rotate * 2);
    gword_t carry = rotate ? shifter & MASK : state.get_flag(CpuState::C_FLAG);
    
    return { shifter, carry };
  }
};

struct ImmShiftOperand : public ShifterOperand {
  byte shift_by, irm;
  BitShift shift_type;

  ImmShiftOperand(gword_t instruction) {
    Nibbles nibbles(instruction);

    shift_by = (byte) ((instruction & 0xF80) >> 6);
    irm = nibbles[0];
    shift_type = (BitShift) ((nibbles[1] & 0b0110) >> 1);
  }

  ShifterOperandValue evaluate(CpuState &state) override {
    constexpr gword_t MASK = flag_mask(31);
    gword_t reg = state.get_register(irm);

    if (irm == 0xF)
      reg += 8;
    
    gword_t value, carry;
    if (shift_by) {
      carry = bool(flag_mask(shift_by - 1) & reg);
      switch (shift_type) {
        case LEFT:
          value = reg << shift_by;
          carry = bool(asr<signed_gword_t>(reg, 32 - shift_by) & reg);
          break;

        case LRIGHT:
          value = lsr<gword_t>(reg, shift_by);
          break;
          
        case ARIGHT:
          value = asr<signed_gword_t>(reg, shift_by);
          break;

        case ROR:
          value = ror<gword_t>(reg, shift_by);
          break;
      }
    } else {
      carry = bool(reg & MASK);

      switch (shift_type) {
        case LEFT: {
          value = reg;
          carry = state.get_flag(CpuState::C_FLAG);
          break;
        }
        case LRIGHT: {
          value = 0;
          carry = MASK & 1;
          break;
        }
          
        case ARIGHT: {
          if (reg & MASK)
            value = -1;
          else
            value = 0;

          break;
        }
        case ROR: {
          value = (bool(state.get_flag(CpuState::C_FLAG)) << 31) | (reg >> 1);
          carry = reg & 1;
          break;
        }
      }
    }

    return { value, carry };
  }
};

struct RegShiftOperand : public ShifterOperand {
  byte irs, irm;
  BitShift shift_type;

  RegShiftOperand(gword_t instruction) {
    Nibbles nibbles(instruction);

    irs = nibbles[2];
    irm = nibbles[0];
    shift_type = (BitShift) ((nibbles[1] & 0b0110) >> 1);
  }
  
  ShifterOperandValue evaluate(CpuState &state) override {
    gword_t rs = state.get_register(irs);
    gword_t rm = state.get_register(irm);

    gword_t shift = rs & 0xFF;
    
    gword_t value, carry;

    switch (shift_type) {
      case LEFT:
        if (shift == 0) {
          value = rm;
          carry = bool(state.get_flag(CpuState::C_FLAG));
        } else if (shift < 32) {
          value = rm << shift;
          carry = bool(flag_mask(32 - shift) & rm);
        } else if (shift == 32) {
          value = 0;
          carry = rm & 1;
        } else {
          value = 0;
          carry = 0;
        }
        break;

      case LRIGHT:
        if (shift == 0) {
          value = rm;
          carry = bool(state.get_flag(CpuState::C_FLAG));
        } else if (shift < 32) {
          value = lsr<gword_t>(rm, shift);
          carry = bool(flag_mask(shift - 1) & rm);
        } else if (shift == 32) {
          value = 0;
          carry = bool(flag_mask(31) & rm);
        } else {
          value = 0;
          carry = 0;
        }
        break;

      case ARIGHT:
        if (shift == 0) {
          value = rm;
          carry = bool(state.get_flag(CpuState::C_FLAG));
        } else if (shift < 32) {
          value = asr<signed_gword_t>(rm, shift);
          carry = bool(flag_mask(shift - 1) & rm);
        } else {
          carry = flag_mask(31) & rm;
          if (flag_mask(31) & rm) {
            value = -1;
          } else {
            value = 0;
          }
        }
        break;

      case ROR: {
        gword_t rotation = rs & 0x1F;
        if (shift == 0) {
          value = rm;
          carry = bool(state.get_flag(CpuState::C_FLAG));
        } else if (rotation == 0) {
          value = rm;
          carry = bool(flag_mask(31) & rm);
        } else {
          value = ror<gword_t>(rm, rotation);
          carry = bool(flag_mask(rotation - 1) & rm);
        }
        break;
      }
    }

    return { value, carry };
  }
};

constexpr gword_t SIGN_BIT = flag_mask(31);

struct CheckedResult {
  gword_t value, carry, overflow;

  static CheckedResult add(gword_t x, gword_t y) {
    gword_t result = x + y;
    gword_t carry = result < x;
    gword_t overflow = (x & SIGN_BIT) == (y & SIGN_BIT) && (result & SIGN_BIT) != (x & SIGN_BIT);
    return { result, carry, overflow };
  }

  static CheckedResult sub(gword_t x, gword_t y) {
    gword_t result = x - y;
    gword_t carry = result > x;
    gword_t overflow = (x & SIGN_BIT) != (y & SIGN_BIT) && (result & SIGN_BIT) != (x & SIGN_BIT);
    return { result, carry, overflow };
  }
};

struct DataProcessing : public Ins {
  static inline const vector<const InstructionDefinition *> definitions = {
    new InstructionDefinition({
      new CondPiece(), new Zeros(3), new IntegralPiece(4, "opcode"), new BoolPiece("S"), new RegPiece("Rn"), new RegPiece("Rd"),
      new IntegralPiece(5, "shift amount"), new IntegralPiece(2, "shift type"), new Zeros(1), new RegPiece("Rm")
    }),
    new InstructionDefinition({
      new CondPiece(), new Zeros(3), new IntegralPiece(4, "opcode"), new BoolPiece("S"), new RegPiece("Rn"), new RegPiece("Rd"),
      new RegPiece("Rs"), new Zeros(1), new IntegralPiece(2, "shift type"), new Ones(1), new RegPiece("Rm")
    }),
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0b001, 3), new IntegralPiece(4, "opcode"), new BoolPiece("S"), new RegPiece("Rn"), new RegPiece("Rd"),
      new RegPiece("rotate"), new IntegralPiece(8, "imm")
    })
  };

  static inline constexpr gword_t MASK_S = flag_mask(20); 
  static inline constexpr gword_t N_FLAG = flag_mask(31);
  static inline constexpr gword_t Z_FLAG = flag_mask(30);
  static inline constexpr gword_t C_FLAG = flag_mask(29);
  static inline constexpr gword_t V_FLAG = flag_mask(28);

  typedef variant<ImmShiftOperand, RegShiftOperand, RotateOperand> Operand;
  
  static inline Operand make_operand(gword_t instruction) {
    Nibbles nibbles(instruction);

    if (nibbles[6] & 0b0010) {
      // imm rotate
      return RotateOperand(instruction);
    } else {
      if (nibbles[1] & 1) {
        // reg shift
        return RegShiftOperand(instruction);
      } else {
        return ImmShiftOperand(instruction);
      }
    }
  }

  enum Opcode : byte {
    AND = 0b0000,
    EOR = 0b0001,
    SUB = 0b0010,
    RSB = 0b0011,
    ADD = 0b0100,
    ADC = 0b0101,
    SBC = 0b0110,
    RSC = 0b0111,
    TST = 0b1000,
    TEQ = 0b1001,
    CMP = 0b1010,
    CMN = 0b1011,
    ORR = 0b1100,
    MOV = 0b1101,
    BIC = 0b1110,
    MVN = 0b1111,
  } opcode;
  
  bool s;
  byte irn, ird;

  Operand operand;

  DataProcessing(gword_t instruction) 
    : Ins(instruction),
      opcode((Opcode) ((instruction >> 21) & 0xF)),
      s(MASK_S & instruction),
      irn(nibbles[4]),
      ird(nibbles[3]),
      operand(make_operand(instruction)) {}

  virtual void execute(CpuState &cpu_state) override {
    ShifterOperandValue operand = std::visit(
      [&](ShifterOperand &op) { return op.evaluate(cpu_state); },
      this->operand
    );

    gword_t op = operand.value;
    gword_t carry = operand.carry;

    gword_t &rd = cpu_state.get_register(ird);
    gword_t rn = cpu_state.get_register(irn);
    gword_t &cpsr = cpu_state.get_cpsr();
    gword_t test;

    gword_t cond_code_mask = Z_FLAG | N_FLAG;
    gword_t overflow = 0;
    
    switch (opcode) {
      case AND:
        rd = rn & op;
        test = rd;
        cond_code_mask |= C_FLAG;
        break;

      case EOR:
        rd = rn ^ op;
        test = rd;
        cond_code_mask |= C_FLAG;
        break;

      case SUB: {
        CheckedResult r0 = CheckedResult::sub(rn, op);
        rd = r0.value;
        test = rd;
        carry = !r0.carry;
        overflow = r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        break;
      }
      
      case RSB: {
        CheckedResult r0 = CheckedResult::sub(op, rn);
        rd = r0.value;
        test = rd;
        carry = !r0.carry;
        overflow = r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        break;
      }
      
      case ADD: {
        CheckedResult r0 = CheckedResult::add(rn, op);
        rd = r0.value;
        test = rd;
        carry = r0.carry;
        overflow = r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        break;
      }
      
      case ADC: {
        CheckedResult r0 = CheckedResult::add(rn, op);
        CheckedResult r1 = CheckedResult::add(r0.value, carry);
        rd = r1.value;
        test = rd;
        carry = r1.carry | r0.carry;
        overflow = r1.overflow | r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        break;
      }
      
      case SBC: {
        CheckedResult r0 = CheckedResult::sub(rn, op);
        CheckedResult r1 = CheckedResult::sub(r0.value, carry ^ 1);
        rd = r1.value;
        test = rd;
        carry = !(r1.carry | r0.carry);
        overflow = r1.overflow | r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        break;
      }
      
      case RSC: {
        CheckedResult r0 = CheckedResult::sub(op, rn);
        CheckedResult r1 = CheckedResult::sub(r0.value, carry ^ 1);
        rd = r1.value;
        test = rd;
        carry = !(r1.carry | r0.carry);
        overflow = r1.overflow | r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        break;
      }
      
      case ORR:
        rd = rn | op;
        test = rd;
        cond_code_mask |= C_FLAG;
        break;
      
      case MOV:
        rd = op;
        test = rd;
        cond_code_mask |= C_FLAG | N_FLAG;
        break;
      
      case BIC:
        rd = rn & ~op;
        cond_code_mask |= N_FLAG | C_FLAG;
        break;
      
      case MVN:
        rd = ~op;
        test = rd;
        cond_code_mask |= C_FLAG | N_FLAG;
        break;

      case TST:
        test = rn & op;
        cond_code_mask |= C_FLAG;
        goto set_flags;
      
      case TEQ:
        test = rn ^ op;
        cond_code_mask |= C_FLAG;
        goto set_flags;
      
      case CMP: {
        CheckedResult r0 = CheckedResult::sub(rn, op);
        test = r0.value;
        carry = !r0.carry;
        overflow = r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        goto set_flags;
      }

      case CMN: {
        CheckedResult r0 = CheckedResult::add(rn, op);
        test = r0.value;
        carry = r0.carry;
        overflow = r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        goto set_flags;
      }
    }

    if (s) {
      if (this->ird == 15) {
        cpsr = cpu_state.get_cpsr();
      } else if (s) {
      set_flags:
        cpsr &= ~cond_code_mask;
        gword_t mask = 
            (test == 0 ? Z_FLAG : 0) 
          | (test & SIGN_BIT ? CpuState::N_FLAG : 0)
          | (overflow ? CpuState::V_FLAG : 0)
          | (carry ? CpuState::C_FLAG : 0);
        cpsr &= mask & cond_code_mask;
      }
    }
  }
};

struct MovStatusToReg : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b00010, 5), new BoolPiece("R"), new Zeros(2), new Ones(4), new RegPiece("Rd"), new Zeros(12)
  });

  bool r;
  byte ird;

  MovStatusToReg(gword_t instruction)
    : Ins(instruction), 
      r(nibbles[5] & 0b0100),
      ird(nibbles[3]) {
  }
};

struct MovRegToStatus : public Ins {
  static inline const InstructionDefinition * definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b00110, 5), new BoolPiece("R"), new ValuePiece(0b10, 2), new RegPiece("mask"), new Ones(4),
    new RegPiece("rotate"), new IntegralPiece(12, "imm", 4)
  });

  bool r;
  byte irm, mask;

  MovRegToStatus(gword_t instruction) 
    : Ins(instruction),
      r(nibbles[5] & 0b0100),
      irm(nibbles[0]),
      mask(nibbles[4]) { }
};

struct BranchExchange : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b00010010, 8), new Ones(12), new ValuePiece(0b0001, 4), new RegPiece("Rm")
  });

  gword_t irm;

  BranchExchange(gword_t instruction) : Ins(instruction), irm(nibbles[0]) { }
};

struct CountLeadingZeros : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b00010110, 8), new Ones(4), new RegPiece("Rd"), new Ones(4), new ValuePiece(0b0001, 4), new RegPiece("Rm")
  });
  
  byte ird, irm;

  CountLeadingZeros(gword_t instruction)
    : Ins(instruction),
      ird(nibbles[3]),
      irm(nibbles[0]) { }
};

struct EnhancedDSPAdditive : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b00010, 5), new IntegralPiece(2, "op"), new Zeros(1), new RegPiece("Rn"), new RegPiece("Rd"), new Zeros(4),
    new ValuePiece(0b0101, 4), new RegPiece("Rm")
  });
 
  byte op, irn, ird, irm;

  EnhancedDSPAdditive(gword_t instruction)
    : Ins(instruction),
      op((nibbles[5] & 0b0110) >> 1),
      irn(nibbles[4]),
      ird(nibbles[3]),
      irm(nibbles[0]) { }
};

struct SWBreak : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b00010010, 8), new IntegralPiece(12, "imm_hi", 4), new ValuePiece(0b0111, 4), new IntegralPiece(4, "imm_lo")
  });

  gshort_t immed;

  SWBreak(gword_t instruction) 
    : Ins(instruction), 
      immed(nibbles[0] | ((instruction >> 8) & ((1 << 12) - 1))) { }
};

struct EnhancedDSPMultiplicative : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b00010, 5), new IntegralPiece(2, "op"), new Zeros(1), new RegPiece("Rd"), new RegPiece("Rn"), new RegPiece("Rs"),
    new Ones(1), new BoolPiece("y"), new BoolPiece("x"), new Zeros(0), new RegPiece("Rm")
  });
  
  bool x, y;
  byte op, ird, irn, irs, irm;

  EnhancedDSPMultiplicative(gword_t instruction)
    : Ins(instruction),
      x(nibbles[1] | 0b0010),
      y(nibbles[1] | 0b0100),
      op((nibbles[5] & 0b0110) >> 1),
      ird(nibbles[4]),
      irn(nibbles[3]),
      irs(nibbles[2]),
      irm(nibbles[0]) { }
};

struct UndefinedInstruction : public Ins {
  gword_t instruction;

  UndefinedInstruction(gword_t instruction) : Ins(instruction) {
    // assert(validate_instruction(UndefinedInstruction::DEFINITIONS, instruction));
  }
};

struct MovImmToStatusReg : public Ins {
  static inline const InstructionDefinition * definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b00110, 5), new BoolPiece("R"), new ValuePiece(0b10, 2), new RegPiece("mask"), new Ones(4),
    new RegPiece("rotate"), new IntegralPiece(12, "imm", 4)
  });

  bool r;
  byte mask;
  RotateOperand rotation;

  MovImmToStatusReg(gword_t instruction)
    : Ins(instruction), r(nibbles[5] & 0b0100), mask(nibbles[4]), rotation(instruction) { }
};

struct LoadStoreOffset : public Ins {
  static inline const vector<const InstructionDefinition *> definitions = {
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0b010, 3), new BoolPiece("P"), new BoolPiece("U"), new BoolPiece("B"), new BoolPiece("W"), new BoolPiece("L"),
      new RegPiece("Rn"), new RegPiece("Rd"), new IntegralPiece(12, "imm", 4)
    }),
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0b011, 3), new BoolPiece("P"), new BoolPiece("U"), new BoolPiece("B"), new BoolPiece("W"), new BoolPiece("L"),
      new RegPiece("Rn"), new RegPiece("Rd"), new IntegralPiece(5, "shift_by"), new IntegralPiece(2, "shift_type"), new Zeros(1), new RegPiece("Rm")
    }),
  };

  variant<gword_t, ImmShiftOperand> operand;

  LoadStoreOffset(gword_t instruction) : Ins(instruction) {

    switch (nibbles[6] & 0b1110) {
      case 0b0100:
        // imm
        operand = 0xFFF & instruction;
        break;

      case 0b0110:
        // reg
        operand = ImmShiftOperand(instruction);
    }
  }
};

struct LoadStoreMultiple : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b100, 3), new BoolPiece("P"), new BoolPiece("U"), new BoolPiece("S"), new BoolPiece("W"), new BoolPiece("L"),
    new RegPiece("Rn"), new IntegralPiece(16, "register_list", 4)
  });

  bool p, u, s, w, l;
  byte irn;
  gshort_t register_list;

  LoadStoreMultiple(gword_t instruction)
    : Ins(instruction), 
      p(nibbles[6] & 1),
      u(nibbles[5] & 0b1000),
      s(nibbles[5] & 0b0100),
      w(nibbles[5] & 0b0010),
      l(nibbles[5] & 0b0001),
      irn(nibbles[4]),
      register_list(instruction & 0xFFFF) { }
};

struct BranchWithLink : public Ins {

  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b101, 3), new BoolPiece("L"), new IntegralPiece(24, "offset")
  });

  bool l;
  gword_t offset;

  BranchWithLink(gword_t instruction) : Ins(instruction), l(nibbles[6] & 1), offset(instruction & 0xFFFFFF) { }
};

struct SoftwareInterrupt : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b1111, 4), new IntegralPiece(24, "swi_number")
  });

  gword_t swi_number;

  SoftwareInterrupt(gword_t instruction) : Ins(instruction), swi_number(instruction & 0xFFFFFF) { }
};

enum Cond : byte {
  EQ = 0b0000,
  NE = 0b0001,
  CSHS = 0b0010,
  CCLO = 0b0011,
  MI = 0b0100,
  PL = 0b0101,
  VS = 0b0110,
  VC = 0b0111,
  HI = 0b0000,
  LS = 0b0001,
  GE = 0b0010,
  LT = 0b0011,
  GT = 0b0100,
  LE = 0b0101,
  AL = 0b0110,
  NV = 0b0111,
};

struct Instruction {
  Cond cond;

  typedef variant<
    MulShort,
    MulLong,
    SingleDataSwap,
    Load,
    DataProcessing,
    MovStatusToReg,
    MovRegToStatus,
    BranchExchange,
    CountLeadingZeros,
    EnhancedDSPAdditive,
    SWBreak,
    EnhancedDSPMultiplicative,
    UndefinedInstruction,
    MovImmToStatusReg,
    LoadStoreOffset,
    LoadStoreMultiple,
    BranchWithLink,
    SoftwareInterrupt
  > InsAlg; 

  InsAlg instruction; 

  Instruction(gword_t instruction) : instruction(UndefinedInstruction(-1)) {
    Nibbles nibbles(instruction);

    cond = (Cond) nibbles[7];

    if (cond == 0b1111) {
      this->instruction = UndefinedInstruction(instruction);
      return;
    }
    
    gword_t masked_opcode = (instruction >> 24) & 0b11001;
    switch (nibbles[6]) {
      case 0b0000:
      case 0b0001: {
        switch (nibbles[1] & 0b1001) {
          case 0b0000:
          case 0b1000:
            if (masked_opcode == 0b10000) {
              // misc ins 3-3
              if (nibbles[1] == 0b0000) {
                if (nibbles[5] & 0b0010) {
                  this->instruction = MovRegToStatus(instruction);
                } else {
                  this->instruction = MovStatusToReg(instruction);
                }
              } else {
                this->instruction = EnhancedDSPMultiplicative(instruction);
              }
            } else {
              this->instruction = DataProcessing(instruction);
            }
            break;

          case 0b0001:
            if (masked_opcode == 0b10000) {
             // misc ins 3-3 
              gword_t op = nibbles[5] << 4;
              op |= nibbles[1];

              switch (op) {
                case 0b00100001:
                  this->instruction = BranchExchange(instruction);
                  break;

                case 0b01100001:
                  this->instruction = CountLeadingZeros(instruction);
                  break;

                case 0b00100011:
                  this->instruction = UndefinedInstruction(instruction);
                  break;

                case 0b00000101:
                case 0b00100101:
                case 0b01000101:
                case 0b01100101:
                  this->instruction = EnhancedDSPAdditive(instruction);
                  break;

                case 0b00100111:
                  this->instruction = SWBreak(instruction);
                  break;
              }
            } else {
              this->instruction = DataProcessing(instruction);
            }
          case 0b1001:
            // misc ins 2-2
            if (nibbles[1] == 0b1001) {
              // mul / mullong / single swap
              if (nibbles[6] & 0b1) {
                this->instruction = SingleDataSwap(instruction);
              } else {
                if (nibbles[5] & 0b1000) {
                  this->instruction = MulShort(instruction);
                } else {
                  this->instruction = MulLong(instruction);
                }
              }
            } else {
              this->instruction = Load(instruction);
            }
            break;
        }
        break;
      }
      
      case 0b0010:
      case 0b0011: {
        gword_t opcode = (instruction >> 20) & 0b11011;
        switch (opcode) {
          case 0b10000:
            this->instruction = UndefinedInstruction(instruction);
            break;
          case 0b10010:
            this->instruction = MovImmToStatusReg(instruction);
            break;
          default:
            this->instruction = DataProcessing(instruction);
        }
        break;
      }

      case 0b0100:
      case 0b0101:
        this->instruction = LoadStoreOffset(instruction);
        break;

      case 0b0110:
      case 0b0111:
        if (nibbles[1] & 1) {
          this->instruction = UndefinedInstruction(instruction);
        } else {
          this->instruction = LoadStoreOffset(instruction);
        }
        break;

      case 0b1000:
      case 0b1001:
        this->instruction = LoadStoreMultiple(instruction);
        break;

      case 0b1010:
      case 0b1011:
        this->instruction = BranchWithLink(instruction);
        break;

      // Coprocessor instructions
      case 0b1100:
      case 0b1101:
      case 0b1110:
        break;

      case 0b1111:
        this->instruction = SoftwareInterrupt(instruction);
        break;
    }
  }

  void execute(CpuState &cpu_state) {
    std::visit([&](Ins &ins) {
      ins.execute(cpu_state);
    }, instruction);
  }
};

void initialize_definition_map() {
  InstructionDefinition::DEFINITION_MAP = {
    {"MulShort", vector({MulShort::definition})},
    {"MulLong", vector({MulLong::definition})},
    {"SingleDataSwap", vector({SingleDataSwap::definition})},
    {"Load", vector(Load::definitions)},
    {"DataProcessing", vector(DataProcessing::definitions)},
    {"MovStatusToReg", vector({MovStatusToReg::definition})},
    {"MovRegToStatus", vector({MovRegToStatus::definition})},
    {"BranchExchange", vector({BranchExchange::definition})},
    {"CondLeadingZeros", vector({CountLeadingZeros::definition})},
    {"EnhancedDSPAdditive", vector({EnhancedDSPAdditive::definition})},
    {"SWBreak", vector({SWBreak::definition})},
    {"EnhancedDSPMultiplicative", vector({EnhancedDSPMultiplicative::definition})},
    {"MovImmToStatusReg", vector({MovImmToStatusReg::definition})},
    {"LoadStoreOffset", vector(LoadStoreOffset::definitions)},
    {"LoadStoreMultiple", vector({LoadStoreMultiple::definition})},
    {"BranchWithLink", vector({BranchWithLink::definition})},
    {"SoftwareInterrupt", vector({SoftwareInterrupt::definition})}
  };
}

}
#endif
