module;
#include <algorithm>
#include <iostream>

export module arm7tdmi.arm.operands;

import arm7tdmi.instruction;

export {
  ;

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
  byte rotate, imm;

  RotateOperand(byte rotate, byte imm) : rotate(rotate & 0xF), imm(imm & 0xFF) { }

  RotateOperand(gword_t instruction) {
    Nibbles nibbles(instruction);
    
    rotate = nibbles[2];
    imm = instruction & 0xFF;
  }

  ShifterOperandValue evaluate(CpuState &state) override {
    constexpr gword_t MASK = flag_mask(31);

    gword_t shifter = ror<gword_t>(imm, rotate * 2);
    gword_t carry = rotate ? bool(shifter & MASK) : state.get_flag(CpuState::C_FLAG);
    return { shifter, carry };
  }
};

struct ThumbOperand : public ShifterOperand {
  byte irm;

  ThumbOperand(byte irm) : irm(irm) { }

  ShifterOperandValue evaluate(CpuState &state) override {
    return { state.read_register(irm), 0 };
  }
};

struct ImmShiftOperand : public ShifterOperand {
  byte shift_by, irm;
  BitShift shift_type;
  
  ImmShiftOperand(byte shift_by, byte irm, BitShift shift_type)
    : shift_by(shift_by),
      irm(irm),
      shift_type(shift_type) { }

  ImmShiftOperand(gword_t instruction) {
    Nibbles nibbles(instruction);

    shift_by = std::min<byte>(31, (byte) ((instruction & 0xF80) >> 7));
    irm = nibbles[0];
    shift_type = (BitShift) ((nibbles[1] & 0b0110) >> 1);
  }

  ShifterOperandValue evaluate(CpuState &state) override {
    constexpr gword_t MASK = flag_mask(31);
    gword_t reg = state.read_register(irm);

    if (irm == 0xF)
      reg += 8;
    
    gword_t value, carry;
    if (shift_by) {
      carry = bool(flag_mask(shift_by - 1) & reg);
      
      switch (shift_type) {
        case LEFT:
          value = reg << shift_by;
          carry = bool(flag_mask(32 - shift_by) & reg);
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
          carry = bool(MASK & reg);
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

  RegShiftOperand(byte irs, byte irm, BitShift shift_type) 
    : irs(irs),
      irm(irm),
      shift_type(shift_type) {}

  RegShiftOperand(gword_t instruction) {
    Nibbles nibbles(instruction);

    irs = nibbles[2];
    irm = nibbles[0];
    shift_type = (BitShift) ((nibbles[1] & 0b0110) >> 1);
  }
  
  ShifterOperandValue evaluate(CpuState &state) override {
    gword_t rs = state.read_register(irs);
    gword_t rm = state.read_register(irm);

    gword_t shift = rs & 0xFF;
    
    gword_t value, carry;

    switch (shift_type) {
      case LEFT:
        if (shift == 0) {
          value = rm;
          carry = state.get_flag(CpuState::C_FLAG);
        } else if (shift < 32) {
          value = rm << shift;
          carry = flag_mask(32 - shift) & rm;
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
          carry = state.get_flag(CpuState::C_FLAG);
        } else if (shift < 32) {
          value = lsr<gword_t>(rm, shift);
          carry = flag_mask(shift - 1) & rm;
        } else if (shift == 32) {
          value = 0;
          carry = flag_mask(31) & rm;
        } else {
          value = 0;
          carry = 0;
        }
        break;

      case ARIGHT:
        if (shift == 0) {
          value = rm;
          carry = state.get_flag(CpuState::C_FLAG);
        } else if (shift < 32) {
          value = asr<signed_gword_t>(rm, shift);
          carry = flag_mask(shift - 1) & rm;
        } else {
          carry = 0x80000000 & rm;
          if (carry) {
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
          carry = state.get_flag(CpuState::C_FLAG);
        } else if (rotation == 0) {
          value = rm;
          carry = flag_mask(31) & rm;
        } else {
          value = ror<gword_t>(rm, rotation);
          carry = flag_mask(rotation - 1) & rm;
        }
        break;
      }
    }

    return { value, bool(carry) };
  }
};

}
