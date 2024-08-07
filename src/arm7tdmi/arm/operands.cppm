module;
#include <algorithm>
#include <iostream>

export module arm7tdmi.arm.operands;

import arm7tdmi.instruction;

export {
  ;

  enum BitShift : u8 { LEFT = 0b00, LRIGHT = 0b01, ARIGHT = 0b10, ROR = 0b11 };

  struct ShifterOperandValue {
    u32 value;
    u32 carry;
  };

  struct ShifterOperand {
    virtual ShifterOperandValue evaluate(CpuState &state) = 0;
  };

  struct RotateOperand : public ShifterOperand {
    u8 rotate, imm;

    RotateOperand(u8 rotate, u8 imm) : rotate(rotate & 0xF), imm(imm & 0xFF) {}

    RotateOperand(u32 instruction) {
      Nibbles nibbles(instruction);

      rotate = nibbles[2];
      imm = instruction & 0xFF;
    }

    ShifterOperandValue evaluate(CpuState &state) override {
      constexpr u32 MASK = flag_mask(31);

      u32 shifter = ror<u32>(imm, rotate * 2);
      u32 carry =
          rotate ? bool(shifter & MASK) : state.get_flag(CpuState::C_FLAG);
      return {shifter, carry};
    }
  };

  struct ThumbOperand : public ShifterOperand {
    u8 irm;

    ThumbOperand(u8 irm) : irm(irm) {}

    ShifterOperandValue evaluate(CpuState &state) override {
      return {state.read_register(irm), 0};
    }
  };

  struct ImmShiftOperand : public ShifterOperand {
    u8 shift_by, irm;
    BitShift shift_type;

    ImmShiftOperand(u8 shift_by, u8 irm, BitShift shift_type)
        : shift_by(shift_by), irm(irm), shift_type(shift_type) {}

    ImmShiftOperand(u32 instruction) {
      Nibbles nibbles(instruction);

      shift_by = std::min<u8>(31, (u8)((instruction & 0xF80) >> 7));
      irm = nibbles[0];
      shift_type = (BitShift)((nibbles[1] & 0b0110) >> 1);
    }

    ShifterOperandValue evaluate(CpuState &state) override {
      constexpr u32 MASK = flag_mask(31);
      u32 reg = state.read_register(irm);

      if (irm == 0xF)
        reg += 8;

      u32 value, carry;
      if (shift_by) {
        carry = bool(flag_mask(shift_by - 1) & reg);

        switch (shift_type) {
        case LEFT:
          value = reg << shift_by;
          carry = bool(flag_mask(32 - shift_by) & reg);
          break;

        case LRIGHT:
          value = lsr<u32>(reg, shift_by);
          break;

        case ARIGHT:
          value = asr<i32>(reg, shift_by);
          break;

        case ROR:
          value = ror<u32>(reg, shift_by);
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

      return {value, carry};
    }
  };

  struct RegShiftOperand : public ShifterOperand {
    u8 irs, irm;
    BitShift shift_type;

    RegShiftOperand(u8 irs, u8 irm, BitShift shift_type)
        : irs(irs), irm(irm), shift_type(shift_type) {}

    RegShiftOperand(u32 instruction) {
      Nibbles nibbles(instruction);

      irs = nibbles[2];
      irm = nibbles[0];
      shift_type = (BitShift)((nibbles[1] & 0b0110) >> 1);
    }

    ShifterOperandValue evaluate(CpuState &state) override {
      u32 rs = state.read_register(irs);
      u32 rm = state.read_register(irm);

      u32 shift = rs & 0xFF;

      u32 value, carry;

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
          value = lsr<u32>(rm, shift);
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
          value = asr<i32>(rm, shift);
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
        u32 rotation = rs & 0x1F;
        if (shift == 0) {
          value = rm;
          carry = state.get_flag(CpuState::C_FLAG);
        } else if (rotation == 0) {
          value = rm;
          carry = flag_mask(31) & rm;
        } else {
          value = ror<u32>(rm, rotation);
          carry = flag_mask(rotation - 1) & rm;
        }
        break;
      }
      }

      return {value, bool(carry)};
    }
  };
}
