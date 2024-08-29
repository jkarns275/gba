module;
#include <format>
#include <string>
#include <variant>
#include <vector>

#include <spdlog/spdlog.h>

export module arm7tdmi.arm:data_processing;

import arm7tdmi;
import arm7tdmi.instruction;
import :operands;

using std::variant;
using std::vector;

static constexpr u32 GWORD_T_SIGN_BIT = ror<u32>(1, 1);

export {
  ;

  struct CheckedResult {
    u32 value, carry, overflow;

    // TODO: This can probably be made more efficient.
    static CheckedResult add(u32 x, u32 y) {
      u32 result = x + y;
      u32 carry = result < x;
      u32 overflow = (x & GWORD_T_SIGN_BIT) == (y & GWORD_T_SIGN_BIT) &&
                     (result & GWORD_T_SIGN_BIT) != (x & GWORD_T_SIGN_BIT);
      return {result, carry, overflow};
    }

    static CheckedResult sub(u32 x, u32 y) {
      u32 result = x - y;
      u32 carry = result > x;
      u32 overflow = (x & GWORD_T_SIGN_BIT) != (y & GWORD_T_SIGN_BIT) &&
                     (result & GWORD_T_SIGN_BIT) != (x & GWORD_T_SIGN_BIT);
      return {result, carry, overflow};
    }
  };
  enum Opcode : u8 {
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
  };

  constexpr std::string OPCODE_TO_STRING[] = {
      "AND", "EOR", "SUB", "RSB", "ADD", "ADC", "SBC", "RSC",
      "TST", "TEQ", "CMP", "CMN", "ORR", "MOV", "BIC", "MVN",
  };

  std::string opcode_to_string(Opcode opcode) {
    return OPCODE_TO_STRING[(int)opcode];
  }

  // ARM ADC
  // ARM ADD
  // ARM AND
  // ARM BIC
  // ARM CMN
  // ARM CMP
  // ARM EOR
  // ARM MLA
  // ARM MOV
  // ARM MVN
  // ARM ORR
  // ARM RSB
  // ARM RSC
  // ARM SBC
  // ARM TEQ
  // ARM TST
  struct DataProcessing : public Ins {
    static inline const vector<const InstructionDefinition *> definitions = {
        new InstructionDefinition(
            {new CondPiece(), new Zeros(3), new IntegralPiece(4, "opcode"),
             new BoolPiece("S"), new RegPiece("Rn"), new RegPiece("Rd"),
             new IntegralPiece(5, "shift amount"),
             new IntegralPiece(2, "shift type"), new Zeros(1),
             new RegPiece("Rm")}),
        new InstructionDefinition(
            {new CondPiece(), new Zeros(3), new IntegralPiece(4, "opcode"),
             new BoolPiece("S"), new RegPiece("Rn"), new RegPiece("Rd"),
             new RegPiece("Rs"), new Zeros(1),
             new IntegralPiece(2, "shift type"), new Ones(1),
             new RegPiece("Rm")}),
        new InstructionDefinition({new CondPiece(), new ValuePiece(0b001, 3),
                                   new IntegralPiece(4, "opcode"),
                                   new BoolPiece("S"), new RegPiece("Rn"),
                                   new RegPiece("Rd"), new RegPiece("rotate"),
                                   new IntegralPiece(8, "imm")})};

    static inline constexpr u32 N_FLAG = flag_mask(31);
    static inline constexpr u32 Z_FLAG = flag_mask(30);
    static inline constexpr u32 C_FLAG = flag_mask(29);
    static inline constexpr u32 V_FLAG = flag_mask(28);
    static inline constexpr u32 S_FLAG = flag_mask(20);

    // TODO: Probably make this class templated instead of doing whatever this
    // is.
    typedef variant<ImmShiftOperand, RegShiftOperand, RotateOperand,
                    ThumbOperand>
        Operand;

    static inline Operand make_operand(u32 instruction) {
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

    Opcode opcode;

    bool s;
    u8 irn, ird;

    Operand operand;

    DataProcessing(u32 instruction)
        : Ins(instruction), opcode((Opcode)((instruction >> 21) & 0xF)),
          s(S_FLAG & instruction), irn(nibbles[4]), ird(nibbles[3]),
          operand(make_operand(instruction)) {}

    DataProcessing(u32 instruction, Opcode opcode, bool s, u8 irn, u8 ird,
                   Operand operand)
        : Ins(instruction), opcode(opcode), s(s), irn(irn), ird(ird),
          operand(operand) {}

    void execute(CpuState &state) override {
      ShifterOperandValue operand =
          std::visit([&](ShifterOperand &op) { return op.evaluate(state); },
                     this->operand);

      u32 op = operand.value;
      u32 carry = operand.carry;
      u32 carry_flag = bool(state.get_flag(CpuState::C_FLAG));

      u32 rd;
      u32 rn = state.read_register(irn);
      u32 cpsr = state.read_cpsr();

      u32 cond_code_mask = Z_FLAG | N_FLAG;
      u32 overflow = 0;

      switch (opcode) {
      case AND:
        rd = rn & op;
        cond_code_mask |= C_FLAG;
        break;

      case EOR:
        rd = rn ^ op;
        cond_code_mask |= C_FLAG;
        break;

      case SUB: {
        CheckedResult r0 = CheckedResult::sub(rn, op);
        rd = r0.value;
        carry = !r0.carry;
        overflow = r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        break;
      }

      case RSB: {
        CheckedResult r0 = CheckedResult::sub(op, rn);
        rd = r0.value;
        carry = !r0.carry;
        overflow = r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        break;
      }

      case ADD: {
        CheckedResult r0 = CheckedResult::add(rn, op);
        rd = r0.value;
        carry = r0.carry;
        overflow = r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        break;
      }

      case ADC: {
        CheckedResult r0 = CheckedResult::add(rn, op);
        CheckedResult r1 = CheckedResult::add(r0.value, carry_flag);
        rd = r1.value;
        carry = r1.carry | r0.carry;
        overflow = r1.overflow | r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        break;
      }

      case SBC: {
        CheckedResult r0 = CheckedResult::sub(rn, op);
        CheckedResult r1 = CheckedResult::sub(r0.value, carry_flag ^ 1);
        rd = r1.value;
        carry = !(r1.carry | r0.carry);
        overflow = r1.overflow | r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        break;
      }

      case RSC: {
        CheckedResult r0 = CheckedResult::sub(op, rn);
        CheckedResult r1 = CheckedResult::sub(r0.value, carry_flag == 0);
        rd = r1.value;
        carry = !(r1.carry | r0.carry);
        overflow = r1.overflow | r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        break;
      }

      case ORR:
        rd = rn | op;
        cond_code_mask |= C_FLAG;
        break;

      case MOV:
        rd = op;
        cond_code_mask |= C_FLAG;
        break;

      case BIC:
        rd = rn & ~op;
        cond_code_mask |= C_FLAG;
        break;

      case MVN:
        rd = ~op;
        cond_code_mask |= C_FLAG;
        break;

      case TST:
        rd = rn & op;
        cond_code_mask |= C_FLAG;
        goto set_flags;

      case TEQ:
        rd = rn ^ op;
        cond_code_mask |= C_FLAG;
        goto set_flags;

      case CMP: {
        CheckedResult r0 = CheckedResult::sub(rn, op);
        rd = r0.value;
        carry = !r0.carry;
        overflow = r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        goto set_flags;
      }

      case CMN: {
        CheckedResult r0 = CheckedResult::add(rn, op);
        rd = r0.value;
        carry = r0.carry;
        overflow = r0.overflow;
        cond_code_mask |= C_FLAG | V_FLAG;
        goto set_flags;
      }
      }

      state.write_register(ird, rd);

      if (s) {
        if (this->ird == 15) {
          cpsr = state.read_spsr();
        } else {
        set_flags:
          cpsr &= ~cond_code_mask;
          u32 mask = (rd == 0 ? Z_FLAG : 0) |
                     (rd & GWORD_T_SIGN_BIT ? N_FLAG : 0) |
                     (overflow ? V_FLAG : 0) | (carry ? C_FLAG : 0);
          cpsr |= mask & cond_code_mask;
        }

        state.write_cpsr(cpsr);
      }
    }

    std::string disassemble() override {
      std::string operand_str = std::visit(
          [](ShifterOperand &op) { return op.disassemble(); }, this->operand);

      switch (opcode) {
      case CMN:
      case CMP:
      case TEQ:
      case TST:
        return std::format("{}{} {}, {}", opcode_to_string(opcode),
                           cond_to_string(cond), pretty_reg_name(irn),
                           operand_str);
      default:
        return std::format("{}{}{} {}, {}, {}", opcode_to_string(opcode),
                           cond_to_string(cond), s ? "S" : "",
                           pretty_reg_name(ird), pretty_reg_name(irn),
                           operand_str);
      }
    }
  };
}
