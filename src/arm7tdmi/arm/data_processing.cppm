module;
#include <variant>
#include <vector>
#include <iostream>

export module arm7tdmi.arm.data_processing;

import arm7tdmi.instruction;
import arm7tdmi.arm.operands;

using std::variant;
using std::vector;

export {
  ;

struct CheckedResult {
  gword_t value, carry, overflow;

  static CheckedResult add(gword_t x, gword_t y) {
    gword_t result = x + y;
    gword_t carry = result < x;
    gword_t overflow = (x & GWORD_T_SIGN_BIT) == (y & GWORD_T_SIGN_BIT) && (result & GWORD_T_SIGN_BIT) != (x & GWORD_T_SIGN_BIT);
    return { result, carry, overflow };
  }

  static CheckedResult sub(gword_t x, gword_t y) {
    gword_t result = x - y;
    gword_t carry = result > x;
    gword_t overflow = (x & GWORD_T_SIGN_BIT) != (y & GWORD_T_SIGN_BIT) && (result & GWORD_T_SIGN_BIT) != (x & GWORD_T_SIGN_BIT);
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

  static inline constexpr gword_t N_FLAG = flag_mask(31);
  static inline constexpr gword_t Z_FLAG = flag_mask(30);
  static inline constexpr gword_t C_FLAG = flag_mask(29);
  static inline constexpr gword_t V_FLAG = flag_mask(28);
  static inline constexpr gword_t S_FLAG = flag_mask(20); 

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
      s(S_FLAG & instruction),
      irn(nibbles[4]),
      ird(nibbles[3]),
      operand(make_operand(instruction)) {}

  virtual void execute(CpuState &cpu_state) override {
    ShifterOperandValue operand = std::visit(
      [&](ShifterOperand &op) { return op.evaluate(cpu_state); },
      this->operand
    );

    gword_t op = operand.value;
    gword_t carry = cpu_state.get_flag(CpuState::C_FLAG);

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
        CheckedResult r1 = CheckedResult::sub(r0.value, carry == 0);
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
        carry = operand.carry;
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
          | (test & GWORD_T_SIGN_BIT ? N_FLAG : 0)
          | (overflow ? V_FLAG : 0)
          | (carry ? C_FLAG : 0);
        cpsr |= mask & cond_code_mask;
      }
    }
  }
};

}
