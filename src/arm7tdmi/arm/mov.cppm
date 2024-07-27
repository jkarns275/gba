module;
#include <utility>
#include <variant>
#include <vector>
#include <iostream>
#include <assert.h>

export module arm7tdmi.arm.mov;

import arm7tdmi.instruction;
import arm7tdmi.arm.operands;

using std::variant;
using std::vector;

export {
  ;

// TODO: Merge `Load` and `LoadStoreOffset` probably.
struct LoadStore : public Ins {
  static inline const vector<const InstructionDefinition *> definitions = {
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0, 3), new BoolPiece("P"), new BoolPiece("U"), new BoolPiece("I"), new BoolPiece("W"), new BoolPiece("L"),
      new RegPiece("Rn"), new RegPiece("Rd"), new Zeros(4), new Ones(1), new BoolPiece("S"), new BoolPiece("H"), new Ones(1), new RegPiece("Rm")
    }),
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0, 3), new BoolPiece("P"), new BoolPiece("U"), new BoolPiece("I"), new BoolPiece("W"), new BoolPiece("L"),
      new RegPiece("Rn"), new RegPiece("Rd"), new RegPiece("immed_hi"), new Ones(1), new BoolPiece("S"), new BoolPiece("H"), new Ones(1), new RegPiece("immed_lo")
    }),
  };

  static constexpr gword_t MASK_P = flag_mask(24);
  static constexpr gword_t MASK_U = flag_mask(23);
  static constexpr gword_t MASK_B = flag_mask(22);
  static constexpr gword_t MASK_W = flag_mask(21);
  static constexpr gword_t MASK_L = flag_mask(20);
  static constexpr gword_t MASK_S = flag_mask(6);
  static constexpr gword_t MASK_H = flag_mask(5);
  
  // If p == 0: post-indexed addressing is used
  // If P == 1: offset addressing or pre-indexed addressing is used, controled by w.
  bool p;
  
  // Indicates whether the offset is added to the base or subtracted.
  bool u;
  
  // if p == 1 and w == 1, the memory address is written back to the base register
  bool w;

  // distinguishes between a load (1) and a store (0)
  bool l;

  // Signed (1) and unsigned (0) access
  bool s;

  byte irn, ird;

  enum IntegralType { BYTE = 1, SHORT = 2, WORD = 4, LONG = 8 } integral_type;
  enum OffsetType { REGISTER = 0, IMMEDIATE = 1 } offset_type;
  
  byte operand;

  LoadStore(gword_t instruction) 
    : Ins(instruction),
      p(MASK_P & instruction),
      u(MASK_U & instruction),
      w(MASK_W & instruction),
      l(MASK_L & instruction),
      s(MASK_S & instruction),
      irn(nibbles[4]),
      ird(nibbles[3]) {

    bool b = MASK_B & instruction;
 
    // Halfword (1) and byte (0) access
    // bool h = MASK_H & instruction;
    
    offset_type = (OffsetType) b;

    switch (offset_type) {
      case REGISTER:
        operand = nibbles[0];
        break;
      case IMMEDIATE:
        operand = nibbles[0] | (nibbles[2] << 4);
        break;
    }

    if (instruction & MASK_H) {
      integral_type = SHORT;
    } else {
      integral_type = BYTE;
    }

    assert(!(integral_type == BYTE && !s));
  }

  LoadStore(gword_t instruction, bool p, bool u, bool w, bool l, bool s, byte irn, byte ird, 
            IntegralType integral_type, OffsetType offset_type, byte operand)
    : Ins(instruction),
      p(p), u(u), w(w), l(l), s(s),
      irn(irn), ird(ird),
      integral_type(integral_type), offset_type(offset_type), operand(operand) {}


  static constexpr gword_t switch_pair(OffsetType offset_type, gword_t p) {
    return (gword_t) offset_type | (p << 1);
  }

  gword_t get_address(CpuState &state) {
    const gword_t sign = u ? 1 : -1;

    gword_t &rn = state.get_register(irn), 
            address;

    switch (switch_pair(offset_type, p)) {
      case switch_pair(REGISTER, false):
        address = rn;
        rn += sign * state.get_register(operand);
        break;
      
      case switch_pair(REGISTER, true):
        address = rn + (sign * state.get_register(operand));
        rn = w ? address : rn;
        break;

      case switch_pair(IMMEDIATE, false):
        address = rn;
        rn += sign * (gword_t) operand;
        break;

      case switch_pair(IMMEDIATE, true):
        address = rn + (sign * (gword_t) operand);
        rn = w ? address : rn;
    }

    return address;
  }

  static constexpr gword_t switch_pair(IntegralType integral_type, bool is_signed) {
    return (gword_t) integral_type | ((gword_t) is_signed * 16);
  }

  void load(CpuState &state) {
    gword_t &rd = state.get_register(ird);
    gword_t address = get_address(state);

    switch (switch_pair(integral_type, s)) {
      case switch_pair(BYTE, true): {
        signed_gword_t word = state.signed_byte_at(address);
        rd = word;
        break;
      }
      
      case switch_pair(SHORT, false): {
        gword_t word = state.short_at(address);
        rd = word;
        break;
      }

      case switch_pair(SHORT, true): {
        signed_gword_t word = state.signed_short_at(address);
        rd = word;
        break;
      }

      case switch_pair(WORD, false):
      case switch_pair(WORD, true):
        rd = state.at(address);
        break;

      // Not implemented since these isntructions dont exist in arm7tdmi
      case switch_pair(LONG, false):
      case switch_pair(LONG, true):
      // This should not happen - when H and S flags are 0 it is actually a multiply instruction.
      case switch_pair(BYTE, false):
        __builtin_unreachable();
    }

  }

  void store(CpuState &state) {
    gword_t rd = state.get_register(ird);
    gword_t address = get_address(state);

    switch (switch_pair(integral_type, s)) {
      case switch_pair(SHORT, false): {
        gshort_t &data = state.short_at(address);
        data = rd;
        break;
      }
      // Storing a word / byte / signed short are all covered in other instructions.
      case switch_pair(WORD, false):
      case switch_pair(SHORT, true):
      case switch_pair(WORD, true):
      case switch_pair(LONG, false):
      case switch_pair(LONG, true):
      case switch_pair(BYTE, false):
      case switch_pair(BYTE, true):
        __builtin_unreachable();
        break;
    }
  }

  void execute(CpuState &state) override {
    if (l)
      load(state);
    else
      store(state);
  }
};

struct MovStatusToReg : public Ins {
  static inline const InstructionDefinition * definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b00010, 5), new BoolPiece("R"), new Zeros(2), new Ones(4), new RegPiece("Rd"), new Zeros(12)
  });
  static constexpr gword_t MASK_R = flag_mask(22);

  bool r;
  byte ird;  

  MovStatusToReg(gword_t instruction)
    : Ins(instruction),
      r(MASK_R & instruction),
      ird(nibbles[3]) { }

  void execute(CpuState &state) override {
    gword_t &rd = state.get_register(ird);
    if (r) {
      rd = state.get_spsr();
    } else {
      rd = state.get_cpsr();
    }
  }
};

struct MovToStatus : public Ins {
  static inline vector<const InstructionDefinition *> definition = {
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0b00010, 5), new BoolPiece("R"), new ValuePiece(0b10, 2), new RegPiece("mask"), new Ones(4),
      new Zeros(8), new IntegralPiece(12, "rm", 4)
    }),
    new InstructionDefinition({
      new CondPiece(), new ValuePiece(0b00110, 5), new BoolPiece("R"), new ValuePiece(0b10, 2), new RegPiece("mask"), new Ones(4),
      new RegPiece("rotate"), new IntegralPiece(12, "imm", 4)
    })
  };

  bool r;
  variant<byte, RotateOperand> operand;
  byte field_mask;

  MovToStatus(gword_t instruction) 
    : Ins(instruction),
      r(nibbles[5] & 0b0100),
      field_mask(nibbles[4]) {
    switch (nibbles[6]) {
      case 0b0011:
        operand = RotateOperand(instruction);
        break;

      case 0b0001:
        operand = nibbles[0];
        break;

      default:
        __builtin_unreachable();
    }
  }

  void execute(CpuState &state) override {
    gword_t operand;

    if (std::holds_alternative<byte>(this->operand)) {
      operand = state.get_register(std::get<byte>(this->operand));
    } else {
      ShifterOperandValue value = std::get<RotateOperand>(this->operand).evaluate(state);
      operand = value.value;
    }

    Mode mode = state.get_mode();
    if (!r && !mode_has_spsr(mode))
      return;

    gword_t &psr = r ? state.get_cpsr() : state.get_spsr();
    gword_t mask = 0;

    if ((r && mode_is_privileged(mode)) || !r) {
      mask |= field_mask & 0b0001 ? 0x000000FF : 0;
      mask |= field_mask & 0b0010 ? 0x0000FF00 : 0;
      mask |= field_mask & 0b0100 ? 0x00FF0000 : 0;
    }
    
    mask |= field_mask & 0b1000 ? 0xFF000000 : 0;

    psr = (mask & operand) | (~mask & psr);
  }
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
  
  static constexpr gword_t MASK_I = flag_mask(25);
  static constexpr gword_t MASK_P = flag_mask(24);
  static constexpr gword_t MASK_U = flag_mask(23);
  static constexpr gword_t MASK_B = flag_mask(22);
  static constexpr gword_t MASK_W = flag_mask(21);
  static constexpr gword_t MASK_L = flag_mask(20);
  static constexpr gword_t MASK_S = flag_mask(6);
  static constexpr gword_t MASK_H = flag_mask(5);
 
  bool register_offset;
  bool pre_indexed_or_offset;
  bool add_offset; 
  bool w;
  bool load;

  byte irn, ird;

  variant<gword_t, ImmShiftOperand> operand;
  enum DataType { BYTE = 1, WORD = 0} data_type;

  LoadStoreOffset(gword_t instruction) 
    : Ins(instruction), 
      register_offset(instruction & MASK_I),
      pre_indexed_or_offset(instruction & MASK_P),
      add_offset(instruction & MASK_U),
      w(instruction & MASK_W),
      load(instruction & MASK_L),
      irn(nibbles[4]),
      ird(nibbles[3]),
      data_type(instruction & MASK_B ? BYTE : WORD) {
    if (register_offset) {
      operand = ImmShiftOperand(instruction);
    } else {
      operand = 0xFFF & instruction;
    }
  }

  LoadStoreOffset(gword_t instruction, bool i, bool p, bool u, bool w, bool l, byte irn, byte ird, DataType data_type, variant<gword_t, ImmShiftOperand> operand)
    : Ins(instruction),
      register_offset(i), pre_indexed_or_offset(p), add_offset(u), w(w), load(l), irn(irn), ird(ird), data_type(data_type), operand(operand) {}

  static constexpr gword_t switch_pair(bool p, bool w) {
    return (p ? 1 : 0) | (w ? 2 : 0);
  }

  gword_t get_address(CpuState &state) {
    gword_t &rn = state.get_register(irn);
    signed_gword_t sign = add_offset ? 1 : -1;
    gword_t addr = rn;
    
    if (irn == 15) {
      // Definition of PC is slightly different in THUMB mode
      if (state.get_flag(CpuState::T_FLAG)) {
        addr &= 0xFFFFFFFC;
        addr <<= 2;
      } else {
        addr += 8;   
      }
    }
    
    signed_gword_t offset = register_offset ? std::get<ImmShiftOperand>(operand).evaluate(state).value : std::get<gword_t>(operand);
    
    switch (switch_pair(pre_indexed_or_offset, w)) {
      case switch_pair(false, false):
      case switch_pair(false, true):
        rn = addr + sign * offset;
        break;
      
      case switch_pair(true,  false):
        addr += sign * offset;
        break;

      case switch_pair(true,  true):
        addr += sign * offset;
        rn = addr;
        break;
    }

    return addr;
  }

  void execute(CpuState &state) override {
    gword_t addr = get_address(state);
    gword_t &rd = state.get_register(ird);
    
    if (load) {
      Mode mode = Mode::USR;

      if (pre_indexed_or_offset || !w)
        mode = state.get_mode();

      switch (data_type) {
        case BYTE:
          rd = (gword_t) state.memory.byte_at(addr, mode);
          break;
        case WORD:
          rd = state.memory.rotated_at(addr, mode);
          break;
      }
    } else {
      switch (data_type) {
        case BYTE:
          state.byte_at(addr) = rd;
          break;
        case WORD:
          state.at(addr & ~(0b11)) = rd;
          break;
      }
    }
    
  }
};

struct LoadStoreMultiple : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b100, 3), new BoolPiece("P"), new BoolPiece("U"), new BoolPiece("S"), new BoolPiece("W"), new BoolPiece("L"),
    new RegPiece("Rn"), new IntegralPiece(16, "register_list", 4)
  });

  static constexpr gword_t MASK_PC = flag_mask(15);
  static constexpr gword_t MASK_PC_ASSIGNMENT = 0xFFFFFFFE;
  static constexpr gword_t MASK_P = flag_mask(24);
  static constexpr gword_t MASK_U = flag_mask(23);
  static constexpr gword_t MASK_S = flag_mask(22);
  static constexpr gword_t MASK_W = flag_mask(21);
  static constexpr gword_t MASK_L = flag_mask(20);

  bool p, u, s, w, l;
  byte irn;
  gshort_t register_list;

  LoadStoreMultiple(gword_t instruction)
    : Ins(instruction), 
      p(instruction & MASK_P),
      u(instruction & MASK_U),
      s(instruction & MASK_S),
      w(instruction & MASK_W),
      l(instruction & MASK_L),
      irn(nibbles[4]),
      register_list(instruction & 0xFFFF) { }

  LoadStoreMultiple(gword_t instruction, bool p, bool u, bool s, bool w, bool l, byte irn, gshort_t register_list)
    : Ins(instruction), p(p), u(u), s(s), w(w), l(l), irn(irn), register_list(register_list) {}

  inline std::pair<gword_t, gword_t> get_address(CpuState &state) {
    gword_t rn = state.get_register(irn);
    gword_t register_width = count_ones(register_list) * 4;
    gword_t start = rn;

    start += (p == u) << 2;
    start -= u ? 0 : register_width;
    rn += u ? register_width : -register_width;

    return {start, rn};
  }

  void execute(CpuState &state) override {
    auto [address, rn_new] = get_address(state);

    if (l) {
      Mode mode = s && !(register_list & MASK_PC) ? Mode::USR : state.get_mode();

      for (int i = 0; i < 15; i++) {
        if ((1 << i) & register_list) {
          state.get_register(i, mode) = state.at(address);
          address += 4;
        }
      }

      if (register_list & MASK_PC) {
        if (s)
          state.get_cpsr() = state.get_spsr();

        state.get_pc() = state.at(address) & MASK_PC_ASSIGNMENT;
        address += 4;
      }

    } else {
      Mode mode = s ? Mode::USR : state.get_mode();

      for (int i = 0; i < 16; i++) {
        if ((1 << i) & register_list) {
          state.at(address) = state.get_register(i, mode);
          address += 4;
        }
      }
    }
    
    state.get_register(irn) = rn_new;
  }
};

}
