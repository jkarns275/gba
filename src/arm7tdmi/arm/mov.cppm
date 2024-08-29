module;
#include <assert.h>
#include <iostream>
#include <spdlog/spdlog.h>
#include <utility>
#include <variant>
#include <vector>

export module arm7tdmi.arm:mov;

import arm7tdmi;
import arm7tdmi.instruction;
import :operands;

using std::variant;
using std::vector;

export {
  ;

  // ARM LDRH
  // ARM LDRSB
  // ARM LDRSH
  // ARM STRH
  struct LoadStore : public Ins {
    static inline const vector<const InstructionDefinition *> definitions = {
        new InstructionDefinition(
            {new CondPiece(), new ValuePiece(0, 3), new BoolPiece("P"),
             new BoolPiece("U"), new BoolPiece("I"), new BoolPiece("W"),
             new BoolPiece("L"), new RegPiece("Rn"), new RegPiece("Rd"),
             new Zeros(4), new Ones(1), new BoolPiece("S"), new BoolPiece("H"),
             new Ones(1), new RegPiece("Rm")}),
        new InstructionDefinition(
            {new CondPiece(), new ValuePiece(0, 3), new BoolPiece("P"),
             new BoolPiece("U"), new BoolPiece("I"), new BoolPiece("W"),
             new BoolPiece("L"), new RegPiece("Rn"), new RegPiece("Rd"),
             new RegPiece("immed_hi"), new Ones(1), new BoolPiece("S"),
             new BoolPiece("H"), new Ones(1), new RegPiece("immed_lo")}),
    };

    static constexpr u32 MASK_P = flag_mask(24);
    static constexpr u32 MASK_U = flag_mask(23);
    static constexpr u32 MASK_B = flag_mask(22);
    static constexpr u32 MASK_W = flag_mask(21);
    static constexpr u32 MASK_L = flag_mask(20);
    static constexpr u32 MASK_S = flag_mask(6);
    static constexpr u32 MASK_H = flag_mask(5);

    // If p == 0: post-indexed addressing is used
    // If P == 1: offset addressing or pre-indexed addressing is used, controled
    // by w.
    bool p;

    // Indicates whether the offset is added to the base or subtracted.
    bool u;

    // if p == 1 and w == 1, the memory address is written back to the base
    // register
    bool w;

    // distinguishes between a load (1) and a store (0)
    bool l;

    // Signed (1) and unsigned (0) access
    bool s;

    u8 irn, ird;

    enum IntegralType { BYTE = 1, SHORT = 2, WORD = 4, LONG = 8 } integral_type;
    enum OffsetType { REGISTER = 0, IMMEDIATE = 1 } offset_type;

    u8 operand;

    LoadStore(u32 instruction)
        : Ins(instruction), p(MASK_P & instruction), u(MASK_U & instruction),
          w(MASK_W & instruction), l(MASK_L & instruction),
          s(MASK_S & instruction), irn(nibbles[4]), ird(nibbles[3]) {

      bool b = MASK_B & instruction;

      // Halfword (1) and u8 (0) access
      // bool h = MASK_H & instruction;

      offset_type = (OffsetType)b;

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

    LoadStore(u32 instruction, bool p, bool u, bool w, bool l, bool s, u8 irn,
              u8 ird, IntegralType integral_type, OffsetType offset_type,
              u8 operand)
        : Ins(instruction), p(p), u(u), w(w), l(l), s(s), irn(irn), ird(ird),
          integral_type(integral_type), offset_type(offset_type),
          operand(operand) {}

    static constexpr u32 switch_pair(OffsetType offset_type, u32 p) {
      return (u32)offset_type | (p << 1);
    }

    u32 get_address(CpuState &state) {
      const u32 sign = u ? 1 : -1;

      u32 rn = state.read_register(irn), address;

      switch (switch_pair(offset_type, p)) {
      case switch_pair(REGISTER, false):
        address = rn;
        rn += sign * state.read_register(operand);
        break;

      case switch_pair(REGISTER, true):
        address = rn + (sign * state.read_register(operand));
        rn = w ? address : rn;
        break;

      case switch_pair(IMMEDIATE, false):
        address = rn;
        rn += sign * (u32)operand;
        break;

      case switch_pair(IMMEDIATE, true):
        address = rn + (sign * (u32)operand);
        rn = w ? address : rn;
      }

      state.write_register(irn, rn);

      return address;
    }

    static constexpr u32 switch_pair(IntegralType integral_type, bool b) {
      return (u32)integral_type | ((u32)b * 16);
    }

    void load(CpuState &state) {
      u32 rd;
      u32 address = get_address(state);

      switch (switch_pair(integral_type, s)) {
      case switch_pair(BYTE, true): {
        i32 word = state.read<i8>(address);
        rd = word;
        break;
      }

      case switch_pair(SHORT, false): {
        u32 word = state.read<u16>(address);
        rd = word;
        break;
      }

      case switch_pair(SHORT, true): {
        i32 word = state.read<i16>(address);
        rd = word;
        break;
      }

      // This should not happen - when H and S flags are 0 it is actually a
      // multiply instruction.
      case switch_pair(BYTE, false):
      default:
        __builtin_unreachable();
      }

      state.write_register(ird, rd);
    }

    void store(CpuState &state) {
      u32 rd = state.read_register(ird);
      u32 address = get_address(state);

      switch (switch_pair(integral_type, s)) {
      case switch_pair(SHORT, false): {
        state.write<u16>(address, rd);
        break;
      }
      // Storing a word / u8 / signed short are all covered in other
      // instructions.
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

    static constexpr u32 switch_pair(bool s, IntegralType integral_type,
                                     bool b) {
      return (u32)s | (u32)(b << 1) | ((u32)integral_type << 2);
    }

    std::string disassemble_operand() {
      char sign = u ? '+' : '-';

      switch (switch_pair(offset_type, p)) {
      case switch_pair(REGISTER, false):
        return std::format("[{}] {}{}", pretty_reg_name(irn), sign,
                           (u32)operand);
      case switch_pair(REGISTER, true):
        return std::format("[{}, {}{}]{}", pretty_reg_name(irn), sign,
                           pretty_reg_name(operand), w ? "!" : "");
      case switch_pair(IMMEDIATE, false):
        return std::format("[{}] #{}{:x}", pretty_reg_name(irn), sign,
                           (u32)operand);
      case switch_pair(IMMEDIATE, true):
        return std::format("[{}, #{}{:x}]{}", pretty_reg_name(irn), sign,
                           (u32)operand, w ? "!" : "");
      default:
        return "<OPERAND NOT SUPPORTED IN ARM7TDMI>";
      }
    }

    std::string disassemble() override {
      std::string name_pre = l ? "LDR" : "STR";
      std::string name_post;
      switch (switch_pair(integral_type, s)) {
      case switch_pair(SHORT, false):
        name_post = "H";
        break;
      case switch_pair(SHORT, true):
        name_post = "SH";
        break;
      case switch_pair(BYTE, true):
        name_post = "SB";
        break;
      default:
        name_post = "<INVALID>";
        break;
      }

      std::string name =
          std::format("{}{}{}", name_pre, cond_to_string(cond), name_post);
      return std::format("{} {}, {}", name, pretty_reg_name(ird),
                         disassemble_operand());
    }
  };

  // ARM MRS
  struct MovStatusToReg : public Ins {
    static inline const InstructionDefinition *definition =
        new InstructionDefinition(
            {new CondPiece(), new ValuePiece(0b00010, 5), new BoolPiece("R"),
             new Zeros(2), new Ones(4), new RegPiece("Rd"), new Zeros(12)});
    static constexpr u32 MASK_R = flag_mask(22);

    bool r;
    u8 ird;

    MovStatusToReg(u32 instruction)
        : Ins(instruction), r(MASK_R & instruction), ird(nibbles[3]) {}

    void execute(CpuState &state) override {
      u32 rd;

      if (r) {
        rd = state.read_spsr();
      } else {
        rd = state.read_cpsr();
      }
      state.write_register(ird, rd);
    }

    std::string disassemble() override {
      std::string psr = r ? "SPSR" : "CPSR";
      return std::format("MRS{} {}, {}", cond_to_string(cond),
                         pretty_reg_name(ird), psr);
    }
  };

  // ARM MSR
  struct MovToStatus : public Ins {
    static inline vector<const InstructionDefinition *> definition = {
        new InstructionDefinition(
            {new CondPiece(), new ValuePiece(0b00010, 5), new BoolPiece("R"),
             new ValuePiece(0b10, 2), new RegPiece("mask"), new Ones(4),
             new Zeros(8), new IntegralPiece(12, "rm", 4)}),
        new InstructionDefinition(
            {new CondPiece(), new ValuePiece(0b00110, 5), new BoolPiece("R"),
             new ValuePiece(0b10, 2), new RegPiece("mask"), new Ones(4),
             new RegPiece("rotate"), new IntegralPiece(12, "imm", 4)})};

    bool r;
    variant<u8, RotateOperand> operand;
    u8 field_mask;

    MovToStatus(u32 instruction)
        : Ins(instruction), r(nibbles[5] & 0b0100), field_mask(nibbles[4]) {
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
      u32 operand;

      if (std::holds_alternative<u8>(this->operand)) {
        operand = state.read_register(std::get<u8>(this->operand));
      } else {
        ShifterOperandValue value =
            std::get<RotateOperand>(this->operand).evaluate(state);
        operand = value.value;
      }

      Mode mode = state.get_mode();
      if (r && !mode_has_spsr(mode))
        return;

      u32 psr = r ? state.read_spsr() : state.read_cpsr();
      u32 mask = 0;

      if ((r && mode_is_privileged(mode)) || !r) {
        mask |= field_mask & 0b0001 ? 0x000000FF : 0;
        mask |= field_mask & 0b0010 ? 0x0000FF00 : 0;
        mask |= field_mask & 0b0100 ? 0x00FF0000 : 0;
      }

      mask |= field_mask & 0b1000 ? 0xFF000000 : 0;

      psr = (mask & operand) | (~mask & psr);

      if (r)
        state.write_spsr(psr);
      else
        state.write_cpsr(psr);
    }

    std::string disassemble() override {
      std::string fields;

      if (field_mask & 0b0001)
        fields += 'c';
      if (field_mask & 0b0010)
        fields += 'x';
      if (field_mask & 0b0100)
        fields += 's';
      if (field_mask & 0b1000)
        fields += 'f';

      std::string operand;
      if (std::holds_alternative<u8>(this->operand)) {
        operand = pretty_reg_name(std::get<u8>(this->operand));
      } else {
        operand = std::get<RotateOperand>(this->operand).disassemble();
      }

      std::string psr = r ? "SPSR" : "CPSR";
      return std::format("MSR{} {}_{}, {}", cond_to_string(cond), psr, fields,
                         operand);
    }
  };

  // ARM LDR
  // ARM LDRB
  // ARM LDRBT
  // ARM LDRT
  // ARM STR
  // ARM STRB
  // ARM STRBT
  // ARM STRT
  struct LoadStoreOffset : public Ins {
    static inline const vector<const InstructionDefinition *> definitions = {
        new InstructionDefinition(
            {new CondPiece(), new ValuePiece(0b010, 3), new BoolPiece("P"),
             new BoolPiece("U"), new BoolPiece("B"), new BoolPiece("W"),
             new BoolPiece("L"), new RegPiece("Rn"), new RegPiece("Rd"),
             new IntegralPiece(12, "imm", 4)}),
        new InstructionDefinition(
            {new CondPiece(), new ValuePiece(0b011, 3), new BoolPiece("P"),
             new BoolPiece("U"), new BoolPiece("B"), new BoolPiece("W"),
             new BoolPiece("L"), new RegPiece("Rn"), new RegPiece("Rd"),
             new IntegralPiece(5, "shift_by"),
             new IntegralPiece(2, "shift_type"), new Zeros(1),
             new RegPiece("Rm")}),
    };

    static constexpr u32 MASK_I = flag_mask(25);
    static constexpr u32 MASK_P = flag_mask(24);
    static constexpr u32 MASK_U = flag_mask(23);
    static constexpr u32 MASK_B = flag_mask(22);
    static constexpr u32 MASK_W = flag_mask(21);
    static constexpr u32 MASK_L = flag_mask(20);
    static constexpr u32 MASK_S = flag_mask(6);
    static constexpr u32 MASK_H = flag_mask(5);

    bool register_offset;
    bool pre_indexed_or_offset;
    bool add_offset;
    bool w;
    bool load;

    u8 irn, ird;

    variant<u32, ImmShiftOperand> operand;
    enum DataType { BYTE = 1, WORD = 0 } data_type;

    LoadStoreOffset(u32 instruction)
        : Ins(instruction), register_offset(instruction & MASK_I),
          pre_indexed_or_offset(instruction & MASK_P),
          add_offset(instruction & MASK_U), w(instruction & MASK_W),
          load(instruction & MASK_L), irn(nibbles[4]), ird(nibbles[3]),
          data_type(instruction & MASK_B ? BYTE : WORD) {
      if (register_offset) {
        operand = ImmShiftOperand(instruction);
      } else {
        operand = 0xFFF & instruction;
      }
    }

    LoadStoreOffset(u32 instruction, bool i, bool p, bool u, bool w, bool l,
                    u8 irn, u8 ird, DataType data_type,
                    variant<u32, ImmShiftOperand> operand)
        : Ins(instruction), register_offset(i), pre_indexed_or_offset(p),
          add_offset(u), w(w), load(l), irn(irn), ird(ird), operand(operand),
          data_type(data_type) {}

    static constexpr u32 switch_pair(bool p, bool w) {
      return (p ? 1 : 0) | (w ? 2 : 0);
    }

    u32 get_address(CpuState &state) {
      u32 rn = state.read_register(irn);
      i32 sign = add_offset ? 1 : -1;
      u32 addr = rn;

      i32 offset =
          register_offset
              ? std::get<ImmShiftOperand>(operand).evaluate(state).value
              : std::get<u32>(operand);

      switch (switch_pair(pre_indexed_or_offset, w)) {
      case switch_pair(false, false):
      case switch_pair(false, true):
        rn = addr + sign * offset;
        break;

      case switch_pair(true, false):
        addr += sign * offset;
        break;

      case switch_pair(true, true):
        addr += sign * offset;
        rn = addr;
        break;
      }

      if (w)
        state.write_register(irn, rn);

      return addr;
    }

    void execute(CpuState &state) override {
      u32 rd = state.read_register(ird);
      u32 addr = get_address(state);

      if (load) {
        Mode mode = Mode::USR;

        if (pre_indexed_or_offset || !w)
          mode = state.get_mode();

        switch (data_type) {
        case BYTE:
          rd = (u32)state.memory.read<u8>(addr, mode);
          break;
        case WORD:
          rd = state.memory.rotated_at(addr, mode);
          break;
        }
      } else {
        switch (data_type) {
        case BYTE:
          spdlog::info("r{} -> {:#04x} -> {}", ird, addr, state.read<u8>(addr));
          state.write<u8>(addr, rd);
          break;
        case WORD:
          state.write<u32>(addr & ~(0b11), rd);
          break;
        }
      }

      state.write_register(ird, rd);
    }

    std::string disassemble_addressing_mode() {
      std::string operand;

      if (register_offset) {
        operand = std::get<ImmShiftOperand>(this->operand).disassemble();
      } else {
        char sign = add_offset ? '+' : '-';
        operand = std::format("#{}{:x}", sign, std::get<u32>(this->operand));
      }

      if (pre_indexed_or_offset) {
        return std::format("[{}, {}]{}", pretty_reg_name(irn), operand,
                           w ? "!" : "");
      } else {
        return std::format("[{}], {}", pretty_reg_name(irn), operand);
      }
    }

    std::string disassemble() override {
      std::string name_pre, name_post;

      if (load)
        name_pre = "LDR";
      else
        name_pre = "STR";

      if (data_type == BYTE)
        name_post.push_back('B');

      if (!pre_indexed_or_offset && w)
        name_post.push_back('T');

      return std::format("{}{}{} {}, {}", name_pre, cond_to_string(cond),
                         name_post, pretty_reg_name(ird),
                         disassemble_addressing_mode());
    }
  };

  // ARM LDM (1)
  // ARM LDM (2)
  // ARM LDM (3)
  // ARM STM (1)
  // ARM STM (2)
  struct LoadStoreMultiple : public Ins {
    static inline const InstructionDefinition *definition =
        new InstructionDefinition({new CondPiece(), new ValuePiece(0b100, 3),
                                   new BoolPiece("P"), new BoolPiece("U"),
                                   new BoolPiece("S"), new BoolPiece("W"),
                                   new BoolPiece("L"), new RegPiece("Rn"),
                                   new IntegralPiece(16, "register_list", 4)});

    static constexpr u32 MASK_PC = flag_mask(15);
    static constexpr u32 MASK_PC_ASSIGNMENT = 0xFFFFFFFE;
    static constexpr u32 MASK_P = flag_mask(24);
    static constexpr u32 MASK_U = flag_mask(23);
    static constexpr u32 MASK_S = flag_mask(22);
    static constexpr u32 MASK_W = flag_mask(21);
    static constexpr u32 MASK_L = flag_mask(20);

    bool p, u, s, w, l;
    u8 irn;
    u16 register_list;

    LoadStoreMultiple(u32 instruction)
        : Ins(instruction), p(instruction & MASK_P), u(instruction & MASK_U),
          s(instruction & MASK_S), w(instruction & MASK_W),
          l(instruction & MASK_L), irn(nibbles[4]),
          register_list(instruction & 0xFFFF) {}

    LoadStoreMultiple(u32 instruction, bool p, bool u, bool s, bool w, bool l,
                      u8 irn, u16 register_list)
        : Ins(instruction), p(p), u(u), s(s), w(w), l(l), irn(irn),
          register_list(register_list) {}

    inline std::pair<u32, u32> get_address(CpuState &state) {
      u32 rn = state.read_register(irn);
      u32 register_width = count_ones(register_list) * 4;
      u32 start = rn;

      start += (p == u) << 2;
      start -= u ? 0 : register_width;
      rn += u ? register_width : -register_width;

      return {start, rn};
    }

    void execute(CpuState &state) override {
      auto [address, rn_new] = get_address(state);

      spdlog::info("S = {}; pc in reg list = {}", s,
                   bool(register_list & MASK_PC));
      if (l) {
        Mode mode =
            s && !(register_list & MASK_PC) ? Mode::USR : state.get_mode();

        for (int i = 0; i < 15; i++) {
          if ((1 << i) & register_list) {
            state.write_register(i, state.read<u32>(address), mode);
            address += 4;
          }
        }

        if (register_list & MASK_PC) {
          if (s)
            state.write_cpsr(state.read_spsr());

          state.write_pc(state.read<u32>(address) & MASK_PC_ASSIGNMENT);
          address += 4;
        }

      } else {
        Mode mode = s ? Mode::USR : state.get_mode();

        for (int i = 0; i < 16; i++) {
          if ((1 << i) & register_list) {
            state.write<u32>(address, state.read_register(i, mode));
            address += 4;
          }
        }
      }

      state.write_register(irn, rn_new);
    }

    static constexpr u32 switch_pair(bool a, bool b) {
      return (u32)a | ((u32)b << 1);
    }

    std::string disassemble() override {
      std::string name_pre = l ? "LDM" : "STM";
      std::string name_post;
      switch (switch_pair(p, u)) {
      case switch_pair(false, false):
        name_post = "DA";
        break;
      case switch_pair(false, true):
        name_post = "IA";
        break;

      case switch_pair(true, false):
        name_post = "DB";
        break;

      case switch_pair(true, true):
        name_post = "IB";
        break;
      }

      // Use usermode when S is set for STM instructions. Use usermode when S is
      // set and pc is not in the register list for STM instructions
      std::string usermode =
          s && (!l || (l && !((1 << 15) & register_list))) ? "^" : "";

      std::string registers = "{ ";
      for (int i = 0; i < 16; i++) {
        if ((1 << i) & register_list)
          registers += std::format("{}, ", pretty_reg_name(i));
      }

      registers.erase(registers.end() - 2, registers.end());
      registers += " }";

      return std::format("{}{}{} {}, {}{}", name_pre, cond_to_string(cond),
                         name_post, pretty_reg_name(irn), registers, usermode);
    }
  };
}
