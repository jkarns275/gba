module;
#include <string>
#include <unordered_map>

#include <iostream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>

export module test.arm7tdmi.arm.mov;

import arm7tdmi.arm;
import arm7tdmi.instruction;

import test.arm7tdmi.test_utils;

using std::string;
using std::unordered_map;

template <typename I> struct LoadStoreBaseTest : public ArmInstructionTest<I> {
  bool p, u, w, l;
  u8 ird, irn;
  u32 rn;
  u32 address, offset, value;

  LoadStoreBaseTest(bool p, bool u, bool w, bool l, u8 ird, u8 irn,
                    u32 rn, u32 offset, u32 value)
      : ArmInstructionTest<I>(), p(p), u(u), w(w), l(l), ird(ird), irn(irn),
        rn(rn), offset(offset), value(value) {
    if (p) {
      i32 sign = u ? 1 : -1;
      address = rn + sign * offset;
    } else {
      address = rn;
    }
  }

  void prepare_state(CpuState &state) override {
    ArmInstructionTest<I>::prepare_state(state);

    // this->value_map["I"] = i;
    this->value_map["P"] = p;
    this->value_map["U"] = u;
    // this->value_map["B"] = b;
    this->value_map["W"] = w;
    this->value_map["L"] = l;

    this->value_map["Rd"] = ird;
    this->value_map["Rn"] = irn;

    state.write_register(irn, rn);
  }
};

struct AddressingModeFixture : public InstructionTestFixture {
  virtual u32 get_offset(CpuState &state) = 0;
  virtual u32 get_i() = 0;
};

struct AddrMode3ImmOffset : public AddressingModeFixture {
  u8 immediate;
  u8 immed_hi, immed_lo;

  AddrMode3ImmOffset(u8 immediate)
      : immediate(immediate), immed_hi((immediate & 0xF0) >> 4),
        immed_lo(immediate & 0xF) {}

  void prepare_state(CpuState &state,
                     unordered_map<string, u32> &value_map) override {
    value_map["immed_lo"] = immed_lo;
    value_map["immed_hi"] = immed_hi;
  }

  u32 get_offset(CpuState &state) override { return immediate; }

  u32 get_i() override { return 1; }
};

struct AddrMode3RegOffset : public AddressingModeFixture {
  u8 irm;
  u32 rm;

  AddrMode3RegOffset(u8 irm, u32 rm) : irm(irm), rm(rm) {}

  void prepare_state(CpuState &state,
                     unordered_map<string, u32> &value_map) override {
    value_map["Rm"] = irm;

    state.write_register(irm, rm);
  }

  u32 get_offset(CpuState &state) override { return rm; }

  u32 get_i() override { return 0; }
};

struct AddrMode2ImmOffset : public AddressingModeFixture {
  u32 offset;

  AddrMode2ImmOffset(u32 offset)
      : AddressingModeFixture(), offset(offset) {}

  void prepare_state(CpuState &state,
                     unordered_map<string, u32> &value_map) override {
    value_map["imm"] = offset;
  }

  u32 get_offset(CpuState &state) override { return offset & 0xFFF; }

  u32 get_i() override { return 0; }
};

struct AddrMode2RegOffset : public AddressingModeFixture {
  u8 shift_imm;
  BitShift shift;
  u8 irm;
  u32 rm;

  AddrMode2RegOffset(u8 shift_imm, BitShift shift, u8 irm, u32 rm)
      : AddressingModeFixture(), shift_imm(shift_imm), shift(shift), irm(irm),
        rm(rm) {}

  void prepare_state(CpuState &state,
                     unordered_map<string, u32> &value_map) override {
    value_map["shift_by"] = shift_imm;
    value_map["shift_type"] = shift;
    value_map["Rm"] = irm;
    state.write_register(irm, rm);
  }

  u32 get_offset(CpuState &state) override {
    return ImmShiftOperand(shift_imm, irm, shift).evaluate(state).value;
  }

  u32 get_i() override { return 1; }
};

struct LoadOffsetTest : public LoadStoreBaseTest<LoadStoreOffset> {
  bool b;
  AddressingModeFixture &addressing_mode;

  LoadOffsetTest(bool p, bool u, bool b, bool w, u8 ird, u8 irn, u32 rn,
                 u32 offset, u32 value,
                 AddressingModeFixture &addressing_mode)
      : LoadStoreBaseTest<LoadStoreOffset>(p, u, w, true, ird, irn, rn, offset,
                                           value),
        b(b), addressing_mode(addressing_mode) {}

  void prepare_state(CpuState &state) override {
    LoadStoreBaseTest<LoadStoreOffset>::prepare_state(state);

    addressing_mode.prepare_state(state, value_map);

    if (b) {
      state.u8_at(address) = value & 0xFF;
    } else {
      state.at(address) = value;
    }

    value_map["I"] = addressing_mode.get_i();
  }

  const InstructionDefinition &get_definition() override {
    return *LoadStoreOffset::definitions[addressing_mode.get_i()];
  }

  static constexpr u32 switch_case(bool p, bool u) {
    return ((u32)p) + ((u32)u << 1);
  }

  void check_requirements(CpuState &state) override {
    u32 target;
    if (b) {
      target = value & 0xFF;
    } else {
      target = ror<u32>(value, 8 * (address % 4));
    }

    REQUIRE(state.read_register(ird) == target);

    i32 sign = u ? 1 : -1;
    switch (switch_case(p, w)) {
    case switch_case(false, false):
    case switch_case(false, true):
      REQUIRE(state.read_register(irn) +
              sign * addressing_mode.get_offset(state));
      break;
    case switch_case(true, false):
      break;
    case switch_case(true, true):
      REQUIRE(state.read_register(irn) == address);
      break;
    }
  }
};

template <typename T> void offset_imm_test(bool b) {
  auto offset = GENERATE(0x0, 0x1, 0x2, 0x3);
  auto p = GENERATE(false, true);
  auto u = GENERATE(false, true);
  auto w = GENERATE(false, true);

  auto rn = GENERATE(0x100, 0x101, 0x103, 0x104, 0x105, 0x106, 0x107, 0x108);

  const u8 IRD = 0;
  const u8 IRN = 1;

  AddrMode2ImmOffset addr_mode(offset);
  T test(p, u, b, w, IRD, IRN, rn, offset, 0xB00B1350, addr_mode);
  test.test();
}

template <typename T> void offset_reg_test(bool b) {
  auto p = GENERATE(false, true);
  auto u = GENERATE(false, true);
  auto w = GENERATE(false, true);

  auto rn =
      GENERATE(0x1000, 0x1001, 0x1003, 0x1004, 0x1005, 0x1006, 0x1007, 0x1008);

  const u8 IRD = 0;
  const u8 IRN = 1;
  const u8 IRM = 2;

  SECTION("LEFT") {
    AddrMode2RegOffset addr_mode_left(1, BitShift::LEFT, IRM, 2);
    T test_left(p, u, b, w, IRD, IRN, rn, 4, 0xB00b1350, addr_mode_left);
    test_left.test();
  }

  SECTION("LRIGHT") {
    AddrMode2RegOffset addr_mode_lright(1, BitShift::LRIGHT, IRM, 2);
    T test_lright(p, u, b, w, IRD, IRN, rn, 1, 0xB00b1350, addr_mode_lright);
    test_lright.test();

    AddrMode2RegOffset addr_mode_lright_2(30, BitShift::LRIGHT, IRM, -1);
    T test_lright_2(p, u, b, w, IRD, IRN, rn, 3, 0xB00b1350,
                    addr_mode_lright_2);
    test_lright_2.test();
  }

  SECTION("ARIGHT") {
    AddrMode2RegOffset addr_mode_aright(1, BitShift::ARIGHT, IRM, 2);
    T test_aright(p, u, b, w, IRD, IRN, rn, 1, 0xB00b1350, addr_mode_aright);
    test_aright.test();

    AddrMode2RegOffset addr_mode_aright_2(2, BitShift::ARIGHT, IRM, -16);
    T test_aright_2(p, u, b, w, IRD, IRN, rn, -4, 0xB00b1350,
                    addr_mode_aright_2);
    test_aright_2.test();
  }
}

TEST_CASE("LDR") {
  SECTION("ImmOffset") { offset_imm_test<LoadOffsetTest>(false); }

  SECTION("RegOffset") { offset_reg_test<LoadOffsetTest>(false); }
}

TEST_CASE("LDRB") {
  SECTION("ImmOffset") { offset_imm_test<LoadOffsetTest>(true); }

  SECTION("RegShiftOffset") { offset_reg_test<LoadOffsetTest>(true); }
}

struct StoreOffsetTest : public LoadStoreBaseTest<LoadStoreOffset> {
  bool b;
  AddressingModeFixture &addressing_mode;

  StoreOffsetTest(bool p, bool u, bool b, bool w, u8 ird, u8 irn,
                  u32 rn, u32 offset, u32 value,
                  AddressingModeFixture &addressing_mode)
      : LoadStoreBaseTest<LoadStoreOffset>(p, u, w, false, ird, irn, rn, offset,
                                           value),
        b(b), addressing_mode(addressing_mode) {}

  void prepare_state(CpuState &state) override {
    LoadStoreBaseTest<LoadStoreOffset>::prepare_state(state);

    addressing_mode.prepare_state(state, value_map);
    if (b) {
      state.write_register(ird, value & 0xFF);
    } else {
      state.write_register(ird, value);
    }

    value_map["I"] = addressing_mode.get_i();
    value_map["B"] = b;
  }

  const InstructionDefinition &get_definition() override {
    return *LoadStoreOffset::definitions[addressing_mode.get_i()];
  }

  static constexpr u32 switch_case(bool p, bool u) {
    return ((u32)p) + ((u32)u << 1);
  }

  void check_requirements(CpuState &state) override {
    if (b) {
      REQUIRE(state.u8_at(address) == (value & 0xFF));
    } else {
      REQUIRE(state.at(address) == value);
    }

    i32 sign = u ? 1 : -1;
    switch (switch_case(p, w)) {
    case switch_case(false, false):
    case switch_case(false, true):
      REQUIRE(state.read_register(irn) +
              sign * addressing_mode.get_offset(state));
      break;
    case switch_case(true, false):
      break;
    case switch_case(true, true):
      REQUIRE(state.read_register(irn) == address);
      break;
    }
  }
};

TEST_CASE("STR") {
  SECTION("ImmOffset") { offset_imm_test<StoreOffsetTest>(false); }

  SECTION("RegOffset") { offset_reg_test<StoreOffsetTest>(false); }
}

TEST_CASE("STRB") {
  SECTION("ImmOffset") { offset_imm_test<StoreOffsetTest>(true); }

  SECTION("RegOffset") { offset_reg_test<StoreOffsetTest>(true); }
}

struct LoadTest : public LoadStoreBaseTest<LoadStore> {
  bool s, h;

  AddressingModeFixture &addressing_mode;
  static constexpr u32 switch_case(bool p, bool u) {
    return ((u32)p) + ((u32)u << 1);
  }

  LoadTest(bool s, bool h, bool p, bool u, bool w, u8 ird, u8 irn,
           u32 rn, u32 offset, u32 value,
           AddressingModeFixture &addressing_mode)
      : LoadStoreBaseTest<LoadStore>(p, u, w, true, ird, irn, rn, offset,
                                     value),
        s(s), h(h), addressing_mode(addressing_mode) {}

  void prepare_state(CpuState &state) override {
    LoadStoreBaseTest<LoadStore>::prepare_state(state);

    addressing_mode.prepare_state(state, value_map);

    switch (switch_case(s, h)) {
    case switch_case(false, false):
      __builtin_unreachable();
      break;
    case switch_case(false, true):
      state.short_at(address) = value & 0xFFFF;
      break;
    case switch_case(true, false):
      state.i8_at(address) = value & 0xFF;
      break;
    case switch_case(true, true):
      state.signed_short_at(address) = value & 0xFFFF;
      break;
    }

    value_map["I"] = addressing_mode.get_i();
    value_map["S"] = s;
    value_map["H"] = h;
  }

  const InstructionDefinition &get_definition() override {
    return *LoadStore::definitions[addressing_mode.get_i()];
  }

  void check_requirements(CpuState &state) override {
    switch (switch_case(s, h)) {
    case switch_case(false, false):
      __builtin_unreachable();
      break;
    case switch_case(false, true):
      REQUIRE(state.read_register(ird) == (value & 0xFFFF));
      break;
    case switch_case(true, false): {
      i8 u8_target = value & 0xFF;
      REQUIRE(state.read_register(ird) == (i32)u8_target);
      break;
    }
    case switch_case(true, true): {
      i16 short_target = value & 0xFFFF;
      REQUIRE(state.read_register(ird) == (i32)short_target);
      break;
    }
    }

    i32 sign = u ? 1 : -1;
    switch (switch_case(p, w)) {
    case switch_case(false, false):
    case switch_case(false, true):
      REQUIRE(state.read_register(irn) +
              sign * addressing_mode.get_offset(state));
      break;
    case switch_case(true, false):
      break;
    case switch_case(true, true):
      REQUIRE(state.read_register(irn) == address);
      break;
    }
  }
};

template <typename T> void load_imm_test(bool s, bool h) {
  auto offset = GENERATE(0x0, 0x1, 0x2, 0x3);
  auto p = GENERATE(false, true);
  auto u = GENERATE(false, true);
  auto w = GENERATE(false, true);

  auto rn = GENERATE(0x100, 0x101, 0x103, 0x104, 0x105, 0x106, 0x107, 0x108);

  auto value = GENERATE(0, 1, 0xFF, 0x7F, 0xF000, 0x7FFF);

  const u8 IRD = 0;
  const u8 IRN = 1;

  AddrMode3ImmOffset addr_mode(offset);
  T test(s, h, p, u, w, IRD, IRN, rn, offset, value, addr_mode);
  test.test();
}

template <typename T> void load_reg_test(bool s, bool h) {
  auto offset = GENERATE(0x0, 0x1, 0x2, 0x3);
  auto p = GENERATE(false, true);
  auto u = GENERATE(false, true);
  auto w = GENERATE(false, true);

  auto rn = GENERATE(0x100, 0x101, 0x103, 0x104, 0x105, 0x106, 0x107, 0x108);

  auto value = GENERATE(0, 1, 0xFF, 0x7F, 0xF000, 0x7FFF);

  const u8 IRD = 0;
  const u8 IRN = 1;
  const u8 IRM = 2;

  AddrMode3RegOffset addr_mode_reg(IRM, offset);
  T test(s, h, p, u, w, IRD, IRN, rn, offset, value, addr_mode_reg);
  test.test();
}

TEST_CASE("LDRH") {
  SECTION("imm") { load_imm_test<LoadTest>(false, true); }
  SECTION("reg") { load_reg_test<LoadTest>(false, true); }
}

TEST_CASE("LDRSB") {
  SECTION("imm") { load_imm_test<LoadTest>(true, false); }
  SECTION("reg") { load_reg_test<LoadTest>(true, false); }
}

TEST_CASE("LDRSH") {
  SECTION("imm") { load_imm_test<LoadTest>(true, true); }
  SECTION("reg") { load_reg_test<LoadTest>(true, true); }
}

struct StoreTest : public LoadStoreBaseTest<LoadStore> {
  bool s, h;

  AddressingModeFixture &addressing_mode;
  static constexpr u32 switch_case(bool p, bool u) {
    return ((u32)p) + ((u32)u << 1);
  }

  StoreTest(bool s, bool h, bool p, bool u, bool w, u8 ird, u8 irn,
            u32 rn, u32 offset, u32 value,
            AddressingModeFixture &addressing_mode)
      : LoadStoreBaseTest<LoadStore>(p, u, w, false, ird, irn, rn, offset,
                                     value),
        s(s), h(h), addressing_mode(addressing_mode) {}

  void prepare_state(CpuState &state) override {
    LoadStoreBaseTest<LoadStore>::prepare_state(state);

    addressing_mode.prepare_state(state, value_map);

    switch (switch_case(s, h)) {
    case switch_case(false, true):
      state.write_register(ird, value & 0xFFFF);
      break;
    default:
      __builtin_unreachable();
    }

    value_map["I"] = addressing_mode.get_i();
    value_map["S"] = s;
    value_map["H"] = h;
  }

  const InstructionDefinition &get_definition() override {
    return *LoadStore::definitions[addressing_mode.get_i()];
  }

  void check_requirements(CpuState &state) override {
    i32 sign = u ? 1 : -1;
    switch (switch_case(p, w)) {
    case switch_case(false, false):
    case switch_case(false, true):
      REQUIRE(address == rn);
      REQUIRE(state.read_register(irn) == rn + sign * offset);
      break;
    case switch_case(true, false):
      REQUIRE(state.read_register(irn) == rn);
      break;
    case switch_case(true, true):
      REQUIRE(state.read_register(irn) == address);
      break;
    }
  }
};

template <typename T> void store_imm_test(bool s, bool h) {
  auto offset = GENERATE(0x0, 0x1, 0x2, 0x3);
  auto p = GENERATE(false, true);
  auto u = GENERATE(false, true);
  auto w = GENERATE(false, true);

  auto rn = GENERATE(0x100, 0x101, 0x103, 0x104, 0x105, 0x106, 0x107, 0x108);

  auto value = GENERATE(0, 1, 0xFF, 0x7F, 0xF000, 0x7FFF);

  const u8 IRD = 0;
  const u8 IRN = 1;

  AddrMode3ImmOffset addr_mode(offset);
  T test(s, h, p, u, w, IRD, IRN, rn, offset, value, addr_mode);
  test.test();
}

template <typename T> void store_reg_test(bool s, bool h) {
  auto offset = GENERATE(0x0, 0x1, 0x2, 0x3);
  auto p = GENERATE(false, true);
  auto u = GENERATE(false, true);
  auto w = GENERATE(false, true);

  auto rn = GENERATE(0x100, 0x101, 0x103, 0x104, 0x105, 0x106, 0x107, 0x108);

  auto value = GENERATE(0x0, 0x1, 0xFF, 0x7F, 0xF000, 0x7FFF);

  const u8 IRD = 0;
  const u8 IRN = 1;
  const u8 IRM = 2;

  AddrMode3RegOffset addr_mode_reg(IRM, offset);
  T test(s, h, p, u, w, IRD, IRN, rn, offset, value, addr_mode_reg);
  test.test();
}

TEST_CASE("STRH") {
  SECTION("imm") { store_imm_test<StoreTest>(false, true); }
  SECTION("reg") { store_reg_test<StoreTest>(false, true); }
}

struct LoadStoreMultipleTest
    : public ArmInstructionTestWithFlags<LoadStoreMultiple> {
  bool p, u, s, w, l;
  u8 irn;
  u32 rn;
  u16 register_list;

  LoadStoreMultipleTest(bool p, bool u, bool s, bool w, bool l, u8 irn,
                        u32 rn, u16 register_list, u32 input_flags,
                        u32 output_flags)
      : ArmInstructionTestWithFlags<LoadStoreMultiple>(input_flags,
                                                       output_flags),
        p(p), u(u), s(s), w(w), l(l), irn(irn), rn(rn),
        register_list(register_list) {}

  void prepare_state(CpuState &state) override {
    ArmInstructionTestWithFlags<LoadStoreMultiple>::prepare_state(state);

    value_map["P"] = p;
    value_map["U"] = u;
    value_map["S"] = s;
    value_map["W"] = w;
    value_map["L"] = l;

    value_map["register_list"] = register_list;
    value_map["Rn"] = irn;

    state.write_register(irn, rn);
  }

  const InstructionDefinition &get_definition() override {
    return *LoadStoreMultiple::definition;
  }
};

struct StoreMultipleTest : public LoadStoreMultipleTest {
  u32 start_address;
  StoreMultipleTest(bool p, bool u, bool s, bool w, u8 irn, u32 rn,
                    u16 register_list, u32 start_address,
                    u32 input_flags, u32 output_flags)
      : LoadStoreMultipleTest(p, u, s, w, false, irn, rn, register_list,
                              input_flags, output_flags),
        start_address(start_address) {}

  void prepare_state(CpuState &state) override {
    LoadStoreMultipleTest::prepare_state(state);

    for (u32 i = 0; i < 16; i++) {
      state.write_register(i, 0xCCCC0 + i, SVC);
      state.write_register(i, 0xFFFF0 + i);
    }

    if (s) {
      state.set_mode(SVC);
    }

    state.write_register(irn, rn);
  }

  void check_requirements(CpuState &state) override {
    u32 base = start_address;

    for (u32 i = 0; i < 16; i++) {
      if (register_list & flag_mask(i)) {
        if (i == irn) {
          if (s && (i == 13 || i == 14)) {
            REQUIRE(state.at(base) == 0xFFFF0 + i);
          } else {
            REQUIRE(state.at(base) == rn);
          }
        } else {
          if (i == 15) {
            REQUIRE(state.at(base) == 0xFFFFF + 8);
          } else {
            REQUIRE(state.at(base) == 0xFFFF0 + i);
          }
        }
        base += 4;
      }
    }

    if (w) {
      u32 sign = u ? 1 : -1;
      REQUIRE(state.read_register(irn) ==
              rn + (sign * count_ones(register_list)) * 4);
    }
  }
};

TEST_CASE("STM") {
  auto IRN = GENERATE(range(0, 15));

  auto w = GENERATE(false, true);
  auto s = GENERATE(false, true);

  auto register_list_x = GENERATE(range(0, 17));

  u32 register_list = 0xFFFF & ((1 << register_list_x) - 1);

  SECTION("!p !u") {
    u32 start_address = 0x104;
    start_address -= 4 * count_ones<u32>(register_list);

    StoreMultipleTest test(false, false, s, w, IRN, 0x100, register_list,
                           start_address, 0, 0);
    test.test();
  }

  SECTION("!p u") {
    u32 start_address = 0x100;

    StoreMultipleTest test(false, true, s, w, IRN, 0x100, register_list,
                           start_address, 0, 0);
    test.test();
  }

  SECTION("p !u") {
    u32 start_address = 0x100;
    start_address -= 4 * count_ones<u32>(register_list);

    StoreMultipleTest test(true, false, s, w, IRN, 0x100, register_list,
                           start_address, 0, 0);
    test.test();
  }

  SECTION("p u") {
    u32 start_address = 0x104;

    StoreMultipleTest test(true, true, s, w, IRN, 0x100, register_list,
                           start_address, 0, 0);
    test.test();
  }
}

struct LoadMultipleTest : public LoadStoreMultipleTest {
  u32 start_address;
  LoadMultipleTest(bool p, bool u, bool s, bool w, u8 irn, u32 rn,
                   u16 register_list, u32 start_address,
                   u32 input_flags, u32 output_flags)
      : LoadStoreMultipleTest(p, u, s, w, true, irn, rn, register_list,
                              input_flags, output_flags),
        start_address(start_address) {}

  void prepare_state(CpuState &state) override {
    LoadStoreMultipleTest::prepare_state(state);

    u32 address = start_address;

    if (s) {
      state.set_mode(SVC);
      state.write_spsr(SVC);
    }

    for (u32 i = 0; i < 16; i++) {
      state.at(address) = 0xFFFF0 + i;
      state.write_register(i, 0xAAAA0 + i);
      address += 4;
    }

    state.write_register(13, 0xCCCC0 + 13, SVC);
    state.write_register(14, 0xCCCC0 + 14, SVC);

    state.write_register(irn, rn);
  }

  void check_requirements(CpuState &state) override {
    bool set_user_no_pc = s && !(register_list & flag_mask(15));
    bool with_load_spsr = s && (register_list & flag_mask(15));

    if (set_user_no_pc) {
      for (u32 i = 0; i < 15; i++) {
        if (register_list & flag_mask(i) && i != irn) {
          REQUIRE(state.read_register(i, USR) == 0xFFFF0 + i);
        }
      }
      REQUIRE(state.read_current_pc() == 0xAAAAF);
    } else if (with_load_spsr) {
      for (u32 i = 0; i < 15; i++) {
        if (register_list & flag_mask(i) && i != irn) {
          REQUIRE(state.read_register(i) == 0xFFFF0 + i);
        }
      }

      REQUIRE(state.read_current_pc() == 0xFFFFE);
      REQUIRE(state.read_spsr() == SVC);
    } else {
      for (u32 i = 0; i < 16; i++) {
        if (register_list & flag_mask(i)) {
          if (i == 15) {
            REQUIRE(state.read_current_pc() == (0xFFFFE));
          } else if (i != irn) {
            REQUIRE(state.read_register(i) == 0xFFFF0 + i);
          }
        }
      }
    }

    if (w && !(register_list & flag_mask(15))) {
      u32 sign = u ? 1 : -1;
      REQUIRE(state.read_register(irn) ==
              rn + (sign * count_ones(register_list)) * 4);
    }
  }
};

TEST_CASE("LDM") {
  auto IRN = GENERATE(range(0, 15));

  auto w = GENERATE(false, true);
  auto s = GENERATE(false, true);

  auto register_list_x = GENERATE(range(0, 17));

  u32 register_list = 0xFFFF & ((1 << register_list_x) - 1);

  SECTION("!p !u") {
    u32 start_address = 0x104;
    start_address -= 4 * count_ones<u32>(register_list);

    LoadMultipleTest test(false, false, s, w, IRN, 0x100, register_list,
                          start_address, 0, 0);
    test.test();
  }

  SECTION("!p u") {
    u32 start_address = 0x100;

    LoadMultipleTest test(false, true, s, w, IRN, 0x100, register_list,
                          start_address, 0, 0);
    test.test();
  }

  SECTION("p !u") {
    u32 start_address = 0x100;
    start_address -= 4 * count_ones<u32>(register_list);

    LoadMultipleTest test(true, false, s, w, IRN, 0x100, register_list,
                          start_address, 0, 0);
    test.test();
  }

  SECTION("p u") {
    u32 start_address = 0x104;

    StoreMultipleTest test(true, true, s, w, IRN, 0x100, register_list,
                           start_address, 0, 0);
    test.test();
  }
}
