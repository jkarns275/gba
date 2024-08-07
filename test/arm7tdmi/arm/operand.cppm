module;

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

export module test.arm7tdmi.arm.operand;

import arm7tdmi.arm;
import arm7tdmi.instruction;

import test.arm7tdmi.test_utils;

using std::string;
using std::unordered_map;
using std::vector;

struct ImmShiftOperandTest : public Test {
  u8 shift_by, irm;
  u32 rm, flags;
  BitShift shift_type;
  ShifterOperandValue expected_value;

  ImmShiftOperandTest(u8 shift_by, u8 irm, u32 rm, u32 flags,
                      BitShift shift_type, ShifterOperandValue expected_value)
      : Test(), shift_by(shift_by), irm(irm), rm(rm), flags(flags),
        shift_type(shift_type), expected_value(expected_value) {}

  void evaluate(CpuState &state) override {}

  void check_requirements(CpuState &state) override {
    ShifterOperandValue value =
        ImmShiftOperand(shift_by, irm, shift_type).evaluate(state);

    REQUIRE(value.value == expected_value.value);
    REQUIRE(value.carry == expected_value.carry);
  }

  void prepare_state(CpuState &state,
                     unordered_map<string, u32> &value_map) override {
    state.write_register(irm, rm);
    state.set_flag(flags);
  }
};

struct RegShiftOperandTest : public Test {
  u8 irs, irm;
  u32 rs, rm, flags;
  BitShift shift_type;
  ShifterOperandValue expected_value;

  RegShiftOperandTest(u8 irs, u32 rs, u8 irm, u32 rm, u32 flags,
                      BitShift shift_type, ShifterOperandValue expected_value)
      : Test(), irs(irs), irm(irm), rs(rs), rm(rm), flags(flags),
        shift_type(shift_type), expected_value(expected_value) {}

  void evaluate(CpuState &state) override {}

  void check_requirements(CpuState &state) override {
    ShifterOperandValue value =
        RegShiftOperand(irs, irm, shift_type).evaluate(state);

    REQUIRE(value.value == expected_value.value);
    REQUIRE(value.carry == expected_value.carry);
  }

  void prepare_state(CpuState &state,
                     unordered_map<string, u32> &value_map) override {
    state.write_register(irm, rm);
    state.write_register(irs, rs);
    state.set_flag(flags);
  }
};

const u32 MAX = 0xFFFFFFFF;

TEST_CASE("ImmShiftOperand::LEFT") {
  const u32 IRM = 0;
  SECTION("C flag") {
    ImmShiftOperandTest test_c_flag(0, IRM, 0, CpuState::C_FLAG, BitShift::LEFT,
                                    {0, 1});
    test_c_flag.test();

    ImmShiftOperandTest test_c_flag_2(0, IRM, 2, CpuState::C_FLAG,
                                      BitShift::LEFT, {2, 1});
    test_c_flag_2.test();
  }

  SECTION("normal") {
    ImmShiftOperandTest test_normal(15, IRM, 1, 0, BitShift::LEFT,
                                    {1 << 15, 0});
    test_normal.test();
  }

  SECTION("carry") {
    ImmShiftOperandTest test_carry(4, IRM, MAX, 0, BitShift::LEFT,
                                   {MAX << 4, 1});
    test_carry.test();
  }

  SECTION("overflow") {
    ImmShiftOperandTest test_over(31, IRM, 6, 0, BitShift::LEFT, {0, 1});
    test_over.test();

    ImmShiftOperandTest test_over_2(31, IRM, 4, 0, BitShift::LEFT, {0, 0});
    test_over_2.test();
  }
}

TEST_CASE("ImmShiftOperand::LRIGHT") {
  const u32 IRM = 0;

  SECTION("normal") {
    ImmShiftOperandTest test_normal(15, IRM, 1, 0, BitShift::LRIGHT,
                                    {lsr<u32>(1, 15), 0});
    test_normal.test();
  }

  SECTION("carry") {
    ImmShiftOperandTest test_carry(0, IRM, MAX, 0, BitShift::LRIGHT, {0, 1});
    test_carry.test();

    ImmShiftOperandTest test_carry_2(1, IRM, 1, 0, BitShift::LRIGHT, {0, 1});
    test_carry_2.test();

    ImmShiftOperandTest test_carry_3(1, IRM, 2, 0, BitShift::LRIGHT, {1, 0});
    test_carry_3.test();
  }

  SECTION("overflow") {
    ImmShiftOperandTest test_over(31, IRM, MAX, 0, BitShift::LRIGHT, {1, 1});
    test_over.test();
  }
}

TEST_CASE("ImmShiftOperand::ARIGHT") {
  const u32 IRM = 0;

  SECTION("normal") {
    ImmShiftOperandTest test_normal(15, IRM, 1, 0, BitShift::ARIGHT,
                                    {lsr<u32>(1, 15), 0});
    test_normal.test();

    ImmShiftOperandTest test_normal_2(2, IRM, (u32)-4, 0, BitShift::ARIGHT,
                                      {(u32)-1, 0});
    test_normal_2.test();
  }

  SECTION("carry") {
    ImmShiftOperandTest test_carry(0, IRM, 0x80000000, 0, BitShift::ARIGHT,
                                   {MAX, 1});
    test_carry.test();

    ImmShiftOperandTest test_carry_2(0, IRM, 0x7FFFFFFF, 0, BitShift::ARIGHT,
                                     {0, 0});
    test_carry_2.test();

    ImmShiftOperandTest test_carry_3(1, IRM, 2, 0, BitShift::ARIGHT, {1, 0});
    test_carry_3.test();

    ImmShiftOperandTest test_carry_4(1, IRM, 1, 0, BitShift::ARIGHT, {0, 1});
    test_carry_4.test();

    ImmShiftOperandTest test_carry_5(1, IRM, (u32)-2, 0, BitShift::ARIGHT,
                                     {(u32)-1, 0});
    test_carry_5.test();

    ImmShiftOperandTest test_carry_6(1, IRM, (u32)-1, 0, BitShift::ARIGHT,
                                     {(u32)-1, 1});
    test_carry_6.test();
  }

  SECTION("overflow") {
    ImmShiftOperandTest test_over(31, IRM, MAX, 0, BitShift::ARIGHT,
                                  {(u32)-1, 1});
    test_over.test();
  }
}

TEST_CASE("ImmShiftOperand::ROR") {
  const u32 IRM = 0;

  SECTION("normal") {
    ImmShiftOperandTest test_normal(15, IRM, 1, 0, BitShift::ROR,
                                    {ror<u32>(1, 15), 0});
    test_normal.test();

    auto imm = GENERATE(range(1, 31));
    ImmShiftOperandTest test_normal_2(imm, IRM, (u32)-1, 0, BitShift::ROR,
                                      {(u32)-1, 1});
    test_normal_2.test();
  }

  SECTION("flag") {
    ImmShiftOperandTest test_flag(0, IRM, (u32)-2, CpuState::C_FLAG,
                                  BitShift::ROR, {(u32)-1, 0});
    test_flag.test();

    ImmShiftOperandTest test_flag_2(0, IRM, 1, CpuState::C_FLAG, BitShift::ROR,
                                    {0x80000000, 1});
    test_flag_2.test();

    ImmShiftOperandTest test_flag_3(0, IRM, 0x7FFFFFFE, CpuState::C_FLAG,
                                    BitShift::ROR, {0xBFFFFFFF, 0});
    test_flag_3.test();
  }
}

TEST_CASE("RegShiftOperand::LEFT") {
  const u32 IRM = 0;
  const u32 IRS = 1;

  SECTION("normal") {
    RegShiftOperandTest test(IRS, 1, IRM, 1, 0, BitShift::LEFT, {2, 0});
    test.test();

    RegShiftOperandTest test_2(IRS, 2, IRM, 0, 0, BitShift::LEFT, {0, 0});
    test_2.test();

    RegShiftOperandTest test_3(IRS, 44, IRM, 1, 0, BitShift::LEFT, {0, 0});
    test_3.test();
  }

  SECTION("flag") {
    RegShiftOperandTest test_flag(IRS, 0, IRM, 0xb00b, CpuState::C_FLAG,
                                  BitShift::LEFT, {0xb00b, 1});
    test_flag.test();

    RegShiftOperandTest test_flag_2(IRS, 1, IRM, 0x80000001, 0, BitShift::LEFT,
                                    {2, 1});
    test_flag_2.test();

    RegShiftOperandTest test_flag_4(IRS, 32, IRM, 1, 0, BitShift::LEFT, {0, 1});
    test_flag_4.test();
  }
}

TEST_CASE("RegShiftOperand::LRIGHT") {
  const u32 IRM = 0;
  const u32 IRS = 1;

  SECTION("normal") {
    RegShiftOperandTest test(IRS, 1, IRM, 2, 0, BitShift::LRIGHT, {1, 0});
    test.test();

    RegShiftOperandTest test_2(IRS, 2, IRM, 0, 0, BitShift::LRIGHT, {0, 0});
    test_2.test();

    RegShiftOperandTest test_3(IRS, 44, IRM, 1, 0, BitShift::LRIGHT, {0, 0});
    test_3.test();
  }

  SECTION("flag") {
    RegShiftOperandTest test_flag(IRS, 0, IRM, 0xb00b, CpuState::C_FLAG,
                                  BitShift::LRIGHT, {0xb00b, 1});
    test_flag.test();

    RegShiftOperandTest test_flag_2(IRS, 1, IRM, 3, 0, BitShift::LRIGHT,
                                    {1, 1});
    test_flag_2.test();

    RegShiftOperandTest test_flag_4(IRS, 32, IRM, (u32)-1, 0, BitShift::LRIGHT,
                                    {0, 1});
    test_flag_4.test();
  }
}

TEST_CASE("RegShiftOperand::ARIGHT") {
  const u32 IRM = 0;
  const u32 IRS = 1;

  SECTION("normal") {
    RegShiftOperandTest test(IRS, 1, IRM, 2, 0, BitShift::ARIGHT, {1, 0});
    test.test();

    RegShiftOperandTest test_2(IRS, 1, IRM, (u32)-2, 0, BitShift::ARIGHT,
                               {(u32)-1, 0});
    test_2.test();

    RegShiftOperandTest test_3(IRS, 44, IRM, 1, 0, BitShift::ARIGHT, {0, 0});
    test_3.test();

    RegShiftOperandTest test_4(IRS, 44, IRM, 0x80000000, 0, BitShift::ARIGHT,
                               {0xFFFFFFFF, 1});
    test_4.test();
  }

  SECTION("flag") {
    RegShiftOperandTest test_flag(IRS, 0, IRM, 0xb00b, CpuState::C_FLAG,
                                  BitShift::ARIGHT, {0xb00b, 1});
    test_flag.test();

    RegShiftOperandTest test_flag_2(IRS, 1, IRM, 3, 0, BitShift::ARIGHT,
                                    {1, 1});
    test_flag_2.test();

    RegShiftOperandTest test_flag_4(IRS, 32, IRM, (u32)-1, 0, BitShift::ARIGHT,
                                    {0xFFFFFFFF, 1});
    test_flag_4.test();
  }
}

TEST_CASE("RegShiftOperand::ROR") {
  const u32 IRM = 0;
  const u32 IRS = 1;

  SECTION("normal") {
    RegShiftOperandTest test_normal(IRS, 15, IRM, 1, 0, BitShift::ROR,
                                    {ror<u32>(1, 15), 0});
    test_normal.test();

    auto imm = GENERATE(range(1, 31));
    RegShiftOperandTest test_normal_2(IRS, imm, IRM, (u32)-1, 0, BitShift::ROR,
                                      {(u32)-1, 1});
    test_normal_2.test();
  }

  SECTION("flag") {
    RegShiftOperandTest test_flag(IRS, 0, IRM, 0xb00b, CpuState::C_FLAG,
                                  BitShift::ROR, {0xb00b, 1});
    test_flag.test();

    RegShiftOperandTest test_flag_2(IRS, 0b100000, IRM, 0xb00b,
                                    CpuState::C_FLAG, BitShift::ROR,
                                    {0xb00b, 0});
    test_flag_2.test();

    RegShiftOperandTest test_flag_3(IRS, 0b100000, IRM, 0x80000000,
                                    CpuState::C_FLAG, BitShift::ROR,
                                    {0x80000000, 1});
    test_flag_3.test();

    RegShiftOperandTest test_flag_4(IRS, 4, IRM, 0b1000, CpuState::C_FLAG,
                                    BitShift::ROR, {0x80000000, 1});
    test_flag_4.test();
  }
}
