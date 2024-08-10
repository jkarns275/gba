module;

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

export module test.arm7tdmi.arm.mul;

import arm7tdmi;
import arm7tdmi.arm;
import arm7tdmi.instruction;

import test.arm7tdmi.test_utils;

using std::string;
using std::unordered_map;
using std::vector;

struct MulShortTest : public ArmInstructionTestWithFlags<MulShort> {
  bool a, s;
  u8 ird, irn, irs, irm;
  u32 rn, rs, rm;

  MulShortTest(bool a, bool s, u8 ird, u8 irn, u32 rn, u8 irs, u32 rs, u8 irm,
               u32 rm)
      : ArmInstructionTestWithFlags<MulShort>(0, 0), a(a), s(s), ird(ird),
        irn(irn), irs(irs), irm(irm), rn(rn), rs(rs), rm(rm) {}

  const InstructionDefinition &get_definition() override {
    return *MulShort::definition;
  }

  void prepare_state(CpuState &state) override {
    ArmInstructionTestWithFlags<MulShort>::prepare_state(state);

    value_map["A"] = a;
    value_map["S"] = s;

    value_map["Rd"] = ird;
    value_map["Rn"] = irn;
    value_map["Rs"] = irs;
    value_map["Rm"] = irm;

    state.write_register(irn, rn);
    state.write_register(irm, rm);
    state.write_register(irs, rs);
  }

  void check_requirements(CpuState &state) override {
    u32 result = rm * rs;
    if (a)
      result += rn;

    REQUIRE(state.read_register(ird) == result);

    if (s) {
      if (result == 0)
        output_flags |= CpuState::Z_FLAG;

      output_flags |= result & CpuState::N_FLAG;

      ArmInstructionTestWithFlags<MulShort>::check_requirements(state);
    }
  }
};

void mul_test(bool a, bool s) {
  const u8 IRM = 0;
  const u8 IRD = 1;
  const u8 IRN = 2;
  const u8 IRS = 3;

  SECTION("random") {
    auto rm = GENERATE(take(100, random<u32>(0, -1)));
    auto rs = GENERATE(take(10, random<u32>(0, -1)));
    auto rn = GENERATE(take(10, random<u32>(0, -1)));

    MulShortTest test(a, s, IRD, IRN, rn, IRS, rs, IRM, rm);
    test.test();
  }

  SECTION("zero") {
    auto rm = 0;
    auto rs = 123123;
    auto rn = 0;

    MulShortTest test(a, s, IRD, IRN, rn, IRS, rs, IRM, rm);
    test.test();
  }
}

TEST_CASE("MUL") { mul_test(false, false); }

TEST_CASE("MULS") { mul_test(false, true); }

TEST_CASE("MLA") { mul_test(true, false); }

TEST_CASE("MLAS") { mul_test(true, true); }

struct MulLongTest : public ArmInstructionTestWithFlags<MulLong> {
  bool u, a, s;
  u8 ird_msw, ird_lsw, irs, irm;
  u32 rd_msw, rd_lsw, rs, rm;

  MulLongTest(bool u, bool a, bool s, u8 ird_msw, u32 rd_msw, u8 ird_lsw,
              u32 rd_lsw, u8 irs, u32 rs, u8 irm, u32 rm)
      : ArmInstructionTestWithFlags<MulLong>(0, 0), u(u), a(a), s(s),
        ird_msw(ird_msw), ird_lsw(ird_lsw), irs(irs), irm(irm), rs(rs), rm(rm) {
  }

  const InstructionDefinition &get_definition() override {
    return *MulLong::definition;
  }

  void prepare_state(CpuState &state) override {
    ArmInstructionTestWithFlags<MulLong>::prepare_state(state);

    value_map["U"] = u;
    value_map["A"] = a;
    value_map["S"] = s;

    value_map["RdHi"] = ird_msw;
    value_map["RdLo"] = ird_lsw;
    value_map["Rs"] = irs;
    value_map["Rm"] = irm;

    state.write_register(ird_msw, rd_msw);
    state.write_register(ird_lsw, rd_lsw);
    state.write_register(irm, rm);
    state.write_register(irs, rs);
  }

  void check_requirements(CpuState &state) override {
    u32 target_msw, target_lsw;
    u64 product;

    if (u) {
      i64 sproduct = (i64)rm * (i64)rs;
      product = sproduct;
    } else {
      product = (u64)rm * (u64)rs;
    }

    if (a) {
      u64 carry = rd_lsw | (((u64)rd_msw) << 32);
      product += carry;
    }

    target_msw = (product >> 32);
    target_lsw = product & 0xFFFFFFFF;

    REQUIRE(state.read_register(ird_msw) == target_msw);
    REQUIRE(state.read_register(ird_lsw) == target_lsw);

    if (s) {
      if (product == 0)
        output_flags |= CpuState::Z_FLAG;

      output_flags |= target_msw & CpuState::N_FLAG;

      ArmInstructionTestWithFlags<MulLong>::check_requirements(state);
    }
  }
};

void mul_long_test(bool u, bool a, bool s) {
  const u8 IRM = 0;
  const u8 IRD_LO = 1;
  const u8 IRD_HI = 2;
  const u8 IRS = 3;

  SECTION("random") {
    auto rm = GENERATE(take(10, random<u32>(0, -1)));
    auto rs = GENERATE(take(10, random<u32>(0, -1)));
    auto rd_lo = GENERATE(take(10, random<u32>(0, -1)));
    auto rd_hi = GENERATE(take(10, random<u32>(0, -1)));

    MulLongTest test(u, a, s, IRD_HI, rd_hi, IRD_LO, rd_lo, IRS, rs, IRM, rm);
    test.test();
  }

  SECTION("zero") {
    auto rm = 0;
    auto rs = GENERATE(take(10, random<u32>(0, -1)));
    auto rd_lo = 0;
    auto rd_hi = 0;
    MulLongTest test(u, a, s, IRD_HI, rd_hi, IRD_LO, rd_lo, IRS, rs, IRM, rm);
    test.test();
  }
}

TEST_CASE("SMLALS") { mul_long_test(true, true, true); }
TEST_CASE("SMLAL") { mul_long_test(true, true, false); }

TEST_CASE("SMULLS") { mul_long_test(true, false, true); }
TEST_CASE("SMULL") { mul_long_test(true, false, false); }

TEST_CASE("UMLALS") { mul_long_test(false, true, true); }
TEST_CASE("UMLAL") { mul_long_test(false, true, false); }

TEST_CASE("UMULLS") { mul_long_test(false, false, true); }
TEST_CASE("UMULL") { mul_long_test(false, false, false); }
