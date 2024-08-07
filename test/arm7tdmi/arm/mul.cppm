module;

#include <string>
#include <vector>
#include <unordered_map>
#include <iostream>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>

export module test.arm7tdmi.arm.mul;

import arm7tdmi.arm;
import arm7tdmi.instruction;

import test.arm7tdmi.test_utils;

using std::string;
using std::vector;
using std::unordered_map;

struct MulShortTest : public ArmInstructionTestWithFlags<MulShort> {
  bool a, s;
  byte ird, irn, irs, irm;
  gword_t rn, rs, rm;

  MulShortTest(bool a, bool s, byte ird, byte irn, gword_t rn, byte irs, gword_t rs, byte irm, gword_t rm)
    : ArmInstructionTestWithFlags<MulShort>(0, 0),
      a(a), s(s),
      ird(ird), irn(irn), irs(irs), irm(irm),
      rn(rn), rs(rs), rm(rm) { }

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
    gword_t result = rm * rs;
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
  const byte IRM = 0;
  const byte IRD = 1;
  const byte IRN = 2;
  const byte IRS = 3;

  SECTION("random") {
    auto rm = GENERATE(take(100, random<gword_t>(0, -1)));
    auto rs = GENERATE(take(10, random<gword_t>(0, -1)));
    auto rn = GENERATE(take(10, random<gword_t>(0, -1)));

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

TEST_CASE("MUL") {
  mul_test(false, false);
}

TEST_CASE("MULS") {
  mul_test(false, true);
}

TEST_CASE("MLA") {
  mul_test(true, false);
}

TEST_CASE("MLAS") {
  mul_test(true, true);
}

struct MulLongTest : public ArmInstructionTestWithFlags<MulLong> {
  bool u, a, s;
  byte ird_msw, ird_lsw, irs, irm;
  gword_t rd_msw, rd_lsw, rs, rm;

  MulLongTest(bool u, bool a, bool s, byte ird_msw, gword_t rd_msw, byte ird_lsw, gword_t rd_lsw, byte irs, gword_t rs, byte irm, gword_t rm)
    : ArmInstructionTestWithFlags<MulLong>(0, 0),
      u(u), a(a), s(s),
      ird_msw(ird_msw), ird_lsw(ird_lsw), irs(irs), irm(irm),
      rs(rs), rm(rm) { }

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
    gword_t target_msw, target_lsw;
    glong_t product;

    if (u) {
      signed_glong_t sproduct = (signed_glong_t) rm * (signed_glong_t) rs;
      product = sproduct;
    } else {
      product = (glong_t) rm * (glong_t) rs;
    }

    if (a) {
      glong_t carry = rd_lsw | (((glong_t) rd_msw) << 32);
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
  const byte IRM = 0;
  const byte IRD_LO = 1;
  const byte IRD_HI = 2;
  const byte IRS = 3;

  SECTION("random") {
    auto rm = GENERATE(take(10, random<gword_t>(0, -1)));
    auto rs = GENERATE(take(10, random<gword_t>(0, -1)));
    auto rd_lo = GENERATE(take(10, random<gword_t>(0, -1)));
    auto rd_hi = GENERATE(take(10, random<gword_t>(0, -1)));

    MulLongTest test(u, a, s, IRD_HI, rd_hi, IRD_LO, rd_lo, IRS, rs, IRM, rm);
    test.test();
  }

  SECTION("zero") {
    auto rm = 0;
    auto rs = GENERATE(take(10, random<gword_t>(0, -1)));
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
