module;

#include <string>
#include <vector>
#include <unordered_map>
#include <iostream>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>

export module test.arm7tdmi.arm.swap;

import arm7tdmi.arm;
import arm7tdmi.instruction;

import test.arm7tdmi.test_utils;

struct SingleDataSwapTest : public ArmInstructionTest<SingleDataSwap> {
  bool b;
  byte ird, irn, irm;
  gword_t rn, rm, value;

  SingleDataSwapTest(bool b, byte ird, byte irn, gword_t rn, byte irm, gword_t rm, gword_t value)
    : ArmInstructionTest<SingleDataSwap>(),
      b(b), ird(ird), irn(irn), irm(irm), rn(rn), rm(rm), value(value) { }

  const InstructionDefinition &get_definition() override {
    return *SingleDataSwap::definition;
  }

  void prepare_state(CpuState &state) override {
    ArmInstructionTest<SingleDataSwap>::prepare_state(state);

    state.write_register(irn, rn);
    state.write_register(irm, rm);

    if (b) {
      state.byte_at(rn) = value;
    } else {
      state.at(rn) = value;
    }

    value_map["Rn"] = irn;
    value_map["Rd"] = ird;
    value_map["Rm"] = irm;
    value_map["B"] = b;
  }

  void check_requirements(CpuState &state) override {
    if (b) {
      REQUIRE(state.read_register(ird) == (value & 0xFF));
      REQUIRE(state.byte_at(rn) == (rm & 0xFF));
    } else {
      gword_t target_data = ror<gword_t>(value, (rn & 0b11) * 8);
      REQUIRE(state.read_register(ird) == target_data);
      REQUIRE(state.at(rn) == rm);
    }
  }
};

TEST_CASE("SWP") {
  const gword_t IRM = 0;
  const gword_t IRD = 1;
  const gword_t IRN = 2;

  const bool b = false;

  auto rm = GENERATE(take(100, random<gword_t>(0, -1)));
  auto value = GENERATE(take(100, random<gword_t>(0, -1)));
  auto rn = GENERATE(0x100, 0x101, 0x102, 0x103);

  SingleDataSwapTest test(b, IRD, IRN, rn, IRM, rm, value);
  test.test();
}

TEST_CASE("SWPB") {
  const gword_t IRM = 0;
  const gword_t IRD = 1;
  const gword_t IRN = 2;

  const bool b = false;

  auto rm = GENERATE(range(0, 256));
  auto value = GENERATE(range(0, 256));
  auto rn = GENERATE(0x100, 0x101, 0x102, 0x103);

  SingleDataSwapTest test(b, IRD, IRN, rn, IRM, rm, value);
  test.test();
}
