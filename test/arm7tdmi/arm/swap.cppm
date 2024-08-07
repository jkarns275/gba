module;

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

export module test.arm7tdmi.arm.swap;

import arm7tdmi.arm;
import arm7tdmi.instruction;

import test.arm7tdmi.test_utils;

struct SingleDataSwapTest : public ArmInstructionTest<SingleDataSwap> {
  bool b;
  u8 ird, irn, irm;
  u32 rn, rm, value;

  SingleDataSwapTest(bool b, u8 ird, u8 irn, u32 rn, u8 irm,
                     u32 rm, u32 value)
      : ArmInstructionTest<SingleDataSwap>(), b(b), ird(ird), irn(irn),
        irm(irm), rn(rn), rm(rm), value(value) {}

  const InstructionDefinition &get_definition() override {
    return *SingleDataSwap::definition;
  }

  void prepare_state(CpuState &state) override {
    ArmInstructionTest<SingleDataSwap>::prepare_state(state);

    state.write_register(irn, rn);
    state.write_register(irm, rm);

    if (b) {
      state.u8_at(rn) = value;
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
      REQUIRE(state.u8_at(rn) == (rm & 0xFF));
    } else {
      u32 target_data = ror<u32>(value, (rn & 0b11) * 8);
      REQUIRE(state.read_register(ird) == target_data);
      REQUIRE(state.at(rn) == rm);
    }
  }
};

TEST_CASE("SWP") {
  const u32 IRM = 0;
  const u32 IRD = 1;
  const u32 IRN = 2;

  const bool b = false;

  auto rm = GENERATE(take(100, random<u32>(0, -1)));
  auto value = GENERATE(take(100, random<u32>(0, -1)));
  auto rn = GENERATE(0x100, 0x101, 0x102, 0x103);

  SingleDataSwapTest test(b, IRD, IRN, rn, IRM, rm, value);
  test.test();
}

TEST_CASE("SWPB") {
  const u32 IRM = 0;
  const u32 IRD = 1;
  const u32 IRN = 2;

  const bool b = false;

  auto rm = GENERATE(range(0, 256));
  auto value = GENERATE(range(0, 256));
  auto rn = GENERATE(0x100, 0x101, 0x102, 0x103);

  SingleDataSwapTest test(b, IRD, IRN, rn, IRM, rm, value);
  test.test();
}
