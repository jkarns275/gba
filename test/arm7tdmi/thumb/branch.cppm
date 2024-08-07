module;
#include <iostream>
#include <string>
#include <unordered_map>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>

export module test.arm7tdmi.thumb.branch;

import arm7tdmi.arm;
import arm7tdmi.thumb;
import arm7tdmi.instruction;

import test.arm7tdmi.test_utils;

template <ArmInstructionType I>
struct TBranchTest : public ArmInstructionTest<I> {
  TBranchTest() : ArmInstructionTest<I>() {}

  void check_requirements(CpuState &state) override {
    REQUIRE(state.read_current_pc() == expected_value(state));
  }

  virtual gword_t expected_value(CpuState &state) = 0;
};

struct TBranchExchangeTest : public TBranchTest<TBranchExchange> {
  byte irm;
  gword_t rm, pc;
  bool exchange, link;

  TBranchExchangeTest(byte irm, gword_t rm, gword_t pc, bool link)
    : TBranchTest<TBranchExchange>(),
      irm(irm), rm(rm), pc(pc), exchange(rm & 1), link(link) {}

  const InstructionDefinition &get_definition() override {
    return *TBranchExchange::definition;
  }

  void prepare_state(CpuState &state) override {
    ArmInstructionTest<TBranchExchange>::prepare_state(state);

    value_map["Rm"] = irm & 0x7;
    value_map["H2"] = bool(irm & 0x8);
    value_map["L"] = link;

    state.write_pc(pc);
    state.write_register(irm, rm);
    state.set_flag(CpuState::T_FLAG);
  }

  void check_requirements(CpuState &state) override {
    TBranchTest<TBranchExchange>::check_requirements(state);
   
    REQUIRE(state.get_flag(CpuState::T_FLAG) == exchange);

    if (link) {
      REQUIRE(state.read_lr() == (pc + 2));
    }
  }

  gword_t expected_value(CpuState &state) override {
    return rm & 0xFFFFFFFE;
  }
};

TEST_CASE("THUMB BX") {
  TBranchExchangeTest test(0, 0x5500, 0x1200, false);
  test.test();
}

TEST_CASE("THUMB BLX (2)") {
  TBranchExchangeTest test(0, 0x5500, 0x1200, true);
  test.test();
}
