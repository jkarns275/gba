module;

#include <string>
#include <vector>
#include <unordered_map>
#include <iostream>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>

export module test.arm7tdmi.arm.swi;

import arm7tdmi.arm;
import arm7tdmi.instruction;

import test.arm7tdmi.test_utils;

struct SoftwareInterruptTest : public ArmInstructionTest<SoftwareInterrupt> {
  gword_t number;
  gword_t flags;

  SoftwareInterruptTest(gword_t number, gword_t flags)
    : ArmInstructionTest<SoftwareInterrupt>(),
      number(number), flags(flags) { }

  const InstructionDefinition &get_definition() override {
    return *SoftwareInterrupt::definition;
  }

  void prepare_state(CpuState &state) override {
    ArmInstructionTest<SoftwareInterrupt>::prepare_state(state);

    state.set_flag(flags);
    
    value_map["swi_number"] = number;
  }

  void check_requirements(CpuState &state) override {
    REQUIRE(state.read_spsr(Mode::SVC) == flags);
    REQUIRE(state.read_register(14, Mode::SVC) == 4);
    REQUIRE(state.get_mode() == Mode::SVC);
    REQUIRE(!(state.read_cpsr() & CpuState::T_FLAG));
    REQUIRE(state.read_cpsr() & CpuState::F_FLAG);
    REQUIRE(state.read_current_pc() == 0x08);
    REQUIRE((state.read_cpsr() & 0xF8000000) == (0xF8000000 & flags));
  }
};

TEST_CASE("SWI") {
  auto number = GENERATE(take(100, random<gword_t>(0, 0xFFFFFF)));
  auto flags = GENERATE(CpuState::T_FLAG, CpuState::F_FLAG, CpuState::T_FLAG | CpuState::F_FLAG, CpuState::V_FLAG);

  SoftwareInterruptTest test(number, flags);
  test.test();
}
