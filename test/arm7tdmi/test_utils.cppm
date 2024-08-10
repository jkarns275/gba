module;
#include <format>
#include <iostream>
#include <string>
#include <unordered_map>

#include <catch2/catch_test_macros.hpp>

export module test.arm7tdmi.test_utils;

import arm7tdmi;
import arm7tdmi.arm;
import arm7tdmi.instruction;

using std::string;
using std::unordered_map;

export {
  ;

  template <typename I>
  concept ArmInstructionType = std::is_base_of<Ins, I>::value;

  struct InstructionTestFixture {
    virtual void prepare_state(CpuState &state,
                               unordered_map<string, u32> &value_map) {}
  };

  struct Test : public InstructionTestFixture {

    unordered_map<string, u32> value_map;

    Test() {}

    virtual void evaluate(CpuState &state) = 0;
    virtual void check_requirements(CpuState &state) = 0;

    void test() {
      SimpleMemory memory;
      CpuState arm_state(memory);
      CpuState &state = arm_state;

      prepare_state(state, value_map);
      evaluate(state);
      check_requirements(state);
    }

    virtual void prepare_state(CpuState &state,
                               unordered_map<string, u32> &value_map) = 0;
  };

  template <ArmInstructionType I> struct ArmInstructionTest : public Test {
    unordered_map<string, u32> value_map;

    ArmInstructionTest() {}

    virtual const InstructionDefinition &get_definition() = 0;

    virtual void prepare_state(CpuState &state) {
      value_map = get_definition().generate_value_map();
    }

    void prepare_state(CpuState &state,
                       unordered_map<string, u32> &value_map) override {}

    void evaluate(CpuState &state) override {
      const InstructionDefinition &def = get_definition();
      u32 ins = def.build(value_map);
      I(ins).execute(state);
    }

    virtual void test() {
      SimpleMemory memory;
      CpuState state(memory);

      // state.print_registers();
      prepare_state(state);
      // state.print_registers();
      evaluate(state);
      // state.print_registers();
      check_requirements(state);
    }
  };

  template <ArmInstructionType I>
  struct ArmInstructionTestWithFlags : public ArmInstructionTest<I> {
    u32 input_flags, output_flags;

    ArmInstructionTestWithFlags(u32 input_flags, u32 output_flags)
        : ArmInstructionTest<I>(), input_flags(input_flags),
          output_flags(output_flags) {}

    void prepare_state(CpuState &state) override {
      ArmInstructionTest<I>::prepare_state(state);
      state.write_cpsr(input_flags);
    }

    void check_requirements(CpuState &state) override {
      REQUIRE(bool(output_flags & CpuState::C_FLAG) ==
              bool(state.get_flag(CpuState::C_FLAG)));
      REQUIRE(bool(output_flags & CpuState::Z_FLAG) ==
              bool(state.get_flag(CpuState::Z_FLAG)));
      REQUIRE(bool(output_flags & CpuState::V_FLAG) ==
              bool(state.get_flag(CpuState::V_FLAG)));
      REQUIRE(bool(output_flags & CpuState::N_FLAG) ==
              bool(state.get_flag(CpuState::N_FLAG)));
    }
  };
}
