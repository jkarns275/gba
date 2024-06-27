module;

#include <bit>
#include <string>
#include <unordered_map>

#include <catch2/catch_test_macros.hpp>

export module test.arm7tdmi.arm.clz;

import arm7tdmi.arm;
import arm7tdmi.instruction;

using std::string;
using std::unordered_map;

TEST_CASE("CLZ") {
  unordered_map<string, gword_t> values = CountLeadingZeros::definition->generate_value_map();
  values["Rm"] = 15;
  
  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;
  
  for (gword_t i = 0; i < 15; i++) {
    values["Rd"] = i;
    for (gword_t j = 0; j < 33; j++) {
      state.get_register(15) = 1u << j;

      CountLeadingZeros clz(CountLeadingZeros::definition->build(values));
      clz.execute(state);

      REQUIRE(state.get_register(15) == 1u << j);
      REQUIRE(state.get_register(i) == count_leading_zeros(1u << j));
    }
  }
}
