module;

#include <bit>
#include <string>
#include <unordered_map>

#include <catch2/catch_test_macros.hpp>
#include <spdlog/spdlog.h>

export module test.arm7tdmi.arm.clz;

import arm7tdmi;
import arm7tdmi.arm;
import arm7tdmi.instruction;

using std::string;
using std::unordered_map;

TEST_CASE("CLZ") {
  unordered_map<string, u32> values =
      CountLeadingZeros::definition->generate_value_map();
  values["Rm"] = 14;

  SimpleMemory memory;
  CpuState state(memory);

  for (u32 i = 0; i < 14; i++) {
    values["Rd"] = i;
    CountLeadingZeros clz(CountLeadingZeros::definition->build(values));
    for (u32 j = 0; j < 32; j++) {
      state.write_register(14, 1u << j);

      clz.execute(state);
      REQUIRE(state.read_register(14) == 1u << j);
      REQUIRE(state.read_register(i) == count_leading_zeros(1u << j));
    }
  }

  values["Rd"] = 0;
  state.write_register(14, -1);
  CountLeadingZeros clz(CountLeadingZeros::definition->build(values));
  REQUIRE(state.read_register(0) == 0);
}
