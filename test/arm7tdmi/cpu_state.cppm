module;

#include <catch2/catch_test_macros.hpp>

export module test.arm7tdmi.cpu_state;

import arm7tdmi.cpu_state;

TEST_CASE("test", "[nan]") {
  REQUIRE(1 == 1);
}
TEST_CASE("test2", "[nan]") {
  REQUIRE(1 == 1);
}
TEST_CASE("test3", "[nan1]") {
  REQUIRE(1 == 1);
}
TEST_CASE("test4", "[nan1]") {
  REQUIRE(1 == 1);
}
