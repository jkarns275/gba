module;

#include <string>
#include <unordered_map>

#include <catch2/catch_test_macros.hpp>
#include <spdlog/spdlog.h>

export module test.arm7tdmi.arm.branch;

import arm7tdmi.arm;
import arm7tdmi.instruction;

using std::string;
using std::unordered_map;

TEST_CASE("branch::B") {
  unordered_map<string, gword_t> values =
      BranchWithLink::definition->generate_value_map();
  values["L"] = 0;

  SimpleMemory memory;
  CpuState state(memory);

  const gword_t LR_VALUE = 1431;
  const gword_t START_ADDRESS = 0x100;

  state.write_lr(LR_VALUE);

  // Positive offset
  for (gword_t offset = 0; offset < 0x100; offset += 0x10) {
    state.write_pc(START_ADDRESS);

    values["offset"] = offset;

    gword_t instruction = BranchWithLink::definition->build(values);
    BranchWithLink be(instruction);
    be.execute(state);

    REQUIRE(state.read_current_pc() == START_ADDRESS + 8 + (offset << 2));
    REQUIRE(state.read_lr() == LR_VALUE);
    REQUIRE(!state.get_flag(CpuState::T_FLAG));
  }

  // Negative offset
  for (gword_t offset = 1; offset < 0x100; offset += 0x10) {
    state.write_pc(START_ADDRESS);

    values["offset"] = -offset;

    gword_t instruction = BranchWithLink::definition->build(values);
    BranchWithLink be(instruction);
    be.execute(state);

    REQUIRE(state.read_current_pc() == START_ADDRESS + 8 - (offset << 2));
    REQUIRE(state.read_lr() == LR_VALUE);
    REQUIRE(!state.get_flag(CpuState::T_FLAG));
  }
}

TEST_CASE("branch::BL") {
  unordered_map<string, gword_t> values =
      BranchWithLink::definition->generate_value_map();
  values["L"] = 1;

  SimpleMemory memory;
  CpuState state(memory);

  const gword_t LR_VALUE = 1431;
  const gword_t START_ADDRESS = 0x100;

  // Positive offset
  for (gword_t offset = 1; offset < 0x100; offset += 0x10) {
    state.clear_flag(CpuState::T_FLAG);
    state.write_pc(START_ADDRESS);
    state.write_lr(LR_VALUE);

    values["offset"] = offset;

    gword_t instruction = BranchWithLink::definition->build(values);
    BranchWithLink be(instruction);
    be.execute(state);

    REQUIRE(state.read_current_pc() == START_ADDRESS + 8 + (offset << 2));
    REQUIRE(state.read_lr() == START_ADDRESS + 4);
    REQUIRE(!state.get_flag(CpuState::T_FLAG));
  }

  // Negative offset
  for (gword_t offset = 1; offset < 0x100; offset += 0x10) {
    state.clear_flag(CpuState::T_FLAG);
    state.write_pc(START_ADDRESS);
    state.write_lr(LR_VALUE);

    values["offset"] = -offset;

    gword_t instruction = BranchWithLink::definition->build(values);
    BranchWithLink be(instruction);
    be.execute(state);

    REQUIRE(state.read_current_pc() == START_ADDRESS + 8 - (offset << 2));
    REQUIRE(state.read_lr() == START_ADDRESS + 4);
    REQUIRE(!state.get_flag(CpuState::T_FLAG));
  }
}

TEST_CASE("branch::BLX (imm)") {
  unordered_map<string, gword_t> values =
      BranchWithLink::definition->generate_value_map();
  values["cond"] = 0xF;

  SimpleMemory memory;
  CpuState state(memory);

  const gword_t LR_VALUE = 1431;
  const gword_t START_ADDRESS = 0x1000;

  for (gword_t l = 0; l < 2; l++) {
    values["L"] = l;
    // Positive offset
    for (gword_t offset = 1; offset < 0x100; offset += 0x10) {
      state.clear_flag(CpuState::T_FLAG);
      state.write_pc(START_ADDRESS);
      state.write_lr(LR_VALUE);

      values["offset"] = offset;

      gword_t instruction = BranchWithLink::definition->build(values);
      BranchWithLink be(instruction);
      be.execute(state);

      REQUIRE(state.read_current_pc() ==
              START_ADDRESS + 8 + (offset << 2) + (l << 1));
      REQUIRE(state.read_lr() == START_ADDRESS + 4);
      REQUIRE(state.get_flag(CpuState::T_FLAG));
    }
  }

  for (gword_t l = 0; l < 2; l++) {
    values["L"] = l;
    // Positive offset
    for (gword_t offset = 1; offset < 0x100; offset += 0x10) {
      state.clear_flag(CpuState::T_FLAG);
      state.write_pc(START_ADDRESS);
      state.write_lr(LR_VALUE);

      values["offset"] = -offset;

      gword_t instruction = BranchWithLink::definition->build(values);
      BranchWithLink be(instruction);
      be.execute(state);

      REQUIRE(state.read_current_pc() ==
              START_ADDRESS + 8 - (offset << 2) + (l << 1));
      REQUIRE(state.read_lr() == START_ADDRESS + 4);
      REQUIRE(state.get_flag(CpuState::T_FLAG));
    }
  }
}

TEST_CASE("branch::BLX (reg)") {
  unordered_map<string, gword_t> values =
      BranchExchange::definition->generate_value_map();
  values["lr"] = 1;

  SimpleMemory memory;
  CpuState state(memory);

  const gword_t LR_VALUE = 1431;
  const gword_t START_ADDRESS = 0x1000;
  const gword_t OFFSET = 0x10;

  for (gword_t bit = 0; bit < 2; bit++) {
    for (gword_t r = 0; r < 14; r++) {
      state.clear_flag(CpuState::T_FLAG);
      state.write_pc(START_ADDRESS);
      state.write_lr(LR_VALUE);
      state.write_register(r, (START_ADDRESS + OFFSET) | bit);

      values["Rm"] = r;

      gword_t instruction = BranchExchange::definition->build(values);
      BranchExchange be(instruction);
      be.execute(state);

      REQUIRE(state.read_current_pc() == ((START_ADDRESS + OFFSET) & ~1));
      REQUIRE(state.read_lr() == START_ADDRESS + 4);
      REQUIRE(bool(state.get_flag(CpuState::T_FLAG)) ==
              bool(state.read_register(r) & 1));
    }
  }
}

TEST_CASE("branch::BX") {
  unordered_map<string, gword_t> values =
      BranchExchange::definition->generate_value_map();
  values["lr"] = 0;

  SimpleMemory memory;
  CpuState state(memory);

  const gword_t LR_VALUE = 1431;
  const gword_t START_ADDRESS = 0x1000;
  const gword_t OFFSET = 0x10;

  for (gword_t bit = 0; bit < 2; bit++) {
    for (gword_t r = 0; r < 14; r++) {
      state.clear_flag(CpuState::T_FLAG);
      state.write_pc(START_ADDRESS);
      state.write_lr(LR_VALUE);
      state.write_register(r, (START_ADDRESS + OFFSET) | bit);

      values["Rm"] = r;

      gword_t instruction = BranchExchange::definition->build(values);
      BranchExchange be(instruction);
      be.execute(state);

      REQUIRE(state.read_current_pc() == ((START_ADDRESS + OFFSET) & ~1));
      REQUIRE(state.read_lr() == LR_VALUE);
      REQUIRE(bool(state.get_flag(CpuState::T_FLAG)) ==
              bool(state.read_register(r) & 1));
    }
  }
}
