module;
#include <iostream>
#include <string>
#include <unordered_map>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>

export module test.arm7tdmi.thumb.shift_imm;

import arm7tdmi.arm;
import arm7tdmi.thumb;
import arm7tdmi.instruction;

import test.arm7tdmi.test_utils;

using std::string;
using std::unordered_map;

struct TShiftImmTest : public ArmInstructionTestWithFlags<TShiftImm> {
  byte opcode, imm, irm;
  gword_t rm;
  byte ird;

  TShiftImmTest(byte opcode, byte imm, byte irm, gword_t rm, byte ird,
                gword_t input_flags, gword_t output_flags)
      : ArmInstructionTestWithFlags<TShiftImm>(input_flags, output_flags),
        opcode(opcode), imm(imm), irm(irm), rm(rm), ird(ird) {}

  void prepare_state(CpuState &state) override {
    ArmInstructionTestWithFlags<TShiftImm>::prepare_state(state);

    value_map["Rm"] = irm;
    value_map["Rd"] = ird;
    value_map["opcode"] = opcode;
    value_map["imm5"] = imm;

    state.write_register(irm, rm);

    ShifterOperandValue op =
        ImmShiftOperand(imm, irm, (BitShift)opcode).evaluate(state);

    if (op.value == 0)
      this->output_flags |= CpuState::Z_FLAG;

    if (CpuState::N_FLAG & op.value)
      this->output_flags |= CpuState::N_FLAG;

    this->output_flags |= op.carry ? CpuState::C_FLAG : 0;
  }

  void check_requirements(CpuState &state) override {
    REQUIRE(state.read_register(ird) == expected_value(state).value);

    ArmInstructionTestWithFlags<TShiftImm>::check_requirements(state);
  }

  const InstructionDefinition &get_definition() override {
    return *TShiftImm::definition;
  }

  ShifterOperandValue expected_value(CpuState &state) {
    return ImmShiftOperand(imm, irm, (BitShift)opcode).evaluate(state);
  }
};

void shift_test(BitShift shift_type) {
  const gword_t IRD = 0;
  const gword_t IRM = 1;

  auto rm = GENERATE(take(100, random<gword_t>(0, -1)));
  auto imm = GENERATE(range(0, 31));

  SECTION(std::format("rm = {:x}, imm = {:x}", rm, imm)) {
    TShiftImmTest test(shift_type, imm, IRM, rm, IRD, 0, 0);
    test.test();
  }
}

TEST_CASE("THUMB LSL (1)") { shift_test(BitShift::LEFT); }

TEST_CASE("THUMB LSR (1)") { shift_test(BitShift::LRIGHT); }

TEST_CASE("THUMB ASR (1)") { shift_test(BitShift::ARIGHT); }
