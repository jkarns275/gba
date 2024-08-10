module;

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

export module test.arm7tdmi.arm.data_processing;

import arm7tdmi;
import arm7tdmi.arm;
import arm7tdmi.instruction;

import test.arm7tdmi.test_utils;

using std::string;
using std::unordered_map;
using std::vector;

using Opcode = DataProcessing::Opcode;

void set_imm_rotate(unordered_map<string, u32> &values, u32 imm, u32 rotate) {
  values["imm"] = imm;
  values["rotate"] = rotate;
}

template <u8 Opcode> struct DataProcessingTest;

template <u8 Opcode> struct Mock {
  u32 expected_value(DataProcessingTest<Opcode> &d, CpuState &state);
};

template <u8 Opcode>
struct DataProcessingTest : public ArmInstructionTestWithFlags<DataProcessing> {
  bool s;
  u8 irn, ird;
  u32 rn;

  DataProcessingTest(bool s, u8 irn, u8 ird, u32 rn, u32 input_flags,
                     u32 output_flags)
      : ArmInstructionTestWithFlags<DataProcessing>(input_flags, output_flags),
        s(s), irn(irn), ird(ird), rn(rn) {}

  void prepare_state(CpuState &state) override {
    ArmInstructionTestWithFlags<DataProcessing>::prepare_state(state);

    value_map["Rn"] = irn;
    value_map["Rd"] = ird;
    value_map["S"] = s;
    value_map["opcode"] = Opcode;

    state.write_register(irn, rn);
  }

  void check_requirements(CpuState &state) override {
    if (calculate_operand(state).carry)
      output_flags |= CpuState::C_FLAG;

    REQUIRE(state.read_register(ird) == expected_value(state));

    if (s) {
      ArmInstructionTestWithFlags<DataProcessing>::check_requirements(state);
    }
  }

  virtual ShifterOperandValue calculate_operand(CpuState &state) = 0;

  u32 expected_value(CpuState &state) {
    return Mock<Opcode>().expected_value(*this, state);
  }
};

template <u8 Opcode> struct RegShiftTest : public DataProcessingTest<Opcode> {
  u8 irs;
  u32 rs;
  BitShift shift;
  u8 irm;
  u32 rm;

  RegShiftTest(bool s, u8 irn, u8 ird, u32 rn, u32 input_flags,
               u32 output_flags, u8 irs, u32 rs, BitShift shift, u8 irm, u32 rm)
      : DataProcessingTest<Opcode>(s, irn, ird, rn, input_flags, output_flags),
        irs(irs), rs(rs), shift(shift), irm(irm), rm(rm) {}

  const InstructionDefinition &get_definition() override {
    return *DataProcessing::definitions[1];
  }

  ShifterOperandValue calculate_operand(CpuState &state) override {
    return RegShiftOperand(irs, irm, shift).evaluate(state);
  }

  void prepare_state(CpuState &state) override {
    DataProcessingTest<Opcode>::prepare_state(state);
    state.write_register(irm, rm);
    state.write_register(irs, rs);
    this->value_map["Rm"] = irm;
    this->value_map["Rs"] = irs;
    this->value_map["shift type"] = shift;
  }
};

template <u8 Opcode> void no_flag_tests_reg_shift() {
  const u32 RN = 0;
  const u32 RD = 1;
  const u32 RM = 2;
  const u32 RS = 3;

  auto rn = GENERATE(0, 1, 0x7FFFFFFE, 0xFFFFFFFE);
  auto rs = GENERATE(0, 1, 2, 4, 8, 16, 31, 255);
  auto rm = GENERATE(1, 3, 5, 123, (u32)-41, (u32)-12345);
  auto sh = GENERATE(BitShift::LEFT, BitShift::LRIGHT, BitShift::ARIGHT);

  RegShiftTest<Opcode> test(false, RN, RD, rn, 0, 0, RS, rs, sh, RM, rm);
  test.test();

  RegShiftTest<Opcode> test_with_carry(false, RN, RD, rn, CpuState::C_FLAG, 0,
                                       RS, rs, sh, RM, rm);
  test_with_carry.test();
}

template <u8 Opcode> struct ImmRotateTest : public DataProcessingTest<Opcode> {
  u8 rotate_imm, imm8;

  ImmRotateTest(bool s, u8 irn, u8 ird, u32 rn, u32 input_flags,
                u32 output_flags, u8 rotate_imm, u8 imm8)
      : DataProcessingTest<Opcode>(s, irn, ird, rn, input_flags, output_flags),
        rotate_imm(rotate_imm), imm8(imm8) {}

  const InstructionDefinition &get_definition() override {
    return *DataProcessing::definitions[2];
  }

  void prepare_state(CpuState &state) override {
    DataProcessingTest<Opcode>::prepare_state(state);
    this->value_map["rotate"] = rotate_imm;
    this->value_map["imm"] = imm8;
  }

  ShifterOperandValue calculate_operand(CpuState &state) override {
    return RotateOperand(rotate_imm, imm8).evaluate(state);
  }
};

template <u8 Opcode> void no_flag_tests_imm_rotate() {
  const u32 RN = 0;
  const u32 RD = 1;

  auto rn = GENERATE(0, 1, 0x7FFFFFFE, 0xFFFFFFFE);
  auto rotate = GENERATE(1, 2, 3, 15);
  auto imm = GENERATE(0, 1, 3, 4, 6, 14, 15, 16, 254, 255);

  ImmRotateTest<Opcode> test(false, RN, RD, rn, 0, 0, rotate, imm);
  test.test();

  ImmRotateTest<Opcode> test_with_carry(false, RN, RD, rn, CpuState::C_FLAG, 0,
                                        rotate, imm);
  test_with_carry.test();
}

template <u8 Opcode> struct ImmShiftTest : public DataProcessingTest<Opcode> {
  u8 shift_imm;
  BitShift shift;
  u8 irm;
  u32 rm;

  ImmShiftTest(bool s, u8 irn, u8 ird, u32 rn, u32 input_flags,
               u32 output_flags, u8 shift_imm, BitShift shift, u8 irm, u32 rm)
      : DataProcessingTest<Opcode>(s, irn, ird, rn, input_flags, output_flags),
        shift_imm(shift_imm), shift(shift), irm(irm), rm(rm) {}

  const InstructionDefinition &get_definition() override {
    return *DataProcessing::definitions[0];
  }

  void prepare_state(CpuState &state) override {
    DataProcessingTest<Opcode>::prepare_state(state);

    this->value_map["shift amount"] = shift_imm;
    this->value_map["shift type"] = shift;
    this->value_map["Rm"] = irm;

    state.write_register(irm, rm);
  }

  ShifterOperandValue calculate_operand(CpuState &state) override {
    return ImmShiftOperand(shift_imm, irm, shift).evaluate(state);
  }
};

template <u8 Opcode> void no_flag_tests_imm_shift() {
  const u32 RM = 0;
  const u32 RD = 1;
  const u32 RN = 2;

  auto shift_imm = GENERATE(0, 1, 2, 4, 31, 255);
  auto sh = GENERATE(BitShift::LEFT, BitShift::LRIGHT, BitShift::ARIGHT);
  auto rm = GENERATE(1, 3, 5, 123, (u32)-41, (u32)-12345, -1);
  auto rn = GENERATE(0, 1, 0x7FFFFFFE, 0xFFFFFFFE);

  SECTION("without carry") {
    ImmShiftTest<Opcode> test(false, RN, RD, rn, 0, 0, shift_imm, sh, RM, rm);
    test.test();
  }

  SECTION("with carry") {
    ImmShiftTest<Opcode> test_with_carry(false, RN, RD, rn, CpuState::C_FLAG, 0,
                                         shift_imm, sh, RM, rm);
    test_with_carry.test();
  }
}

template <>
u32 Mock<Opcode::ADC>::expected_value(DataProcessingTest<Opcode::ADC> &d,
                                      CpuState &state) {
  return state.read_register(d.irn) + d.calculate_operand(state).value +
         bool((u32)d.input_flags & CpuState::C_FLAG);
}

TEST_CASE("ADC (reg shift)") {
  const u32 RN = 0;
  const u32 RD = 1;
  const u32 RM = 2;
  const u32 RS = 3;

  no_flag_tests_reg_shift<Opcode::ADC>();

  // Test signed overflow
  SECTION("without carry") {
    RegShiftTest<Opcode::ADC> signed_overflow(
        true, RN, RD, 0x7FFFFFFE, 0, CpuState::V_FLAG | CpuState::N_FLAG, RS, 1,
        BitShift::LEFT, RM, 1);
    signed_overflow.test();
  }

  // Test unsigned overflow i.e. carry bit
  SECTION("with carry") {
    RegShiftTest<Opcode::ADC> test_carry(true, RN, RD, 0xFFFFFFFF, 0,
                                         CpuState::C_FLAG, RS, 1,
                                         BitShift::LEFT, RM, 1);
    test_carry.test();
  }
}

TEST_CASE("ADC (imm shift)") {
  const u32 RN = 0;
  const u32 RD = 1;
  const u32 RM = 2;

  no_flag_tests_imm_shift<Opcode::ADC>();

  // Test signed overflow
  SECTION("without carry") {
    ImmShiftTest<Opcode::ADC> signed_overflow(
        true, RN, RD, 0x7FFFFFFF, 0, CpuState::V_FLAG | CpuState::N_FLAG, 1,
        BitShift::LEFT, RM, 1);
    signed_overflow.test();
  }

  // Test unsigned overflow i.e. carry bit
  SECTION("with carry") {
    ImmShiftTest<Opcode::ADC> test_carry(true, RN, RD, 0xFFFFFFFF, 0,
                                         CpuState::C_FLAG, 1, BitShift::LEFT,
                                         RM, 1);
    test_carry.test();
  }
}

TEST_CASE("ADC (imm rot)") {
  const u32 RN = 0;
  const u32 RD = 1;

  no_flag_tests_imm_rotate<Opcode::ADC>();

  // Test signed overflow
  SECTION("signed overflow") {
    ImmRotateTest<Opcode::ADC> signed_overflow(
        true, RN, RD, 0x7FFFFFFF, 0, CpuState::V_FLAG | CpuState::N_FLAG, 0, 1);
    signed_overflow.test();
  }

  // Test unsigned overflow i.e. carry bit
  SECTION("unsigned overflow") {
    ImmRotateTest<Opcode::ADC> test_carry(
        true, RN, RD, 0xFFFFFFFF, 0, CpuState::C_FLAG | CpuState::Z_FLAG, 0, 1);
    test_carry.test();
  }
}

template <>
u32 Mock<Opcode::ADD>::expected_value(DataProcessingTest<Opcode::ADD> &d,
                                      CpuState &state) {
  return state.read_register(d.irn) + d.calculate_operand(state).value;
}

TEST_CASE("ADD (reg shift)") {
  const u32 RN = 0;
  const u32 RD = 1;
  const u32 RM = 2;
  const u32 RS = 3;

  no_flag_tests_reg_shift<Opcode::ADD>();

  // Test signed overflow
  SECTION("signed overflow") {
    RegShiftTest<Opcode::ADD> signed_overflow(
        true, RN, RD, 0x7FFFFFFE, 0, CpuState::V_FLAG | CpuState::N_FLAG, RS, 1,
        BitShift::LEFT, RM, 1);
    signed_overflow.test();
  }

  // Test unsigned overflow i.e. carry bit
  SECTION("unsigned overflow") {
    RegShiftTest<Opcode::ADD> test_carry(true, RN, RD, 0xFFFFFFFF, 0,
                                         CpuState::C_FLAG, RS, 1,
                                         BitShift::LEFT, RM, 1);
    test_carry.test();
  }
}

TEST_CASE("ADD (imm shift)") {
  const u32 RN = 0;
  const u32 RD = 1;
  const u32 RM = 2;

  no_flag_tests_imm_shift<Opcode::ADD>();

  // Test signed overflow
  SECTION("signed overflow") {
    ImmShiftTest<Opcode::ADD> signed_overflow(
        true, RN, RD, 0x7FFFFFFF, 0, CpuState::V_FLAG | CpuState::N_FLAG, 1,
        BitShift::LEFT, RM, 1);
    signed_overflow.test();
  }

  // Test unsigned overflow i.e. carry bit
  SECTION("unsigned overflow") {
    ImmShiftTest<Opcode::ADD> test_carry(true, RN, RD, 0xFFFFFFFF, 0,
                                         CpuState::C_FLAG, 1, BitShift::LEFT,
                                         RM, 1);
    test_carry.test();
  }
}

TEST_CASE("ADD (imm rot)") {
  const u32 RN = 0;
  const u32 RD = 1;

  no_flag_tests_imm_rotate<Opcode::ADD>();

  // Test signed overflow
  SECTION("signed overflow") {
    ImmRotateTest<Opcode::ADD> signed_overflow(
        true, RN, RD, 0x7FFFFFFF, 0, CpuState::V_FLAG | CpuState::N_FLAG, 0, 1);
    signed_overflow.test();
  }

  // Test unsigned overflow i.e. carry bit
  SECTION("unsigned overflow") {
    ImmRotateTest<Opcode::ADD> test_carry(
        true, RN, RD, 0xFFFFFFFF, 0, CpuState::C_FLAG | CpuState::Z_FLAG, 0, 1);
    test_carry.test();
  }
}

template <>
u32 Mock<Opcode::AND>::expected_value(DataProcessingTest<Opcode::AND> &d,
                                      CpuState &state) {
  return state.read_register(d.irn) & d.calculate_operand(state).value;
}

TEST_CASE("AND (reg shift)") { no_flag_tests_reg_shift<Opcode::AND>(); }

TEST_CASE("AND (imm shift)") { no_flag_tests_imm_shift<Opcode::AND>(); }

TEST_CASE("AND (imm rot)") { no_flag_tests_imm_rotate<Opcode::AND>(); }

template <>
u32 Mock<Opcode::BIC>::expected_value(DataProcessingTest<Opcode::BIC> &d,
                                      CpuState &state) {
  return state.read_register(d.irn) & ~d.calculate_operand(state).value;
}

TEST_CASE("BIC (reg shift)") { no_flag_tests_reg_shift<Opcode::BIC>(); }

TEST_CASE("BIC (imm shift)") { no_flag_tests_imm_shift<Opcode::BIC>(); }

TEST_CASE("BIC (imm rot)") { no_flag_tests_imm_rotate<Opcode::BIC>(); }

template <>
u32 Mock<Opcode::EOR>::expected_value(DataProcessingTest<Opcode::EOR> &d,
                                      CpuState &state) {
  return state.read_register(d.irn) ^ d.calculate_operand(state).value;
}

TEST_CASE("EOR (reg shift)") { no_flag_tests_reg_shift<Opcode::EOR>(); }

TEST_CASE("OR (imm shift)") { no_flag_tests_imm_shift<Opcode::EOR>(); }

TEST_CASE("OR (imm rot)") { no_flag_tests_imm_rotate<Opcode::EOR>(); }

template <>
u32 Mock<Opcode::MOV>::expected_value(DataProcessingTest<Opcode::MOV> &d,
                                      CpuState &state) {
  return d.calculate_operand(state).value;
}

// TODO: C flag carry out
TEST_CASE("OV (reg shift)") { no_flag_tests_reg_shift<Opcode::MOV>(); }

TEST_CASE("MOV (imm shift)") { no_flag_tests_imm_shift<Opcode::MOV>(); }

TEST_CASE("MOV (imm rot)") { no_flag_tests_imm_rotate<Opcode::MOV>(); }

template <>
u32 Mock<Opcode::MVN>::expected_value(DataProcessingTest<Opcode::MVN> &d,
                                      CpuState &state) {
  return ~d.calculate_operand(state).value;
}

TEST_CASE("MVN (reg shift)") { no_flag_tests_reg_shift<Opcode::MVN>(); }

TEST_CASE("MVN (imm shift)") { no_flag_tests_imm_shift<Opcode::MVN>(); }

TEST_CASE("MVN (imm rot)") { no_flag_tests_imm_rotate<Opcode::MVN>(); }

template <>
u32 Mock<Opcode::ORR>::expected_value(DataProcessingTest<Opcode::ORR> &d,
                                      CpuState &state) {
  return state.read_register(d.irn) | d.calculate_operand(state).value;
}

TEST_CASE("ORR (reg shift)") { no_flag_tests_reg_shift<Opcode::ORR>(); }

TEST_CASE("ORR (imm shift)") { no_flag_tests_imm_shift<Opcode::ORR>(); }

TEST_CASE("ORR (imm rot)") { no_flag_tests_imm_rotate<Opcode::ORR>(); }

template <>
u32 Mock<Opcode::RSB>::expected_value(DataProcessingTest<Opcode::RSB> &d,
                                      CpuState &state) {
  return d.calculate_operand(state).value - state.read_register(d.irn);
}

TEST_CASE("RSB (reg shift)") { no_flag_tests_reg_shift<Opcode::RSB>(); }

TEST_CASE("RSB (imm shift)") { no_flag_tests_imm_shift<Opcode::RSB>(); }

TEST_CASE("RSB (imm rot)") { no_flag_tests_imm_rotate<Opcode::RSB>(); }

template <>
u32 Mock<Opcode::RSC>::expected_value(DataProcessingTest<Opcode::RSC> &d,
                                      CpuState &state) {
  return d.calculate_operand(state).value - state.read_register(d.irn) -
         !bool(d.input_flags & CpuState::C_FLAG);
}

TEST_CASE("RSC (reg shift)") {
  const u32 RN = 0;
  const u32 RD = 1;
  const u32 RM = 2;
  const u32 RS = 3;

  no_flag_tests_reg_shift<Opcode::RSC>();

  // Test signed overflow
  SECTION("signed overflow") {
    RegShiftTest<Opcode::RSC> signed_overflow(
        true, RN, RD, 0x80000000, 0, CpuState::V_FLAG | CpuState::N_FLAG, RS, 2,
        BitShift::LEFT, RM, 2);
    signed_overflow.test();
  }

  // Test borrow from
  SECTION("unsigned overflow") {
    RegShiftTest<Opcode::RSC> test_no_borrow(true, RN, RD, 0x7FFFFFFF, 0,
                                             CpuState::N_FLAG, RS, 2,
                                             BitShift::LEFT, RM, 2);
    test_no_borrow.test();
  }

  SECTION("not unsigned overflow") {
    RegShiftTest<Opcode::RSC> test_borrow_not(
        true, RN, RD, 4, 0, CpuState::C_FLAG, RS, 2, BitShift::LEFT, RM, 2);
    test_borrow_not.test();
  }
}

TEST_CASE("RSC (imm shift)") { no_flag_tests_imm_shift<Opcode::RSC>(); }

TEST_CASE("RSC (imm rot)") { no_flag_tests_imm_rotate<Opcode::RSC>(); }

template <>
u32 Mock<Opcode::SBC>::expected_value(DataProcessingTest<Opcode::SBC> &d,
                                      CpuState &state) {
  return state.read_register(d.irn) - d.calculate_operand(state).value -
         !bool(d.input_flags & CpuState::C_FLAG);
}

TEST_CASE("SBC (reg shift)") {
  const u32 RN = 0;
  const u32 RD = 1;
  const u32 RM = 2;
  const u32 RS = 3;

  no_flag_tests_reg_shift<Opcode::SBC>();

  // Test signed overflow
  SECTION("signed overflow") {
    RegShiftTest<Opcode::SBC> signed_overflow(
        true, RN, RD, 0x7FFFFFFF, CpuState::C_FLAG,
        CpuState::V_FLAG | CpuState::N_FLAG, RS, 0, BitShift::LEFT, RM, -1);
    signed_overflow.test();
  }

  SECTION("unsigned overflow") {
    RegShiftTest<Opcode::SBC> test_no_borrow(
        true, RN, RD, 0, 0, CpuState::N_FLAG, RS, 0, BitShift::LEFT, RM, 0);
    test_no_borrow.test();
  }

  SECTION("not unsigned overflow") {
    RegShiftTest<Opcode::SBC> test_borrow_not(
        true, RN, RD, 16, 0, CpuState::C_FLAG, RS, 1, BitShift::LEFT, RM, 1);
    test_borrow_not.test();
  }
}

TEST_CASE("SBC (imm shift)") { no_flag_tests_imm_shift<Opcode::SBC>(); }

TEST_CASE("SBC (imm rot)") { no_flag_tests_imm_rotate<Opcode::SBC>(); }

template <>
u32 Mock<Opcode::SUB>::expected_value(DataProcessingTest<Opcode::SUB> &d,
                                      CpuState &state) {
  return state.read_register(d.irn) - d.calculate_operand(state).value;
}

TEST_CASE("SUB (reg shift)") {
  const u32 RN = 0;
  const u32 RD = 1;
  const u32 RM = 2;
  const u32 RS = 3;

  no_flag_tests_reg_shift<Opcode::SUB>();

  // Test signed overflow
  SECTION("V flag") {
    RegShiftTest<Opcode::SUB> signed_overflow(
        true, RN, RD, 0x7FFFFFFF, CpuState::C_FLAG,
        CpuState::V_FLAG | CpuState::N_FLAG, RS, 0, BitShift::LEFT, RM, -1);
    signed_overflow.test();
  }

  // Test borrow from
  SECTION("N flag") {
    RegShiftTest<Opcode::SUB> test_no_borrow(
        true, RN, RD, 0, 0, CpuState::N_FLAG, RS, 1, BitShift::LEFT, RM, 1);
    test_no_borrow.test();
  }

  SECTION("C flag") {
    RegShiftTest<Opcode::SUB> test_borrow_not(
        true, RN, RD, 16, 0, CpuState::C_FLAG, RS, 1, BitShift::LEFT, RM, 1);
    test_borrow_not.test();
  }
}

TEST_CASE("SUB (imm shift)") { no_flag_tests_imm_shift<Opcode::SUB>(); }

TEST_CASE("SUB (imm rot)") { no_flag_tests_imm_rotate<Opcode::SUB>(); }

template <>
u32 Mock<Opcode::CMN>::expected_value(DataProcessingTest<Opcode::CMN> &d,
                                      CpuState &state) {
  return state.read_register(d.ird);
}

TEST_CASE("CMN (reg shift)") {
  const u32 RN = 0;
  const u32 RD = 1;
  const u32 RM = 2;
  const u32 RS = 3;

  // Test zero
  SECTION("Z flag") {
    RegShiftTest<Opcode::CMN> zero_test(true, RN, RD, 0, 0, CpuState::Z_FLAG,
                                        RS, 0, BitShift::LEFT, RM, 0);
    zero_test.test();
  }

  // Test negative
  SECTION("N flag") {
    RegShiftTest<Opcode::CMN> negative_test(
        true, RN, RD, 0, 0, CpuState::N_FLAG, RS, 0, BitShift::LEFT, RM, -1);
    negative_test.test();
  }

  // Test carry
  SECTION("C flag") {
    RegShiftTest<Opcode::CMN> carry_test(true, RN, RD, 0xFFFFFFFF, 0,
                                         CpuState::C_FLAG, RS, 0,
                                         BitShift::LEFT, RM, 2);
    carry_test.test();
  }

  // Test overflow
  SECTION("V flag") {
    RegShiftTest<Opcode::CMN> overflow_test(true, RN, RD, 0x7FFFFFFF, 0,
                                            CpuState::N_FLAG | CpuState::V_FLAG,
                                            RS, 0, BitShift::LEFT, RM, 1);
    overflow_test.test();
  }
}

template <>
u32 Mock<Opcode::CMP>::expected_value(DataProcessingTest<Opcode::CMP> &d,
                                      CpuState &state) {
  return state.read_register(d.ird);
}

TEST_CASE("CMP (reg shift)") {
  const u32 RN = 0;
  const u32 RD = 1;
  const u32 RM = 2;
  const u32 RS = 3;

  // Test zero
  SECTION("Z flag") {
    RegShiftTest<Opcode::CMP> zero_test(true, RN, RD, 1, 0,
                                        CpuState::C_FLAG | CpuState::Z_FLAG, RS,
                                        0, BitShift::LEFT, RM, 1);
    zero_test.test();
  }

  // Test negative
  SECTION("N flag") {
    RegShiftTest<Opcode::CMP> negative_test(
        true, RN, RD, 0, 0, CpuState::N_FLAG, RS, 0, BitShift::LEFT, RM, 1);
    negative_test.test();
  }

  // Test carry not
  SECTION("C flag") {
    RegShiftTest<Opcode::CMP> carry_not_test(
        true, RN, RD, 4, 0, CpuState::C_FLAG, RS, 0, BitShift::LEFT, RM, 2);
    carry_not_test.test();
  }

  SECTION("not C flag") {
    RegShiftTest<Opcode::CMP> carry_test(true, RN, RD, 4, 0, CpuState::N_FLAG,
                                         RS, 0, BitShift::LEFT, RM, 5);
    carry_test.test();
  }

  // Test overflow
  SECTION("V flag") {
    RegShiftTest<Opcode::CMP> overflow_test(true, RN, RD, 0x7FFFFFFF, 0,
                                            CpuState::N_FLAG | CpuState::V_FLAG,
                                            RS, 0, BitShift::LEFT, RM, -1);
    overflow_test.test();
  }
}

template <>
u32 Mock<Opcode::TEQ>::expected_value(DataProcessingTest<Opcode::TEQ> &d,
                                      CpuState &state) {
  return state.read_register(d.ird);
}

TEST_CASE("TEQ (reg shift)") {
  const u32 RN = 0;
  const u32 RD = 1;
  const u32 RM = 2;
  const u32 RS = 3;

  // Test zero
  SECTION("Z flag") {
    RegShiftTest<Opcode::TEQ> zero_test(true, RN, RD, 0xDEAD, 0,
                                        CpuState::Z_FLAG, RS, 0, BitShift::LEFT,
                                        RM, 0xDEAD);
    zero_test.test();
  }

  // Test negative
  SECTION("N flag") {
    RegShiftTest<Opcode::TEQ> negative_test(true, RN, RD, 0x80000000, 0,
                                            CpuState::N_FLAG, RS, 0,
                                            BitShift::LEFT, RM, 0x7FFFFFFF);
    negative_test.test();
  }

  // Test carry
  SECTION("C flag") {
    RegShiftTest<Opcode::TEQ> carry_test(true, RN, RD, 0, 0,
                                         CpuState::C_FLAG | CpuState::N_FLAG,
                                         RS, 1, BitShift::LEFT, RM, 0xFFFFFFFF);
    carry_test.test();
  }
}
