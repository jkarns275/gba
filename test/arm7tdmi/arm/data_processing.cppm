module;

#include <string>
#include <vector>
#include <unordered_map>
#include <iostream>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

export module test.arm7tdmi.arm.data_processing;

import arm7tdmi.arm;
import arm7tdmi.instruction;

using std::string;
using std::vector;
using std::unordered_map;

using Opcode = DataProcessing::Opcode;

void set_imm_rotate(unordered_map<string, gword_t> &values, gword_t imm, gword_t rotate) {
  values["imm"] = imm;
  values["rotate"] = rotate;
}

template <byte Opcode>
struct DataProcessingTest;

template <byte Opcode>
struct Mock {
  byte get_opcode() {
    return Opcode;
  }
  gword_t expected_value(DataProcessingTest<Opcode> &d, CpuState &state);
};

template <byte Opcode>
struct DataProcessingTest {
  bool s;
  byte irn, ird;
  gword_t rn;
  gword_t input_flags;
  gword_t output_flags;

  unordered_map<string, gword_t> value_map;

  DataProcessingTest(bool s, byte irn, byte ird, gword_t rn, gword_t input_flags, gword_t output_flags)
    : s(s),
      irn(irn),
      ird(ird),
      rn(rn),
      input_flags(input_flags),
      output_flags(output_flags) {}

  byte get_opcode() { return Opcode; }
  virtual const InstructionDefinition &get_definition() = 0;
  
  virtual void prepare_state(CpuState &state) {
    value_map = get_definition().generate_value_map();
    value_map["Rn"] = irn;
    value_map["Rd"] = ird;
    value_map["S"] = s;

    state.get_register(irn) = rn;
    state.clear_flag(CpuState::ALL_FLAGS);
    state.set_flag(input_flags);
  }

  void execute(CpuState &state) {
    const InstructionDefinition &def = get_definition();

    value_map["opcode"] = get_opcode();
    gword_t ins = def.build(value_map);
    DataProcessing dp(ins);
    string out;

    dp.execute(state);
  }

  void check_requirements(CpuState &state) {
    REQUIRE(state.get_register(ird) == expected_value(state));

    if (s) {
      REQUIRE(bool(output_flags & CpuState::C_FLAG) == bool(state.get_flag(CpuState::C_FLAG)));
      REQUIRE(bool(output_flags & CpuState::Z_FLAG) == bool(state.get_flag(CpuState::Z_FLAG)));
      REQUIRE(bool(output_flags & CpuState::V_FLAG) == bool(state.get_flag(CpuState::V_FLAG)));
      REQUIRE(bool(output_flags & CpuState::N_FLAG) == bool(state.get_flag(CpuState::N_FLAG)));
    }
  }

  virtual gword_t calculate_operand(CpuState &state) = 0;
  gword_t expected_value(CpuState &state) {
    return Mock<Opcode>().expected_value(*this, state);
  }

  void test(CpuState &state) {
    prepare_state(state);
    // state.print_registers();
    execute(state);
    // state.print_registers();
    check_requirements(state);
  }

};

template<byte Opcode>
struct RegShiftTest : public DataProcessingTest<Opcode> {
  byte irs;
  gword_t rs;
  BitShift shift;
  byte irm;
  gword_t rm;

  RegShiftTest(bool s, byte irn, byte ird, gword_t rn, gword_t input_flags, gword_t output_flags, byte irs, gword_t rs, BitShift shift, byte irm, gword_t rm)
    : DataProcessingTest<Opcode>(s, irn, ird, rn, input_flags, output_flags),
      irs(irs),
      rs(rs),
      shift(shift),
      irm(irm),
      rm(rm) {}

  const InstructionDefinition &get_definition() override { return *DataProcessing::definitions[1]; }
  
  gword_t calculate_operand(CpuState &state) override {
    return RegShiftOperand(irs, irm, shift).evaluate(state).value;
  }

  void prepare_state(CpuState &state) override {
    DataProcessingTest<Opcode>::prepare_state(state);
    state.get_register(irm) = rm;
    state.get_register(irs) = rs;
    this->value_map["Rm"] = irm;
    this->value_map["Rs"] = irs;
    this->value_map["shift type"] = shift;
  }

};

template <byte Opcode>
void no_flag_tests_reg_shift() {
  const gword_t RN = 0;
  const gword_t RD = 1;
  const gword_t RM = 2;
  const gword_t RS = 3;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;

  auto rn = GENERATE(0, 1, 0x7FFFFFFE, 0xFFFFFFFE);
  auto rs = GENERATE(0, 1, 2, 4, 8, 16, 31, 255);
  auto rm = GENERATE(1, 3, 5, 123, (gword_t) -41, (gword_t) -12345);
  auto sh = GENERATE(BitShift::LEFT, BitShift::LRIGHT, BitShift::ARIGHT);
  
  RegShiftTest<Opcode> test(false, RN, RD, rn, 0, 0, RS, rs, sh, RM, rm);
  test.test(state);

  RegShiftTest<Opcode> test_with_carry(false, RN, RD, rn, CpuState::C_FLAG, 0, RS, rs, sh, RM, rm);
  test_with_carry.test(state);
}

template<byte Opcode>
struct ImmRotateTest : public DataProcessingTest<Opcode> {
  byte rotate_imm, imm8;

  ImmRotateTest(bool s, byte irn, byte ird, gword_t rn, gword_t input_flags, gword_t output_flags, byte rotate_imm, byte imm8)
    : DataProcessingTest<Opcode>(s, irn, ird, rn, input_flags, output_flags),
      rotate_imm(rotate_imm),
      imm8(imm8) {}
  
  const InstructionDefinition &get_definition() override { return *DataProcessing::definitions[2]; }
 
  void prepare_state(CpuState &state) override {
    DataProcessingTest<Opcode>::prepare_state(state);
    this->value_map["rotate"] = rotate_imm;
    this->value_map["imm"] = imm8;
  }

  gword_t calculate_operand(CpuState &state) override {
    return RotateOperand(rotate_imm, imm8).evaluate(state).value;
  }

};

template <byte Opcode>
void no_flag_tests_imm_rotate() {
  const gword_t RN = 0;
  const gword_t RD = 1;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;

  auto rn = GENERATE(0, 1, 0x7FFFFFFE, 0xFFFFFFFE);
  auto rotate = GENERATE(1, 2, 3, 15);
  auto imm = GENERATE(0, 1, 3, 4, 6, 14, 15, 16, 254, 255);
  
  ImmRotateTest<Opcode> test(false, RN, RD, rn, 0, 0, rotate, imm);
  test.test(state);

  ImmRotateTest<Opcode> test_with_carry(false, RN, RD, rn, CpuState::C_FLAG, 0, rotate, imm);
  test_with_carry.test(state);
}

template<byte Opcode>
struct ImmShiftTest : public DataProcessingTest<Opcode> {
  byte shift_imm;
  BitShift shift;
  byte irm;
  gword_t rm;

  ImmShiftTest(bool s, byte irn, byte ird, gword_t rn, gword_t input_flags, gword_t output_flags, byte shift_imm, BitShift shift, byte irm, gword_t rm)
    : DataProcessingTest<Opcode>(s, irn, ird, rn, input_flags, output_flags),
      shift_imm(shift_imm),
      shift(shift),
      irm(irm),
      rm(rm) { }

  const InstructionDefinition &get_definition() override { return *DataProcessing::definitions[0]; }
  
  void prepare_state(CpuState &state) override {
    DataProcessingTest<Opcode>::prepare_state(state);
    
    this->value_map["shift amount"] = shift_imm;
    this->value_map["shift type"] = shift;
    this->value_map["Rm"] = irm;

    state.get_register(irm) = rm;
  }

  gword_t calculate_operand(CpuState &state) override {
    return ImmShiftOperand(shift_imm, irm, shift).evaluate(state).value;
  }
};

template <byte Opcode>
void no_flag_tests_imm_shift() {
  const gword_t RM = 0;
  const gword_t RD = 1;
  const gword_t RN = 2;
  
  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;
 
  auto shift_imm = GENERATE(0, 1, 2, 4, 31, 32, 255);
  auto sh = GENERATE(BitShift::LEFT, BitShift::LRIGHT, BitShift::ARIGHT);
  auto rm = GENERATE(1, 3, 5, 123, (gword_t) -41, (gword_t) -12345);
  auto rn = GENERATE(0, 1, 0x7FFFFFFE, 0xFFFFFFFE);

  SECTION("without carry") {
    ImmShiftTest<Opcode> test(false, RN, RD, rn, 0, 0, shift_imm, sh, RM, rm);
    test.test(state);
  }
  
  SECTION("with carry") {
    ImmShiftTest<Opcode> test_with_carry(false, RN, RD, rn, 0, 0, shift_imm, sh, RM, rm);
    test_with_carry.test(state);
  }
}

template <>
gword_t Mock<Opcode::ADC>::expected_value(DataProcessingTest<Opcode::ADC> &d, CpuState &state) {
  return state.get_register(d.irn) + d.calculate_operand(state) + bool((gword_t) d.input_flags & CpuState::C_FLAG);
}

TEST_CASE("data_processing::ADC (reg shift)") {
  const gword_t RN = 0;
  const gword_t RD = 1;
  const gword_t RM = 2;
  const gword_t RS = 3;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;

  no_flag_tests_reg_shift<Opcode::ADC>();

  // Test signed overflow
  SECTION("without carry") {
    RegShiftTest<Opcode::ADC> signed_overflow(true, RN, RD, 0x7FFFFFFE, 0, CpuState::V_FLAG | CpuState::N_FLAG, RS, 1, BitShift::LEFT, RM, 1);
    signed_overflow.test(state);
  }

  // Test unsigned overflow i.e. carry bit
  SECTION("with carry") {
    RegShiftTest<Opcode::ADC> test_carry(true, RN, RD, 0xFFFFFFFF, 0, CpuState::C_FLAG, RS, 1, BitShift::LEFT, RM, 1);
    test_carry.test(state);
  }
}

TEST_CASE("data_processing::ADC (imm shift)") {
  const gword_t RN = 0;
  const gword_t RD = 1;
  const gword_t RM = 2;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;
  
  no_flag_tests_imm_shift<Opcode::ADC>();
  
  // Test signed overflow
  SECTION("without carry") {
    ImmShiftTest<Opcode::ADC> signed_overflow(true, RN, RD, 0x7FFFFFFF, 0, CpuState::V_FLAG | CpuState::N_FLAG, 1, BitShift::LEFT, RM, 1);
    signed_overflow.test(state);
  }

  // Test unsigned overflow i.e. carry bit
  SECTION("with carry") {
    ImmShiftTest<Opcode::ADC> test_carry(true, RN, RD, 0xFFFFFFFF, 0, CpuState::C_FLAG, 1, BitShift::LEFT, RM, 1);
    test_carry.test(state);
  }
}

TEST_CASE("data_processing::ADC (imm rot)") {
  const gword_t RN = 0;
  const gword_t RD = 1;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;
 
  no_flag_tests_imm_rotate<Opcode::ADC>();
  
  // Test signed overflow
  SECTION("signed overflow") {
    ImmRotateTest<Opcode::ADC> signed_overflow(true, RN, RD, 0x7FFFFFFF, 0, CpuState::V_FLAG | CpuState::N_FLAG, 0, 1);
    signed_overflow.test(state);
  }

  // Test unsigned overflow i.e. carry bit
  SECTION("unsigned overflow") {
    ImmRotateTest<Opcode::ADC> test_carry(true, RN, RD, 0xFFFFFFFF, 0, CpuState::C_FLAG | CpuState::Z_FLAG, 0, 1);
    test_carry.test(state);
  }
}

template <>
gword_t Mock<Opcode::ADD>::expected_value(DataProcessingTest<Opcode::ADD> &d, CpuState &state) {
  return state.get_register(d.irn) + d.calculate_operand(state);
}

TEST_CASE("data_processing::ADD (reg shift)") {
  const gword_t RN = 0;
  const gword_t RD = 1;
  const gword_t RM = 2;
  const gword_t RS = 3;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;

  no_flag_tests_reg_shift<Opcode::ADD>();

  // Test signed overflow
  SECTION("signed overflow") {
    RegShiftTest<Opcode::ADD> signed_overflow(true, RN, RD, 0x7FFFFFFE, 0, CpuState::V_FLAG | CpuState::N_FLAG, RS, 1, BitShift::LEFT, RM, 1);
    signed_overflow.test(state);
  }

  // Test unsigned overflow i.e. carry bit
  SECTION("unsigned overflow") {
    RegShiftTest<Opcode::ADD> test_carry(true, RN, RD, 0xFFFFFFFF, 0, CpuState::C_FLAG, RS, 1, BitShift::LEFT, RM, 1);
    test_carry.test(state);
  }
}

TEST_CASE("data_processing::ADD (imm shift)") {
  const gword_t RN = 0;
  const gword_t RD = 1;
  const gword_t RM = 2;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;
  
  no_flag_tests_imm_shift<Opcode::ADD>();
  
  // Test signed overflow
  SECTION("signed overflow") {
    ImmShiftTest<Opcode::ADD> signed_overflow(true, RN, RD, 0x7FFFFFFF, 0, CpuState::V_FLAG | CpuState::N_FLAG, 1, BitShift::LEFT, RM, 1);
    signed_overflow.test(state);
  }

  // Test unsigned overflow i.e. carry bit
  SECTION("unsigned overflow") {
    ImmShiftTest<Opcode::ADD> test_carry(true, RN, RD, 0xFFFFFFFF, 0, CpuState::C_FLAG, 1, BitShift::LEFT, RM, 1);
    test_carry.test(state);
  }
}

TEST_CASE("data_processing::ADD (imm rot)") {
  const gword_t RN = 0;
  const gword_t RD = 1;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;
  
  no_flag_tests_imm_rotate<Opcode::ADD>();
  
  // Test signed overflow
  SECTION("signed overflow") {
    ImmRotateTest<Opcode::ADD> signed_overflow(true, RN, RD, 0x7FFFFFFF, 0, CpuState::V_FLAG | CpuState::N_FLAG, 0, 1);
    signed_overflow.test(state);
  }

  // Test unsigned overflow i.e. carry bit
  SECTION("unsigned overflow") {
    ImmRotateTest<Opcode::ADD> test_carry(true, RN, RD, 0xFFFFFFFF, 0, CpuState::C_FLAG | CpuState::Z_FLAG, 0, 1);
    test_carry.test(state);
  }
}

template <>
gword_t Mock<Opcode::AND>::expected_value(DataProcessingTest<Opcode::AND> &d, CpuState &state) {
  return state.get_register(d.irn) & d.calculate_operand(state);
}

TEST_CASE("data_processing::AND (reg shift)") {
  no_flag_tests_reg_shift<Opcode::AND>();
}

TEST_CASE("data_processing::AND (imm shift)") {
  no_flag_tests_imm_shift<Opcode::AND>();
}

TEST_CASE("data_processing::AND (imm rot)") {
  no_flag_tests_imm_rotate<Opcode::AND>();
}

template <>
gword_t Mock<Opcode::BIC>::expected_value(DataProcessingTest<Opcode::BIC> &d, CpuState &state) {
  return state.get_register(d.irn) & ~d.calculate_operand(state);
}

TEST_CASE("data_processing::BIC (reg shift)") {
  no_flag_tests_reg_shift<Opcode::BIC>();
}

TEST_CASE("data_processing::BIC (imm shift)") {
  no_flag_tests_imm_shift<Opcode::BIC>();
}

TEST_CASE("data_processing::BIC (imm rot)") {
  no_flag_tests_imm_rotate<Opcode::BIC>();
}

template <>
gword_t Mock<Opcode::EOR>::expected_value(DataProcessingTest<Opcode::EOR> &d, CpuState &state) {
  return state.get_register(d.irn) ^ d.calculate_operand(state);
}

TEST_CASE("data_processing::EOR (reg shift)") {
  no_flag_tests_reg_shift<Opcode::EOR>();
}

TEST_CASE("data_processing::EOR (imm shift)") {
  no_flag_tests_imm_shift<Opcode::EOR>();
}

TEST_CASE("data_processing::EOR (imm rot)") {
  no_flag_tests_imm_rotate<Opcode::EOR>();
}

template <>
gword_t Mock<Opcode::MOV>::expected_value(DataProcessingTest<Opcode::MOV> &d, CpuState &state) {
  return d.calculate_operand(state);
}

// TODO: C flag carry out
TEST_CASE("data_processing::MOV (reg shift)") {
  no_flag_tests_reg_shift<Opcode::MOV>();
}

TEST_CASE("data_processing::MOV (imm shift)") {
  no_flag_tests_imm_shift<Opcode::MOV>();
}

TEST_CASE("data_processing::MOV (imm rot)") {
  no_flag_tests_imm_rotate<Opcode::MOV>();
}

template <>
gword_t Mock<Opcode::MVN>::expected_value(DataProcessingTest<Opcode::MVN> &d, CpuState &state) {
  return ~d.calculate_operand(state);
}

TEST_CASE("data_processing::MVN (reg shift)") {
  no_flag_tests_reg_shift<Opcode::MVN>();
}

TEST_CASE("data_processing::MVN (imm shift)") {
  no_flag_tests_imm_shift<Opcode::MVN>();
}

TEST_CASE("data_processing::MVN (imm rot)") {
  no_flag_tests_imm_rotate<Opcode::MVN>();
}

template <>
gword_t Mock<Opcode::ORR>::expected_value(DataProcessingTest<Opcode::ORR> &d, CpuState &state) {
  return state.get_register(d.irn) | d.calculate_operand(state);
}

TEST_CASE("data_processing::ORR (reg shift)") {
  no_flag_tests_reg_shift<Opcode::ORR>();
}

TEST_CASE("data_processing::ORR (imm shift)") {
  no_flag_tests_imm_shift<Opcode::ORR>();
}

TEST_CASE("data_processing::ORR (imm rot)") {
  no_flag_tests_imm_rotate<Opcode::ORR>();
}

template <>
gword_t Mock<Opcode::RSB>::expected_value(DataProcessingTest<Opcode::RSB> &d, CpuState &state) {
  return  d.calculate_operand(state) - state.get_register(d.irn);
}

TEST_CASE("data_processing::RSB (reg shift)") {
  no_flag_tests_reg_shift<Opcode::RSB>();
}

TEST_CASE("data_processing::RSB (imm shift)") {
  no_flag_tests_imm_shift<Opcode::RSB>();
}

TEST_CASE("data_processing::RSB (imm rot)") {
  no_flag_tests_imm_rotate<Opcode::RSB>();
}

template <>
gword_t Mock<Opcode::RSC>::expected_value(DataProcessingTest<Opcode::RSC> &d, CpuState &state) {
  return d.calculate_operand(state) - state.get_register(d.irn) - !bool(d.input_flags & CpuState::C_FLAG);
}

TEST_CASE("data_processing::RSC (reg shift)") {
  const gword_t RN = 0;
  const gword_t RD = 1;
  const gword_t RM = 2;
  const gword_t RS = 3;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;

  no_flag_tests_reg_shift<Opcode::RSC>();

  // Test signed overflow
  SECTION("signed overflow") {
    RegShiftTest<Opcode::RSC> signed_overflow(true, RN, RD, 0x80000000, 0, CpuState::V_FLAG | CpuState::N_FLAG, RS, 2, BitShift::LEFT, RM, 2);
    signed_overflow.test(state);
  }

  // Test borrow from
  SECTION("unsigned overflow") {
    RegShiftTest<Opcode::RSC> test_no_borrow(true, RN, RD, 0x7FFFFFFF, 0, CpuState::N_FLAG, RS, 2, BitShift::LEFT, RM, 2);
    test_no_borrow.test(state);
  }

  SECTION("not unsigned overflow") {
    RegShiftTest<Opcode::RSC> test_borrow_not(true, RN, RD, 4, 0, CpuState::C_FLAG, RS, 2, BitShift::LEFT, RM, 2);
    test_borrow_not.test(state);
  }
}

TEST_CASE("data_processing::RSC (imm shift)") {
  no_flag_tests_imm_shift<Opcode::RSC>();
}

TEST_CASE("data_processing::RSC (imm rot)") {
  no_flag_tests_imm_rotate<Opcode::RSC>();
}

template <>
gword_t Mock<Opcode::SBC>::expected_value(DataProcessingTest<Opcode::SBC> &d, CpuState &state) {
  return state.get_register(d.irn) - d.calculate_operand(state) - !bool(d.input_flags & CpuState::C_FLAG);
}

TEST_CASE("data_processing::SBC (reg shift)") {
  const gword_t RN = 0;
  const gword_t RD = 1;
  const gword_t RM = 2;
  const gword_t RS = 3;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;

  no_flag_tests_reg_shift<Opcode::SBC>();

  // Test signed overflow
  SECTION("signed overflow") {
    RegShiftTest<Opcode::SBC> signed_overflow(
      true, RN, RD, 0x7FFFFFFF, CpuState::C_FLAG, CpuState::V_FLAG | CpuState::N_FLAG,
      RS, 0, BitShift::LEFT, RM, -1
    );
    signed_overflow.test(state);
  }

  SECTION("unsigned overflow") {
    RegShiftTest<Opcode::SBC> test_no_borrow(true, RN, RD, 0, 0, CpuState::N_FLAG, RS, 0, BitShift::LEFT, RM, 0);
    test_no_borrow.test(state);
  }

  SECTION("not unsigned overflow") {
    RegShiftTest<Opcode::SBC> test_borrow_not(true, RN, RD, 16, 0, CpuState::C_FLAG, RS, 1, BitShift::LEFT, RM, 1);
    test_borrow_not.test(state);
  }
}

TEST_CASE("data_processing::SBC (imm shift)") {
  no_flag_tests_imm_shift<Opcode::SBC>();
}

TEST_CASE("data_processing::SBC (imm rot)") {
  no_flag_tests_imm_rotate<Opcode::SBC>();
}

template <>
gword_t Mock<Opcode::SUB>::expected_value(DataProcessingTest<Opcode::SUB> &d, CpuState &state) {
  return state.get_register(d.irn) - d.calculate_operand(state);
}

TEST_CASE("data_processing::SUB (reg shift)") {
  const gword_t RN = 0;
  const gword_t RD = 1;
  const gword_t RM = 2;
  const gword_t RS = 3;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;

  no_flag_tests_reg_shift<Opcode::SUB>();

  // Test signed overflow
  SECTION("V flag") {
    RegShiftTest<Opcode::SUB> signed_overflow(
      true, RN, RD, 0x7FFFFFFF, CpuState::C_FLAG, CpuState::V_FLAG | CpuState::N_FLAG,
      RS, 0, BitShift::LEFT, RM, -1
    );
    signed_overflow.test(state);
  }

  // Test borrow from
  SECTION("N flag") {
    RegShiftTest<Opcode::SUB> test_no_borrow(true, RN, RD, 0, 0, CpuState::N_FLAG, RS, 1, BitShift::LEFT, RM, 1);
    test_no_borrow.test(state);
  }

  SECTION("C flag") {
    RegShiftTest<Opcode::SUB> test_borrow_not(true, RN, RD, 16, 0, CpuState::C_FLAG, RS, 1, BitShift::LEFT, RM, 1);
    test_borrow_not.test(state);
  }
}

TEST_CASE("data_processing::SUB (imm shift)") {
  no_flag_tests_imm_shift<Opcode::SUB>();
}

TEST_CASE("data_processing::SUB (imm rot)") {
  no_flag_tests_imm_rotate<Opcode::SUB>();
}

template <>
gword_t Mock<Opcode::CMN>::expected_value(DataProcessingTest<Opcode::CMN> &d, CpuState &state) {
  return state.get_register(d.ird);
}

TEST_CASE("data_processing::CMN (reg shift)") {
  const gword_t RN = 0;
  const gword_t RD = 1;
  const gword_t RM = 2;
  const gword_t RS = 3;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;

  // Test zero
  SECTION("Z flag") {
    RegShiftTest<Opcode::CMN> zero_test(true, RN, RD, 0, 0, CpuState::Z_FLAG, RS, 0, BitShift::LEFT, RM, 0);
    zero_test.test(state);
  }

  // Test negative 
  SECTION("N flag") {
    RegShiftTest<Opcode::CMN> negative_test(true, RN, RD, 0, 0, CpuState::N_FLAG, RS, 0, BitShift::LEFT, RM, -1);
    negative_test.test(state);
  }

  // Test carry 
  SECTION("C flag") {
    RegShiftTest<Opcode::CMN> carry_test(true, RN, RD, 0xFFFFFFFF, 0, CpuState::C_FLAG, RS, 0, BitShift::LEFT, RM, 2);
    carry_test.test(state);
  }

  // Test overflow
  SECTION("V flag") {
    RegShiftTest<Opcode::CMN> overflow_test(true, RN, RD, 0x7FFFFFFF, 0, CpuState::N_FLAG | CpuState::V_FLAG, RS, 0, BitShift::LEFT, RM, 1);
    overflow_test.test(state);
  }
}

template <>
gword_t Mock<Opcode::CMP>::expected_value(DataProcessingTest<Opcode::CMP> &d, CpuState &state) {
  return state.get_register(d.ird);
}

TEST_CASE("data_processing::CMP (reg shift)") {
  const gword_t RN = 0;
  const gword_t RD = 1;
  const gword_t RM = 2;
  const gword_t RS = 3;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;

  // Test zero
  SECTION("Z flag") {
    RegShiftTest<Opcode::CMP> zero_test(true, RN, RD, 1, 0, CpuState::C_FLAG | CpuState::Z_FLAG, RS, 0, BitShift::LEFT, RM, 1);
    zero_test.test(state);
  }

  // Test negative 
  SECTION("N flag") {
    RegShiftTest<Opcode::CMP> negative_test(true, RN, RD, 0, 0, CpuState::N_FLAG, RS, 0, BitShift::LEFT, RM, 1);
    negative_test.test(state);
  }

  // Test carry not
  SECTION("C flag") {
    RegShiftTest<Opcode::CMP> carry_not_test(true, RN, RD, 4, 0, CpuState::C_FLAG, RS, 0, BitShift::LEFT, RM, 2);
    carry_not_test.test(state);
  }

  SECTION("not C flag") {
    RegShiftTest<Opcode::CMP> carry_test(true, RN, RD, 4, 0, CpuState::N_FLAG, RS, 0, BitShift::LEFT, RM, 5);
    carry_test.test(state);
  }

  // Test overflow
  SECTION("V flag") {
    RegShiftTest<Opcode::CMP> overflow_test(true, RN, RD, 0x7FFFFFFF, 0, CpuState::N_FLAG | CpuState::V_FLAG, RS, 0, BitShift::LEFT, RM, -1);
    overflow_test.test(state);
  }
}

template <>
gword_t Mock<Opcode::TEQ>::expected_value(DataProcessingTest<Opcode::TEQ> &d, CpuState &state) {
  return state.get_register(d.ird);
}

TEST_CASE("data_processing::TEQ (reg shift)") {
  const gword_t RN = 0;
  const gword_t RD = 1;
  const gword_t RM = 2;
  const gword_t RS = 3;

  SimpleMemory memory;
  ArmCpuState arm_state(memory);
  CpuState &state = arm_state;

  // Test zero
  SECTION("Z flag") {
    RegShiftTest<Opcode::TEQ> zero_test(true, RN, RD, 0xDEAD, 0, CpuState::Z_FLAG, RS, 0, BitShift::LEFT, RM, 0xDEAD);
    zero_test.test(state);
  }

  // Test negative 
  SECTION("N flag") {
    RegShiftTest<Opcode::TEQ> negative_test(true, RN, RD, 0x80000000, 0, CpuState::N_FLAG, RS, 0, BitShift::LEFT, RM, 0x7FFFFFFF);
    negative_test.test(state);
  }

  // Test carry
  SECTION("C flag") {
    RegShiftTest<Opcode::TEQ> carry_test(true, RN, RD, 0, 0, CpuState::C_FLAG | CpuState::N_FLAG, RS, 1, BitShift::LEFT, RM, 0xFFFFFFFF);
    carry_test.test(state);
  }
}
