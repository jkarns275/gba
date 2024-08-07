module;
#include <iostream>
#include <string>
#include <unordered_map>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_all.hpp>

export module test.arm7tdmi.thumb.data_processing;

import arm7tdmi.arm;
import arm7tdmi.thumb;
import arm7tdmi.instruction;

import test.arm7tdmi.test_utils;

template <ArmInstructionType I>
struct TDataProcessingTest : public ArmInstructionTestWithFlags<I> {
  u8 ird;

  TDataProcessingTest(u8 ird, u32 input_flags, u32 output_flags)
      : ArmInstructionTestWithFlags<I>(input_flags, output_flags), ird(ird) {}

  void prepare_state(CpuState &state) override {
    ArmInstructionTestWithFlags<I>::prepare_state(state);

    this->value_map["Rd"] = ird;
  }

  void check_requirements(CpuState &state) override {
    REQUIRE(state.read_register(ird) == expected_value(state));

    ArmInstructionTestWithFlags<I>::check_requirements(state);
  }

  virtual u32 expected_value(CpuState &state) = 0;
};

enum AddSubOpcode {
  ADD = 0,
  SUB = 1,
};

template <u8 opcode>
struct TAddSubRegTest : public TDataProcessingTest<TAddSubReg> {
  u8 irm, irn;
  u32 rm, rn;

  TAddSubRegTest(u8 irm, u32 rm, u8 irn, u32 rn, u8 ird, u32 input_flags,
                 u32 output_flags)
      : TDataProcessingTest<TAddSubReg>(ird, input_flags, output_flags),
        irm(irm), irn(irn), rm(rm), rn(rn) {}

  const InstructionDefinition &get_definition() override {
    return *TAddSubReg::definition;
  }

  void prepare_state(CpuState &state) override {
    TDataProcessingTest<TAddSubReg>::prepare_state(state);

    value_map["opcode"] = opcode;
    value_map["Rm"] = irm;
    value_map["Rn"] = irn;

    state.write_register(irn, rn);
    state.write_register(irm, rm);
  }

  u32 expected_value(CpuState &state) override {
    switch ((AddSubOpcode)opcode) {
    case AddSubOpcode::ADD:
      return rm + rn;
    case AddSubOpcode::SUB:
      return rn - rm;
    }
  }
};

template <u8 opcode>
struct TAddSubImmTest : public TDataProcessingTest<TAddSubImm> {
  u8 imm, irn;
  u32 rn;

  TAddSubImmTest(u8 imm, u8 irn, u32 rn, u8 ird, u32 input_flags,
                 u32 output_flags)
      : TDataProcessingTest<TAddSubImm>(ird, input_flags, output_flags),
        imm(imm), irn(irn), rn(rn) {}

  const InstructionDefinition &get_definition() override {
    return *TAddSubImm::definition;
  }

  void prepare_state(CpuState &state) override {
    TDataProcessingTest<TAddSubImm>::prepare_state(state);

    value_map["imm3"] = imm & 0b111;
    value_map["opcode"] = opcode;
    value_map["Rn"] = irn;

    state.write_register(irn, rn);
  }

  u32 expected_value(CpuState &state) override {
    switch ((AddSubOpcode)opcode) {
    case AddSubOpcode::ADD:
      return rn + imm;
    case AddSubOpcode::SUB:
      return rn - imm;
    }
  }
};

TEST_CASE("THUMB SUB (1)") {
  auto ird = 0;
  auto irn = 1;

  SECTION("no flags") {
    auto rn = GENERATE(take(10, random<u32>(0, 0x7FFFFFFF)));
    auto imm = GENERATE(take(10, random<u32>(0, 7)));

    TAddSubImmTest<AddSubOpcode::SUB> test(imm, irn, rn, ird, 0,
                                           CpuState::C_FLAG);
    test.test();
  }

  SECTION("N flag") {
    TAddSubImmTest<AddSubOpcode::SUB> test(1, irn, -4, ird, -100,
                                           CpuState::C_FLAG | CpuState::N_FLAG);
    test.test();
  }

  SECTION("C flag") {
    TAddSubImmTest<AddSubOpcode::SUB> test(1, irn, 0x0, ird, 0,
                                           CpuState::N_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB ADD (1)") {
  auto ird = 0;
  auto irn = 1;

  SECTION("no flags") {
    auto rn = GENERATE(take(10, random<u32>(0, 0x7FFFFFFF - 16)));
    auto imm = GENERATE(take(10, random<u32>(0, (1 << 3) - 1)));

    TAddSubImmTest<AddSubOpcode::ADD> test(imm, irn, rn, ird, 0, 0);
    test.test();
  }

  SECTION("N flag") {
    TAddSubImmTest<AddSubOpcode::ADD> test(1, irn, -4, ird, 0,
                                           CpuState::N_FLAG);
    test.test();
  }

  SECTION("V flag") {
    TAddSubImmTest<AddSubOpcode::ADD> test(1, irn, 0x7FFFFFFF, ird, 0,
                                           CpuState::N_FLAG | CpuState::V_FLAG);
    test.test();
  }

  SECTION("C flag") {
    TAddSubImmTest<AddSubOpcode::ADD> test(2, irn, 0xFFFFFFFF, ird, 0,
                                           CpuState::C_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB ADD (3)") {
  auto ird = 0;
  auto irm = 1;
  auto irn = 2;

  SECTION("no flags") {
    auto rm = GENERATE(take(10, random<u32>(0, 10000)));
    auto rn = GENERATE(take(10, random<u32>(0, 10000)));

    if (rm != rn) {
      TAddSubRegTest<AddSubOpcode::ADD> test(irm, rm, irn, rn, ird, 0, 0);
      test.test();
    }
  }

  SECTION("N flag") {
    TAddSubRegTest<AddSubOpcode::ADD> test(irm, -1, irn, 0, ird, 0,
                                           CpuState::N_FLAG);
    test.test();
  }

  SECTION("V flag") {
    TAddSubRegTest<AddSubOpcode::ADD> test(irm, 0x7FFFFFFF, irn, 2, ird, 0,
                                           CpuState::V_FLAG | CpuState::N_FLAG);
    test.test();
  }

  SECTION("C flag") {
    TAddSubRegTest<AddSubOpcode::ADD> test(irm, 0xFFFFFFFF, irn, 2, ird, 0,
                                           CpuState::C_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB SUB (3)") {
  auto ird = 0;
  auto irm = 1;
  auto irn = 2;

  SECTION("no flag") {
    auto rm = GENERATE(take(100, random<u32>(0, 100)));
    auto rn = GENERATE(take(100, random<u32>(0, 100)));

    if (rm != rn) {
      auto output_flags = rn > rm ? CpuState::C_FLAG : CpuState::N_FLAG;
      TAddSubRegTest<AddSubOpcode::SUB> test(irm, rm, irn, rn, ird, 0,
                                             output_flags);
      test.test();
    }
  }

  SECTION("N flag") {
    TAddSubRegTest<AddSubOpcode::SUB> test(irm, -1, irn, -2, ird, 0,
                                           CpuState::N_FLAG);
    test.test();
  }

  SECTION("C flag") {
    TAddSubRegTest<AddSubOpcode::SUB> test(irm, 11, irn, 10, ird, 0,
                                           CpuState::N_FLAG);
    test.test();
  }

  SECTION("V flag") {
    TAddSubRegTest<AddSubOpcode::SUB> test(irm, 1, irn, 0x80000000, ird, 0,
                                           CpuState::C_FLAG | CpuState::V_FLAG);
    test.test();
  }
}

enum ImmOpcode {
  IMM_MOV = 0,
  IMM_CMP = 1,
  IMM_ADD = 2,
  IMM_SUB = 3,
};

template <u8 opcode>
struct TDataProcessingImmTest : public TDataProcessingTest<TDataProcessingImm> {
  u8 ir, imm;
  u32 r;

  TDataProcessingImmTest(u8 ir, u32 r, u8 imm, u32 input_flags,
                         u32 output_flags)
      : TDataProcessingTest<TDataProcessingImm>(ir, input_flags, output_flags),
        ir(ir), imm(imm), r(r) {}

  const InstructionDefinition &get_definition() override {
    return *TDataProcessingImm::definition;
  }

  void prepare_state(CpuState &state) override {
    TDataProcessingTest<TDataProcessingImm>::prepare_state(state);

    value_map["R"] = ir;
    value_map["imm8"] = imm;
    value_map["opcode"] = opcode;

    state.write_register(ir, r);
  }

  void check_requirements(CpuState &state) override {
    if (opcode != ImmOpcode::IMM_CMP) {
      REQUIRE(state.read_register(ir) == expected_value(state));
    }

    ArmInstructionTestWithFlags<TDataProcessingImm>::check_requirements(state);
  }

  u32 expected_value(CpuState &state) override {
    switch ((ImmOpcode)opcode) {
    case ImmOpcode::IMM_MOV:
      return imm;
    case ImmOpcode::IMM_CMP:
      return r - imm;
    case ImmOpcode::IMM_ADD:
      return r + imm;
    case ImmOpcode::IMM_SUB:
      return r - imm;
    }
  }
};

TEST_CASE("THUMB ADD (2)") {
  auto ird = 0;

  SECTION("no flag") {
    auto rd = GENERATE(take(100, random<u32>(0, 0x7FFFFFFF - 0x100)));
    auto imm = GENERATE(take(100, random<u32>(0, 255)));
    TDataProcessingImmTest<ImmOpcode::IMM_ADD> test(ird, rd, imm, 0, 0);
    test.test();
  }

  SECTION("N flag") {
    TDataProcessingImmTest<ImmOpcode::IMM_ADD> test(ird, 0x80000000, 1, 0,
                                                    CpuState::N_FLAG);
    test.test();
  }

  SECTION("V flag") {
    TDataProcessingImmTest<ImmOpcode::IMM_ADD> test(
        ird, 0x7FFFFFFF, 1, 0, CpuState::N_FLAG | CpuState::V_FLAG);
    test.test();
  }

  SECTION("C flag") {
    TDataProcessingImmTest<ImmOpcode::IMM_ADD> test(ird, 0xFFFFFFFF, 2, 0,
                                                    CpuState::C_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB SUB (2)") {
  auto ird = 0;

  SECTION("no flag") {
    auto rd = GENERATE(take(100, random<u32>(255, 0x7FFFFFFF - 0x100)));
    auto imm = GENERATE(take(100, random<u32>(0, 255)));
    TDataProcessingImmTest<ImmOpcode::IMM_SUB> test(ird, rd, imm, 0,
                                                    CpuState::C_FLAG);
    test.test();
  }

  SECTION("N flag") {
    TDataProcessingImmTest<ImmOpcode::IMM_SUB> test(ird, 0, 1, 0,
                                                    CpuState::N_FLAG);
    test.test();
  }

  SECTION("V flag") {
    TDataProcessingImmTest<ImmOpcode::IMM_SUB> test(
        ird, 0x80000000, 1, 0, CpuState::V_FLAG | CpuState::C_FLAG);
    test.test();
  }

  SECTION("C flag") {
    TDataProcessingImmTest<ImmOpcode::IMM_SUB> test(ird, 0, 1, 0,
                                                    CpuState::N_FLAG);
    test.test();
    TDataProcessingImmTest<ImmOpcode::IMM_SUB> test2(
        ird, 1, 1, 0, CpuState::C_FLAG | CpuState::Z_FLAG);
    test2.test();
  }
}

TEST_CASE("THUMB MOV (1)") {
  auto ird = 0;

  SECTION("no flag") {
    auto imm = GENERATE(take(100, random<u32>(1, 255)));
    TDataProcessingImmTest<ImmOpcode::IMM_MOV> test(ird, 0, imm, 0, 0);
    test.test();
  }

  SECTION("Z flag") {
    TDataProcessingImmTest<ImmOpcode::IMM_MOV> test(ird, 0, 0, 0,
                                                    CpuState::Z_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB CMP (1)") {
  auto ird = 0;

  SECTION("no flag") {
    auto rd = 256;
    auto imm = GENERATE(take(100, random<u32>(0, 255)));
    TDataProcessingImmTest<ImmOpcode::IMM_CMP> test(ird, rd, imm, 0,
                                                    CpuState::C_FLAG);
    test.test();
  }

  SECTION("C flag") {
    TDataProcessingImmTest<ImmOpcode::IMM_CMP> test(ird, 0, 1, 0,
                                                    CpuState::N_FLAG);
    test.test();
  }

  SECTION("Z flag") {
    TDataProcessingImmTest<ImmOpcode::IMM_CMP> test(
        ird, 1, 1, 0, CpuState::Z_FLAG | CpuState::C_FLAG);
    test.test();
  }

  SECTION("V flag") {
    TDataProcessingImmTest<ImmOpcode::IMM_CMP> test(
        ird, 0x80000000, 1, 0, CpuState::V_FLAG | CpuState::C_FLAG);
    test.test();
  }
}

template <u8 opcode>
struct TDataProcessingRegTest : public TDataProcessingTest<TDataProcessing> {
  u8 irms, irdn;
  u32 rms, rdn;

  TDataProcessingRegTest(u8 irms, u32 rms, u8 irdn, u32 rdn, u32 input_flags,
                         u32 output_flags)
      : TDataProcessingTest<TDataProcessing>(irdn, input_flags, output_flags),
        irms(irms), irdn(irdn), rms(rms), rdn(rdn) {}

  const InstructionDefinition &get_definition() override {
    return *TDataProcessing::definition;
  }

  void prepare_state(CpuState &state) override {
    TDataProcessingTest<TDataProcessing>::prepare_state(state);

    value_map["Rm/Rs"] = irms;
    value_map["opcode"] = opcode;
    value_map["Rd/Rn"] = irdn;

    state.write_register(irms, rms);
    state.write_register(irdn, rdn);
  }

  void check_requirements(CpuState &state) override {
    switch (opcode) {
    case Opcode::MOV:
      __builtin_unreachable();
      assert(false);
    default:
      REQUIRE(state.read_register(irdn) == expected_value(state));
    case Opcode::TST:
    case Opcode::CMP:
    case Opcode::CMN:
      break;
    }

    ArmInstructionTestWithFlags<TDataProcessing>::check_requirements(state);
  }

  u32 expected_value(CpuState &state) override {
    switch (opcode) {
    // THUMB AND
    case Opcode::AND:
      return rdn & rms;
    case Opcode::EOR:
      return rdn ^ rms;
    case Opcode::ADC: {
      CheckedResult r0 = CheckedResult::add(rdn, rms);
      CheckedResult r1 =
          CheckedResult::add(r0.value, bool(input_flags & CpuState::C_FLAG));
      return r1.value;
    }
    case Opcode::SBC: {
      CheckedResult r0 = CheckedResult::sub(rdn, rms);
      CheckedResult r1 =
          CheckedResult::sub(r0.value, !(input_flags & CpuState::C_FLAG));
      return r1.value;
    }
    case Opcode::ORR:
      return rdn | rms;
    case Opcode::BIC:
      return rdn & ~rms;
    case Opcode::MVN:
      return ~rms;
    // ROR
    case Opcode::RSC: {
      u32 x = ror<u32>(rdn, rms & 0x1F);
      if (x & 0x80000000)
        output_flags |= CpuState::N_FLAG;
      if (!x)
        output_flags |= CpuState::Z_FLAG;
      output_flags |=
          (rdn & flag_mask(rms - 1)) && rms > 0 ? CpuState::C_FLAG : 0;
      return x;
    }
    // LSR
    case Opcode::RSB: {
      u32 x = lsr<u32>(rdn, rms & 0xFF);
      if (rms == 0) {
        output_flags |= CpuState::C_FLAG & input_flags;
      } else if (rms < 32) {
        output_flags |= rdn & flag_mask(rms - 1) ? CpuState::C_FLAG : 0;
      } else if (rms == 32) {
        output_flags |= rdn & 0x80000000 ? CpuState::C_FLAG : 0;
        x = 0;
      } else {
        x = 0;
      }

      if (x & 0x80000000)
        output_flags |= CpuState::N_FLAG;
      if (!x)
        output_flags |= CpuState::Z_FLAG;

      return x;
    }
    // LSL
    case Opcode::SUB: {
      u32 shift = rms & 0xFF;
      u32 x = lsl<u32>(rdn, shift);
      if (shift == 0) {
        output_flags |= CpuState::C_FLAG & input_flags;
        x = rdn;
      } else if (shift < 32) {
        output_flags |= rdn & flag_mask(32 - shift) ? CpuState::C_FLAG : 0;
      } else {
        x = 0;
        if (shift == 32 && rdn & 1)
          output_flags |= CpuState::C_FLAG;
      }

      if (x & 0x80000000)
        output_flags |= CpuState::N_FLAG;
      if (!x)
        output_flags |= CpuState::Z_FLAG;

      return x;
    }
    // ASR
    case Opcode::ADD: {
      u32 x = asr<i32>(rdn, rms & 0xFF);
      if (rms == 0) {
        output_flags |= CpuState::C_FLAG & input_flags;
      } else if (rms < 32) {
        output_flags |= rdn & flag_mask(rms - 1) ? CpuState::C_FLAG : 0;
      } else if (rms >= 32) {
        output_flags |= rdn & 0x80000000 ? CpuState::C_FLAG : 0;
        if (rdn & 0x80000000) {
          x = -1;
        } else {
          x = 0;
        }
      } else {
        x = 0;
      }

      if (x & 0x80000000)
        output_flags |= CpuState::N_FLAG;
      if (!x)
        output_flags |= CpuState::Z_FLAG;

      return x;
    }

    case Opcode::MOV: // Undefined / not applicable
    case Opcode::TST:
    case Opcode::TEQ:
    case Opcode::CMP:
    case Opcode::CMN:
    default:
      __builtin_unreachable();
      assert(false);
    }
  }
};

TEST_CASE("THUMB AND") {
  auto irm = 0;
  auto ird = 1;

  SECTION("no flag") {
    auto rd = GENERATE(take(100, random<u32>(0, 0x7FFFFFFF))) | 1;
    auto rm = GENERATE(take(100, random<u32>(0, 0x7FFFFFFF))) | 1;

    TDataProcessingRegTest<Opcode::AND> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }

  SECTION("Z flag") {
    TDataProcessingRegTest<Opcode::AND> test(irm, 1, ird, 2, 0,
                                             CpuState::Z_FLAG);
    test.test();
  }

  SECTION("N flag") {
    TDataProcessingRegTest<Opcode::AND> test(irm, 0x80000000, ird, 0xFFFFFFFF,
                                             0, CpuState::N_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB EOR") {
  auto irm = 0;
  auto ird = 1;

  SECTION("no flag") {
    auto rd = GENERATE(take(100, random<u32>(0, 0x7FFFFFFF))) | 1;
    auto rm = GENERATE(take(100, random<u32>(0, 0x7FFFFFF0)));

    TDataProcessingRegTest<Opcode::EOR> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }

  SECTION("Z flag") {
    TDataProcessingRegTest<Opcode::EOR> test(irm, 1, ird, 1, 0,
                                             CpuState::Z_FLAG);
    test.test();
  }

  SECTION("N flag") {
    TDataProcessingRegTest<Opcode::EOR> test(irm, 0x80000000, ird, 0, 0,
                                             CpuState::N_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB ADC") {
  auto irm = 0;
  auto ird = 1;

  SECTION("no flag") {
    auto rd = GENERATE(take(100, random<u32>(0, 0xFFFFFF))) | 1;
    auto rm = GENERATE(take(100, random<u32>(0, 0xFFFFFF)));

    TDataProcessingRegTest<Opcode::ADC> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }

  SECTION("Z flag") {
    TDataProcessingRegTest<Opcode::ADC> test(irm, 0, ird, 0, 0,
                                             CpuState::Z_FLAG);
    test.test();
  }

  SECTION("C flag") {
    TDataProcessingRegTest<Opcode::ADC> test(irm, 0xFFFFFFFF, ird, 2, 0,
                                             CpuState::C_FLAG);
    test.test();
  }

  SECTION("V flag") {
    TDataProcessingRegTest<Opcode::ADC> test(
        irm, 0x7FFFFFFF, ird, 1, 0, CpuState::V_FLAG | CpuState::N_FLAG);
    test.test();
  }

  SECTION("N flag") {
    TDataProcessingRegTest<Opcode::ADC> test(irm, 0x80000000, ird, 0, 0,
                                             CpuState::N_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB SBC") {
  auto irm = 0;
  auto ird = 1;

  SECTION("no flag") {
    auto rd = 0xFFFFFF;
    auto rm = GENERATE(take(100, random<u32>(0, 0xFFFFFE)));

    TDataProcessingRegTest<Opcode::SBC> test(irm, rm, ird, rd, CpuState::C_FLAG,
                                             CpuState::C_FLAG);
    test.test();
  }

  SECTION("Z flag") {
    TDataProcessingRegTest<Opcode::SBC> test(
        irm, 4, ird, 4, CpuState::C_FLAG, CpuState::Z_FLAG | CpuState::C_FLAG);
    test.test();
  }

  SECTION("C flag") {
    TDataProcessingRegTest<Opcode::SBC> test(irm, 0xFFFFFFFF, ird, 1, 0, 0);
    test.test();
  }

  SECTION("V flag") {
    TDataProcessingRegTest<Opcode::SBC> test(
        irm, -1, ird, 0x7FFFFFFF, CpuState::C_FLAG,
        CpuState::V_FLAG | CpuState::N_FLAG);
    test.test();
  }

  SECTION("N flag") {
    TDataProcessingRegTest<Opcode::SBC> test(
        irm, 0, ird, 0x80000000, CpuState::C_FLAG,
        CpuState::N_FLAG | CpuState::C_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB ORR") {
  auto irm = 0;
  auto ird = 1;

  SECTION("no flag") {
    auto rd = GENERATE(take(100, random<u32>(0, 0x7FFFFFFF))) | 1;
    auto rm = GENERATE(take(100, random<u32>(0, 0x7FFFFFF0)));

    TDataProcessingRegTest<Opcode::ORR> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }

  SECTION("Z flag") {
    TDataProcessingRegTest<Opcode::ORR> test(irm, 0, ird, 0, 0,
                                             CpuState::Z_FLAG);
    test.test();
  }

  SECTION("N flag") {
    TDataProcessingRegTest<Opcode::ORR> test(irm, 0x80000000, ird, 0, 0,
                                             CpuState::N_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB BIC") {
  auto irm = 0;
  auto ird = 1;

  SECTION("no flag") {
    auto rd = GENERATE(take(100, random<u32>(0, 0x7FFFFFFF))) | 1;
    auto rm = GENERATE(take(100, random<u32>(0, 0x0FFFFFFF))) << 1;

    TDataProcessingRegTest<Opcode::BIC> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }

  SECTION("Z flag") {
    TDataProcessingRegTest<Opcode::BIC> test(irm, 1, ird, 1, 0,
                                             CpuState::Z_FLAG);
    test.test();
  }

  SECTION("N flag") {
    TDataProcessingRegTest<Opcode::BIC> test(irm, 0x7FFFFFFF, ird, 0x80000000,
                                             0, CpuState::N_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB MVN") {
  auto irm = 0;
  auto ird = 1;
  auto rd = 0;

  SECTION("no flag") {
    auto rm = GENERATE(take(100, random<u32>(1, 0x7FFFFFFE))) | 0x80000000;
    TDataProcessingRegTest<Opcode::MVN> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }

  SECTION("Z flag") {
    TDataProcessingRegTest<Opcode::MVN> test(irm, 0xFFFFFFFF, ird, 0, 0,
                                             CpuState::Z_FLAG);
    test.test();
  }

  SECTION("N flag") {
    TDataProcessingRegTest<Opcode::MVN> test(irm, 0x7FFFFFFF, ird, 0, 0,
                                             CpuState::N_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB TST") {
  auto irm = 0;
  auto ird = 1;

  SECTION("no flag") {
    TDataProcessingRegTest<Opcode::TST> test(irm, 1, ird, 1, 0, 0);
    test.test();
  }

  SECTION("Z flag") {
    TDataProcessingRegTest<Opcode::TST> test(irm, 0xFFFFFFFF, ird, 0, 0,
                                             CpuState::Z_FLAG);
    test.test();
  }

  SECTION("N flag") {
    TDataProcessingRegTest<Opcode::TST> test(irm, 0xFFFFFFFF, ird, 0x80000000,
                                             0, CpuState::N_FLAG);
    test.test();
  }
}

TEST_CASE("THUMB ROR") {
  constexpr Opcode ROR = Opcode::RSC;
  auto irm = 0;
  auto ird = 1;

  auto rd = GENERATE(take(100, random<u32>(1, 0x7FFFFFFE))) | 0x80000000;
  SECTION("1-31") {
    auto rm = GENERATE(range(1, 30));
    TDataProcessingRegTest<ROR> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }

  SECTION("0") {
    TDataProcessingRegTest<ROR> test(irm, 0, ird, rd, 0, 0);
    test.test();
  }

  SECTION("32") {
    TDataProcessingRegTest<ROR> test(irm, 32, ird, rd, 0, 0);
    test.test();
  }

  SECTION("> 32") {
    auto rm = GENERATE(range(33, 128));
    TDataProcessingRegTest<ROR> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }

  SECTION("-10") {
    TDataProcessingRegTest<ROR> test(irm, -10, ird, rd, 0, 0);
    test.test();
  }
}

TEST_CASE("THUMB LSR") {
  constexpr Opcode LSR = Opcode::RSB;
  auto irm = 0;
  auto ird = 1;

  auto rd = GENERATE(take(100, random<u32>(0, 0xFFFFFFFF)));
  SECTION("1-31") {
    auto rm = GENERATE(range(1, 31));
    TDataProcessingRegTest<LSR> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }

  SECTION("0") {
    TDataProcessingRegTest<LSR> test(irm, 0, ird, rd, 0, 0);
    test.test();
  }

  SECTION("32") {
    TDataProcessingRegTest<LSR> test(irm, 32, ird, rd, 0, 0);
    test.test();
  }

  SECTION("> 32") {
    auto rm = GENERATE(range(33, 128));
    TDataProcessingRegTest<LSR> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }
}

TEST_CASE("THUMB ASR") {
  constexpr Opcode ASR = Opcode::ADD;
  auto irm = 0;
  auto ird = 1;

  auto rd = GENERATE(take(100, random<u32>(0, 0xFFFFFFFF)));
  SECTION("1-31") {
    auto rm = GENERATE(range(1, 31));
    TDataProcessingRegTest<ASR> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }

  SECTION("0") {
    TDataProcessingRegTest<ASR> test(irm, 0, ird, rd, 0, 0);
    test.test();
  }

  SECTION("32") {
    TDataProcessingRegTest<ASR> test(irm, 32, ird, rd, 0, 0);
    test.test();
  }

  SECTION("> 32") {
    auto rm = GENERATE(range(33, 128));
    TDataProcessingRegTest<ASR> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }
}

TEST_CASE("THUMB LSL") {
  constexpr Opcode LSL = Opcode::SUB;
  auto irm = 0;
  auto ird = 1;

  auto rd = GENERATE(take(100, random<u32>(0, 0xFFFFFFFF)));
  SECTION("1-31") {
    auto rm = GENERATE(range(1, 31));
    TDataProcessingRegTest<LSL> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }

  SECTION("0") {
    TDataProcessingRegTest<LSL> test(irm, 0, ird, rd, 0, 0);
    test.test();
  }

  SECTION("32") {
    TDataProcessingRegTest<LSL> test(irm, 32, ird, rd, 0, 0);
    test.test();
  }

  SECTION("> 32") {
    auto rm = GENERATE(range(33, 128));
    TDataProcessingRegTest<LSL> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }
}

struct TMulTest : public ArmInstructionTestWithFlags<TMul> {
  u8 irm, ird;
  u32 rm, rd;

  TMulTest(u8 irm, u32 rm, u8 ird, u32 rd, u32 input_flags, u32 output_flags)
      : ArmInstructionTestWithFlags<TMul>(input_flags, output_flags), irm(irm),
        ird(ird), rm(rm), rd(rd) {}

  const InstructionDefinition &get_definition() override {
    return *TMul::definition;
  }

  void prepare_state(CpuState &state) override {
    ArmInstructionTestWithFlags<TMul>::prepare_state(state);

    this->value_map["Rd"] = ird;
    this->value_map["Rm"] = irm;

    state.write_register(ird, rd);
    state.write_register(irm, rm);
  }

  void check_requirements(CpuState &state) override {
    REQUIRE(state.read_register(ird) == expected_value(state));

    ArmInstructionTestWithFlags<TMul>::check_requirements(state);
  }

  u32 expected_value(CpuState &state) { return rm * rd; }
};

TEST_CASE("THUMB MUL") {
  const u8 irm = 0;
  const u8 ird = 1;

  SECTION("no flags") {
    auto rm = GENERATE(take(100, random<u32>(1, 0xFFF)));
    auto rd = GENERATE(take(10, random<u32>(1, 0xFFF)));
    TMulTest test(irm, rm, ird, rd, 0, 0);
    test.test();
  }

  SECTION("N flag") {
    auto rm = GENERATE(take(100, random<u32>(1, 0x7FFFFFFF)));
    TMulTest test(irm, rm, ird, -1, 0, CpuState::N_FLAG);
    test.test();
  }

  SECTION("Z flag") {
    auto rm = GENERATE(take(100, random<u32>(1, 0xFFFFFFFF)));
    TMulTest test(irm, rm, ird, 0, 0, CpuState::Z_FLAG);
    test.test();
  }
}

enum HiRegOpcode {
  HI_ADD = 0,
  HI_CMP = 1,
  HI_MOV = 2,
};

template <u8 opcode>
struct TDataProcessingHiRegTest
    : public TDataProcessingTest<TDataProcessingHiReg> {
  u8 irm, irdn;
  u32 rm, rdn;

  TDataProcessingHiRegTest(u8 irm, u32 rm, u8 irdn, u32 rdn, u32 input_flags,
                           u32 output_flags)
      : TDataProcessingTest<TDataProcessingHiReg>(irdn, input_flags,
                                                  output_flags),
        irm(irm), irdn(irdn), rm(rm), rdn(rdn) {}

  const InstructionDefinition &get_definition() override {
    return *TDataProcessingHiReg::definition;
  }

  void prepare_state(CpuState &state) override {
    TDataProcessingTest<TDataProcessingHiReg>::prepare_state(state);

    value_map["opcode"] = opcode;
    value_map["H1"] = bool(irdn & 0x8);
    value_map["Rd/Rn"] = irdn;
    value_map["H2"] = bool(irm & 0x8);
    value_map["Rm"] = irm;

    state.write_register(irdn, rdn);
    state.write_register(irm, rm);
  }

  void check_requirements(CpuState &state) override {
    if (opcode != HiRegOpcode::HI_CMP) {
      REQUIRE(state.read_register(irdn) == expected_value(state));
    }

    ArmInstructionTestWithFlags<TDataProcessingHiReg>::check_requirements(
        state);
  }

  u32 expected_value(CpuState &state) override {
    switch ((HiRegOpcode)opcode) {
    case HiRegOpcode::HI_ADD:
      return rdn + rm;
    case HiRegOpcode::HI_MOV:
      return rm;
    default:
    }
    return 0;
  }
};

TEST_CASE("THUMB MOV (3)") {
  const u8 irm = GENERATE(range(0, 15));
  const u8 ird = GENERATE(range(0, 15));

  if (irm != ird) {
    auto rm = GENERATE(take(10, random<u32>(0, 0xFFFFFFFF)));

    TDataProcessingHiRegTest<HI_MOV> test(irm, rm, ird, 0, 0, 0);
    test.test();
  }
}

TEST_CASE("THUMB ADD (4)") {
  const u8 irm = GENERATE(range(0, 15));
  const u8 ird = GENERATE(range(0, 15));

  if (irm != ird) {
    auto rm = GENERATE(take(10, random<u32>(0, 0xFFFFFFFF)));
    auto rd = GENERATE(take(10, random<u32>(0, 0xFFFFFFFF)));

    TDataProcessingHiRegTest<HI_ADD> test(irm, rm, ird, rd, 0, 0);
    test.test();
  }
}

TEST_CASE("THUMB CMP (3)") {
  const u8 irm = GENERATE(range(0, 15));
  const u8 ird = GENERATE(range(0, 15));

  if (irm != ird) {
    SECTION("N flag") {
      TDataProcessingHiRegTest<HI_CMP> test(
          irm, 0, ird, -3, 0, CpuState::N_FLAG | CpuState::C_FLAG);
      test.test();
    }

    SECTION("Z flag") {
      TDataProcessingHiRegTest<HI_CMP> test(
          irm, 1, ird, 1, 0, CpuState::Z_FLAG | CpuState::C_FLAG);
      test.test();
    }

    SECTION("C flag") {
      TDataProcessingHiRegTest<HI_CMP> test(irm, 1, ird, 0, 0,
                                            CpuState::N_FLAG);
      test.test();
    }

    SECTION("V flag") {
      TDataProcessingHiRegTest<HI_CMP> test(
          irm, 1, ird, 0x80000000, 0, CpuState::V_FLAG | CpuState::C_FLAG);
      test.test();
    }
  }
}
