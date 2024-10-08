module;
#include <format>
#include <string>

export module arm7tdmi.arm:mul;

import arm7tdmi;
import arm7tdmi.instruction;

export {
  ;

  struct MulShort : public Ins {
    static inline const InstructionDefinition *definition =
        new InstructionDefinition(
            {new CondPiece(), new Zeros(6), new BoolPiece("A"),
             new BoolPiece("S"), new IntegralPiece(4, "Rd"),
             new IntegralPiece(4, "Rn"), new IntegralPiece(4, "Rs"),
             new ValuePiece(0b1001, 4), new IntegralPiece(4, "Rm")});

    static constexpr u32 MASK_A = flag_mask(21);
    static constexpr u32 MASK_S = flag_mask(20);

    bool a, s;
    u8 ird, irn, irs, irm;

    MulShort(u32 instruction)
        : Ins(instruction), a(MASK_A & instruction), s(MASK_S & instruction),
          ird(nibbles[4]), irn(nibbles[3]), irs(nibbles[2]), irm(nibbles[0]) {}

    MulShort(u32 instruction, bool a, bool s, u8 ird, u8 irn, u8 irs, u8 irm)
        : Ins(instruction), a(a), s(s), ird(ird), irn(irn), irs(irs), irm(irm) {
    }

    void execute(CpuState &state) override {
      u32 rn = state.read_register(irn), rs = state.read_register(irs),
          rm = state.read_register(irm);

      u32 value;

      if (a) {
        value = rm * rs + rn;
      } else {
        value = rm * rs;
      }

      state.write_register(ird, value);

      if (s) {
        state.set_flag(CpuState::N_FLAG & value);
        if (value == 0)
          state.set_flag(CpuState::Z_FLAG);
      }
    }

    std::string disassemble() override {
      return std::format("{}{}{} {}, {}, {}", a ? "MLA" : "MUL",
                         cond_to_string(cond), s ? "S" : "",
                         pretty_reg_name(ird), pretty_reg_name(irm),
                         pretty_reg_name(irs));
    }
  };

  struct MulLong : public Ins {
    static inline const InstructionDefinition *definition =
        new InstructionDefinition(
            {new CondPiece(), new Zeros(4), new Ones(1), new BoolPiece("U"),
             new BoolPiece("A"), new BoolPiece("S"),
             new IntegralPiece(4, "RdHi"), new IntegralPiece(4, "RdLo"),
             new RegPiece("Rs"), new ValuePiece(0b1001, 4),
             new IntegralPiece(4, "Rm")});

    static inline const u32 MASK_U = flag_mask(22);
    static inline const u32 MASK_A = flag_mask(21);
    static inline const u32 MASK_S = flag_mask(20);

    bool u, a, s;
    u8 ird_msw, ird_lsw, irs, irm;

    MulLong(u32 instruction)
        : Ins(instruction), u(MASK_U & instruction), a(MASK_A & instruction),
          s(MASK_S & instruction), ird_msw(nibbles[4]), ird_lsw(nibbles[3]),
          irs(nibbles[2]), irm(nibbles[0]) {}

    void execute(CpuState &state) override {
      u32 rd_lo = state.read_register(ird_lsw),
          rd_hi = state.read_register(ird_msw), rs = state.read_register(irs),
          rm = state.read_register(irm);

      if (u) {
        // Unsigned
        u64 rm_long = rm, rs_long = rs, rd_long;

        if (a) {
          rd_long = rd_lo | ((u64)rd_hi) << 32;
        } else {
          rd_long = 0;
        }

        rd_long = rm_long * rs_long + rd_long;

        rd_lo = rd_long & 0xFFFFFFFF;
        rd_hi = rd_long >> 32;
      } else {
        // Signed
        i64 rm_long = rm, rs_long = rs, rd_long;

        if (a) {
          rd_long = rd_lo | ((i64)rd_hi) << 32;
        } else {
          rd_long = 0;
        }

        rd_long = rm_long * rs_long + rd_long;

        rd_lo = rd_long & 0xFFFFFFFF;
        rd_hi = rd_long >> 32;
      }

      if (s) {
        state.set_flag(CpuState::N_FLAG & rd_hi);
        if (rd_lo == 0 && rd_hi == 0)
          state.set_flag(CpuState::Z_FLAG);
      }

      state.write_register(ird_msw, rd_hi);
      state.write_register(ird_lsw, rd_lo);
    }

    static constexpr u32 switch_case(bool _0, bool _1) {
      return ((u32)_0) | ((u32)_1 << 1);
    }

    std::string disassemble() override {
      std::string name;

      switch (switch_case(u, a)) {
      case switch_case(false, false):
        name = "UMULL";
        break;
      case switch_case(false, true):
        name = "UMULAL";
        break;
      case switch_case(true, false):
        name = "SMULL";
        break;
      case switch_case(true, true):
        name = "SMULAL";
        break;
      }

      return std::format("{}{}{} {}, {}, {}, {}", name, cond_to_string(cond),
                         s ? "S" : "", pretty_reg_name(ird_lsw),
                         pretty_reg_name(ird_msw), pretty_reg_name(irm),
                         pretty_reg_name(irs));
    }
  };
}
