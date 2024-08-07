module;
#include <iostream>

export module arm7tdmi.arm.mul;

import arm7tdmi.instruction;

export {
  ;

struct MulShort : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new Zeros(6), new BoolPiece("A"), new BoolPiece("S"), new IntegralPiece(4, "Rd"),
    new IntegralPiece(4, "Rn"), new IntegralPiece(4, "Rs"), new ValuePiece(0b1001, 4), new IntegralPiece(4, "Rm")
  });

  static constexpr gword_t MASK_A = flag_mask(21);
  static constexpr gword_t MASK_S = flag_mask(20);

  bool a, s;
  byte ird, irn, irs, irm;

  MulShort(gword_t instruction) 
    : Ins(instruction),
      a(MASK_A & instruction),
      s(MASK_S & instruction),
      ird(nibbles[4]),
      irn(nibbles[3]),
      irs(nibbles[2]),
      irm(nibbles[0]) { }
  
  MulShort(gword_t instruction, bool a, bool s, byte ird, byte irn, byte irs, byte irm)
    : Ins(instruction),
      a(a),
      s(s),
      ird(ird),
      irn(irn),
      irs(irs),
      irm(irm) {}

  void execute(CpuState &state) override {
    gword_t rn = state.read_register(irn),
            rs = state.read_register(irs),
            rm = state.read_register(irm);

    gword_t value;

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
};

struct MulLong : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new Zeros(4), new Ones(1), new BoolPiece("U"), new BoolPiece("A"), new BoolPiece("S"),
    new IntegralPiece(4, "RdHi"), new IntegralPiece(4, "RdLo"), new RegPiece("Rs"), new ValuePiece(0b1001, 4), new IntegralPiece(4, "Rm")
  });

  static inline const gword_t MASK_U = flag_mask(22);
  static inline const gword_t MASK_A = flag_mask(21);
  static inline const gword_t MASK_S = flag_mask(20);

  bool u, a, s;
  byte ird_msw, ird_lsw, irs, irm;
 
  MulLong(gword_t instruction) 
    : Ins(instruction), 
      u(MASK_U & instruction),
      a(MASK_A & instruction),
      s(MASK_S & instruction),
      ird_msw(nibbles[4]),
      ird_lsw(nibbles[3]),
      irs(nibbles[2]),
      irm(nibbles[0]) { }
  
  void execute(CpuState &state) override {
    gword_t rd_lo   = state.read_register(ird_lsw),
            rd_hi   = state.read_register(ird_msw),
            rs      = state.read_register(irs),
            rm      = state.read_register(irm);

    if (u) {
      // Unsigned
      glong_t  rm_long = rm,
               rs_long = rs,
               rd_long;

      if (a) {
        rd_long = rd_lo | ((glong_t) rd_hi) << 32;
      } else {
        rd_long = 0;
      }
      
      rd_long = rm_long * rs_long + rd_long;

      rd_lo = rd_long & 0xFFFFFFFF;
      rd_hi = rd_long >> 32;
    } else {
      // Signed
      signed_glong_t  rm_long = rm,
                      rs_long = rs,
                      rd_long;

      if (a) {
        rd_long = rd_lo | ((signed_glong_t) rd_hi) << 32;
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
};

}
