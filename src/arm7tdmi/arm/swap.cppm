export module arm7tdmi.arm.swap;

import arm7tdmi.instruction;

export {
  ;

struct SingleDataSwap : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b00010, 5), new BoolPiece("B"), new Zeros(2), new IntegralPiece(4, "Rn"),
    new IntegralPiece(4, "Rd"), new Zeros(4), new ValuePiece(0b1001, 4), new IntegralPiece(4, "Rm")
  });

  static constexpr gword_t MASK_B = flag_mask(22);

  bool b;
  byte irn, ird, irm;

  SingleDataSwap(gword_t instruction) 
    : Ins(instruction),
      b(MASK_B & instruction),
      irn(nibbles[4]),
      ird(nibbles[3]),
      irm(nibbles[0]) { }

  void execute(CpuState &state) override {
    gword_t rd,
            rn = state.read_register(irn),
            rm = state.read_register(irm);
    if (b) {
      byte temp = state.byte_at(rn);
      state.byte_at(rn) = rm;
      rd = temp;
    } else {
      gword_t temp = state.rotated_at(rn);
      state.at(rn) = rm;
      rd = temp;
    }
    state.write_register(ird, rd);
  }
};

}
