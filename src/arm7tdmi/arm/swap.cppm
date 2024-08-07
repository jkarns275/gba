export module arm7tdmi.arm.swap;

import arm7tdmi.instruction;

export {
  ;

  struct SingleDataSwap : public Ins {
    static inline const InstructionDefinition *definition =
        new InstructionDefinition(
            {new CondPiece(), new ValuePiece(0b00010, 5), new BoolPiece("B"),
             new Zeros(2), new IntegralPiece(4, "Rn"),
             new IntegralPiece(4, "Rd"), new Zeros(4),
             new ValuePiece(0b1001, 4), new IntegralPiece(4, "Rm")});

    static constexpr u32 MASK_B = flag_mask(22);

    bool b;
    u8 irn, ird, irm;

    SingleDataSwap(u32 instruction)
        : Ins(instruction), b(MASK_B & instruction), irn(nibbles[4]),
          ird(nibbles[3]), irm(nibbles[0]) {}

    void execute(CpuState &state) override {
      u32 rd, rn = state.read_register(irn), rm = state.read_register(irm);
      if (b) {
        u8 temp = state.u8_at(rn);
        state.u8_at(rn) = rm;
        rd = temp;
      } else {
        u32 temp = state.rotated_at(rn);
        state.at(rn) = rm;
        rd = temp;
      }
      state.write_register(ird, rd);
    }
  };
}
