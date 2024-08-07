export module arm7tdmi.arm.clz;

import arm7tdmi.instruction;

export {
  ;

  struct CountLeadingZeros : public Ins {
    static inline const InstructionDefinition *definition =
        new InstructionDefinition(
            {new CondPiece(), new ValuePiece(0b00010110, 8), new Ones(4),
             new RegPiece("Rd"), new Ones(4), new ValuePiece(0b0001, 4),
             new RegPiece("Rm")});

    u8 ird, irn;

    CountLeadingZeros(u32 instruction)
        : Ins(instruction), ird(nibbles[3]), irn(nibbles[0]) {}

    void execute(CpuState &state) override {
      u32 rn = state.read_register(irn);

      state.write_register(ird, count_leading_zeros(rn));
    }
  };
}
