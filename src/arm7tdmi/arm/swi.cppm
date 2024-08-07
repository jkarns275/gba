export module arm7tdmi.arm.swi;

import arm7tdmi.instruction;

export {
  ;
  // Undefined in ARMv4
  // struct SWBreak : public Ins {
  //   static inline const InstructionDefinition *definition = new
  //   InstructionDefinition({
  //     new CondPiece(), new ValuePiece(0b00010010, 8), new IntegralPiece(12,
  //     "imm_hi", 4), new ValuePiece(0b0111, 4), new IntegralPiece(4, "imm_lo")
  //   });
  //
  //   u16 immed;
  //
  //   SWBreak(u32 instruction)
  //     : Ins(instruction),
  //       immed(nibbles[0] | ((instruction >> 8) & ((1 << 12) - 1))) { }
  // };

  struct SoftwareInterrupt : public Ins {
    static inline const InstructionDefinition *definition =
        new InstructionDefinition({new CondPiece(), new ValuePiece(0b1111, 4),
                                   new IntegralPiece(24, "swi_number")});

    u32 swi_number;

    SoftwareInterrupt(u32 instruction)
        : Ins(instruction), swi_number(instruction & 0xFFFFFF) {}
    SoftwareInterrupt(u32 instruction, u32 swi_number)
        : Ins(instruction), swi_number(swi_number) {}

    void execute(CpuState &state) override {
      state.write_spsr(state.read_cpsr(), Mode::SVC);
      state.set_mode(Mode::SVC);
      state.write_register(14, state.read_current_pc() + 4);
      state.clear_flag(CpuState::T_FLAG);
      state.set_flag(CpuState::F_FLAG);

      state.write_pc(0x00000008);
    }
  };
}
