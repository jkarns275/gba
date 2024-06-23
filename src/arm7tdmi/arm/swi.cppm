export module arm7tdmi.arm.swi;

import arm7tdmi.instruction;

export {
  ;
// Undefined in ARMv4
// struct SWBreak : public Ins {
//   static inline const InstructionDefinition *definition = new InstructionDefinition({
//     new CondPiece(), new ValuePiece(0b00010010, 8), new IntegralPiece(12, "imm_hi", 4), new ValuePiece(0b0111, 4), new IntegralPiece(4, "imm_lo")
//   });
// 
//   gshort_t immed;
// 
//   SWBreak(gword_t instruction) 
//     : Ins(instruction), 
//       immed(nibbles[0] | ((instruction >> 8) & ((1 << 12) - 1))) { }
// };

struct SoftwareInterrupt : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b1111, 4), new IntegralPiece(24, "swi_number")
  });

  gword_t swi_number;

  SoftwareInterrupt(gword_t instruction) : Ins(instruction), swi_number(instruction & 0xFFFFFF) { }

  void execute(CpuState &state) override {
    state.get_register(14, Mode::SVC) = state.get_pc() + 4;
    state.get_spsr(Mode::SVC) = state.get_cpsr();
    state.set_mode(Mode::SVC);
    state.get_cpsr() &= ~CpuState::T_FLAG;
    state.get_cpsr() |= CpuState::F_FLAG;

    state.get_pc() = 0x00000008;
  }
};

}
