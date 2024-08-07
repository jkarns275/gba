module;
#include <iostream>
#include <spdlog/spdlog.h>

export module arm7tdmi.arm.branch;

import arm7tdmi.instruction;

export {
  ;

struct BranchExchange : public Ins {
  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b00010010, 8), new Ones(12), new Zeros(2), new BoolPiece("lr"), new Ones(1), new RegPiece("Rm")
  });

  static constexpr gword_t MASK_LR = flag_mask(5);
  static constexpr gword_t MASK_T = 0b1;

  bool set_lr;
  gword_t irm;

  BranchExchange(gword_t instruction)
    : Ins(instruction),
      set_lr(instruction & MASK_LR),
      irm(nibbles[0]) {}

  BranchExchange(gword_t instruction, bool set_lr, byte irm)
    : Ins(instruction),
      set_lr(set_lr),
      irm(irm) {}

  void execute(CpuState &state) override {
    if (set_lr) {
      if (state.is_thumb_mode())
        state.write_lr(state.read_current_pc() + 2);
      else
        state.write_lr(state.read_current_pc() + 4);
    }

    gword_t rm = state.read_register(irm);

    state.write_pc(rm & ~1);
    
    if (rm & 1)
      state.set_flag(CpuState::T_FLAG);
    else
      state.clear_flag(CpuState::T_FLAG);
  }
};

struct BranchWithLink : public Ins {

  static inline const InstructionDefinition *definition = new InstructionDefinition({
    new CondPiece(), new ValuePiece(0b101, 3), new BoolPiece("L"), new IntegralPiece(24, "offset")
  });

  static constexpr gword_t MASK_L = flag_mask(24);
  static constexpr gword_t MASK_OFFSET = 0xFFFFFF;
  static constexpr gword_t MASK_OFFSET_SIGN = flag_mask(23);

  bool l;
  bool exchange;

  // 24 bit-signed integer in the instruction. Needs to be sign extended for proper use.
  signed_gword_t offset;

  BranchWithLink(gword_t instruction) : Ins(instruction), l(instruction & MASK_L), exchange(nibbles[7] == 0xF), offset(instruction & MASK_OFFSET) {
    if (offset & MASK_OFFSET_SIGN) {
      offset |= 0xFF000000;
    }

    offset <<= 2;
    if (exchange)
      offset += gword_t(l) << 1;
  }

  BranchWithLink(gword_t instruction, bool l, bool exchange, signed_gword_t offset)
    : Ins(instruction),
      l(l),
      exchange(exchange),
      offset(offset) {}

  void execute(CpuState &state) override {
    if (exchange || l) {
      gword_t lr = state.read_current_pc();
      if (state.is_thumb_mode()) {
        lr += 2;
      } else {
        lr += 4;
      }
      state.write_lr(lr);
    }

    state.write_pc(state.read_pc() + offset);
    
    if (exchange)
      state.set_flag(CpuState::T_FLAG);
  }
};

}
