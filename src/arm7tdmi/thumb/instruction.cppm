export module arm7tdmi.thumb;

import arm7tdmi.instruction;

export {
  ;

struct ThumbInstruction {
  typedef variant<
    UndefinedThumbInstruction
  > InsAlg;

  InsAlg instruction;

  ThumbInstruction(gword_t instruction) : instruction(UndefinedThumbInstruction(-1)) {
    Nibbles nibbles(instruction);
  }
};
}
