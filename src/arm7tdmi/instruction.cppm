export module arm7tdmi.instruction;

export import bitutil;
export import types;

export import arm7tdmi.cpu_state;
export import arm7tdmi.instruction_definition;

export {
  ;

  struct Ins {
    Nibbles nibbles;

    Ins(gword_t instruction) : nibbles(instruction) {}

    virtual void execute(CpuState &cpu_state) {}
  };
}
