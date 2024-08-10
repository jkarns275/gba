module;

export module arm7tdmi:mode;

import types;

export {
  ;
  enum Mode : u8 {
    USR = 0b10000,
    FIQ = 0b10001,
    IRQ = 0b10010,
    SVC = 0b10011,
    ABT = 0b10111,
    UND = 0b11011,
    SYS = 0b11111,
  };

  bool mode_is_privileged(Mode mode) { return mode != USR; }

  bool mode_has_spsr(Mode mode) { return mode != USR && mode != SYS; }

  bool mode_is_valid(Mode mode) {
    switch (mode) {
    case USR:
    case FIQ:
    case IRQ:
    case SVC:
    case ABT:
    case UND:
    case SYS:
      return true;
    default:
      return false;
    }
  }
}
