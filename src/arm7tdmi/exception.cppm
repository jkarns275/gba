module;
#include <assert.h>

export module arm7tdmi:exception;

import types;

export {
  ;

  constexpr u32 STATUS_STATE_MASK = 1 << 5;

  class Exception {
    enum EKind { EBL, ESWI, EUDEF, EPABT, EFIQ, EIRQ, EDABT, ERESET };

    EKind kind;

    u32 offset_arm(u32 status_register) {
      assert(kind != ERESET);
      switch (kind) {
      case EBL:
      case ESWI:
      case EUDEF:
        return (status_register & STATUS_STATE_MASK) ? 2 : 4;
      case EPABT:
      case EFIQ:
      case EIRQ:
        return 4;
      case EDABT:
        return 8;
      default:
        __builtin_unreachable();
      }
    }
  };
}
