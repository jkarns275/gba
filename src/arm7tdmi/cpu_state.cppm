#ifndef GBA_ARM7TDMI_CPU_STATE
#define GBA_ARM7TDMI_CPU_STATE
module;

#include <assert.h>

#include <iostream>
#include <format>
#include <variant>
#include <vector>

export module arm7tdmi.cpu_state;

import bitutil;
import types;

using std::format;
using std::variant;
using std::vector;

export {

enum Mode : byte {
  USR = 0b10000,
  FIQ = 0b10001,
  IRQ = 0b10010,
  SVC = 0b10011,
  ABT = 0b10111,
  UND = 0b11011,
  SYS = 0b11111,
};

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

constexpr gword_t STATUS_NEGATIVE_MASK = 1 << 31;
constexpr gword_t STATUS_ZERO_MASK = 1 << 30;
constexpr gword_t STATUS_CARRY_MASK = 1 << 29;
constexpr gword_t STATUS_OVERFLOW_MASK = 1 << 28;
constexpr gword_t STATUS_IRQ_DISABLE_MASK = 1 << 7;
constexpr gword_t STATUS_FIQ_DISABLE_MASK = 1 << 6;
constexpr gword_t STATUS_STATE_MASK = 1 << 5;
constexpr gword_t STATUS_MODE_MASK = (1 << 5) - 1;

class Exception {
  enum EKind {
    EBL,
    ESWI,
    EUDEF,
    EPABT,
    EFIQ,
    EIRQ,
    EDABT,
    ERESET
  };

  EKind kind;

  gword_t offset_arm(gword_t status_register) {
    assert (kind != ERESET);
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

struct CpuState {
  static inline constexpr gword_t N_FLAG = flag_mask(31);
  static inline constexpr gword_t Z_FLAG = flag_mask(30);
  static inline constexpr gword_t C_FLAG = flag_mask(29);
  static inline constexpr gword_t V_FLAG = flag_mask(28);

  static inline constexpr gword_t INDEX_PC = 15;
  static inline constexpr gword_t INDEX_LR = 14;
  static inline constexpr gword_t INDEX_SP = 13;

  virtual gword_t &get_register(gword_t index) = 0;
  virtual gword_t &get_cpsr() = 0;
  virtual gword_t &get_spsr() = 0;

  gword_t get_flag(gword_t mask) {
    return bool(get_cpsr() & mask);
  }

  gword_t &get_sp() {
    return get_register(INDEX_SP);
  }

  gword_t &get_lr() {
    return get_register(INDEX_LR);
  }

  gword_t &get_pc() {
    return get_register(INDEX_PC);
  }
  
  void print_registers() {
    for (int i = 0; i < 16; i++) {
      if (i % 4 == 0 && i)
        std::cout << "\n";
      gword_t value = get_register(i);
      std::cout <<  std::format("r{:<2} : 0x{:<8x}", i, value) << "  ";
    }
    std::cout << "\n";
  }

  bool registers_equal(CpuState &other)  {
    for (int i = 0; i < 16; i++)
      if (this->get_register(i) != other.get_register(i))
        return false;

    return this->get_cpsr() == other.get_cpsr();
  }
};



struct ArmCpuState : CpuState {

  Mode mode = USR;

  gword_t reg[16] = {0};
  gword_t reg_bank_fiq[7] = {0};
  gword_t reg_bank_svc[2] = {0};
  gword_t reg_bank_abt[2] = {0};
  gword_t reg_bank_irq[2] = {0};
  gword_t reg_bank_und[2] = {0};

  gword_t cpsr = 0;
  
  gword_t spsr_fiq = 0,
          spsr_svc = 0,
          spsr_abt = 0,
          spsr_irq = 0,
          spsr_und = 0;
  
  gword_t &get_spsr() override {
    assert (mode != SYS);
    assert (mode != USR);
  
    switch (mode) {
      case FIQ: return spsr_fiq;
      case SVC: return spsr_svc;
      case ABT: return spsr_abt;
      case IRQ: return spsr_irq;
      case UND: return spsr_und;
      default: __builtin_unreachable();
    }
  }
  
  gword_t &get_register(gword_t index) override {
    assert (index < 16);
    assert (mode != IRQ);
  
    if (index == 15)
      return reg[index];
   
    // Modes other than usr and fiq all have registers 13 and 14 banked.
    // This will point to one of those register banks, mode permitting.
    gword_t *normal_bank;
  
    switch (mode) {
      case USR:
      case SYS:
        return reg[index];
      case FIQ:
        if (index > 7)
          return reg[index];
        else
          return reg_bank_fiq[index - 8];
      case SVC:
        normal_bank = reg_bank_svc;
        break;
      case ABT:
        normal_bank = reg_bank_abt;
        break;
      case IRQ:
        normal_bank = reg_bank_irq;
        break;
      case UND:
        normal_bank = reg_bank_und;
        break;
    }
  
    if (index < 13)
      return reg[index];
    else
      return normal_bank[index - 13];
  }
  
  gword_t &get_cpsr() override {
    return cpsr;
  }
};

struct RegOverride {
  gword_t index, value;

  RegOverride(gword_t index, gword_t value) : index(index), value(value) {}
};

struct CPSROverride {
  gword_t value;

  CPSROverride(gword_t value) : value(value) {}
};

struct CpuStateOverride : public CpuState {
  CpuState &state;
  vector<variant<RegOverride, CPSROverride>> overrides;

  CpuStateOverride(CpuState &state, vector<variant<RegOverride, CPSROverride>> overrides) : state(state), overrides(move(overrides)) {}

  virtual gword_t &get_register(gword_t index) {
    for (auto &v : overrides) {
      if (std::holds_alternative<RegOverride>(v)) {
        RegOverride &ro = std::get<RegOverride>(v);
        if (ro.index == index) {
          return ro.value;
        }
      }
    }

    return state.get_register(index);
  }

  virtual gword_t &get_cpsr() override {
    return state.get_cpsr();
  }

  virtual gword_t &get_spsr() override {
    return state.get_spsr();
  }
};

}

#endif
