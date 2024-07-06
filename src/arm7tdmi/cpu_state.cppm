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
  ;

enum Mode : byte {
  USR = 0b10000,
  FIQ = 0b10001,
  IRQ = 0b10010,
  SVC = 0b10011,
  ABT = 0b10111,
  UND = 0b11011,
  SYS = 0b11111,
};

bool mode_is_privileged(Mode mode) {
  return mode != USR;
}

bool mode_has_spsr(Mode mode) {
  return mode != USR && mode != SYS;
}

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

constexpr gword_t STATUS_NEGATIVE_MASK = flag_mask(31);
constexpr gword_t STATUS_ZERO_MASK = flag_mask(30);
constexpr gword_t STATUS_CARRY_MASK = flag_mask(29);
constexpr gword_t STATUS_OVERFLOW_MASK = flag_mask(28);
constexpr gword_t STATUS_IRQ_DISABLE_MASK = 1 << 7;
constexpr gword_t STATUS_FIQ_DISABLE_MASK = 1 << 6;
constexpr gword_t STATUS_STATE_MASK = 1 << 5;
constexpr gword_t STATUS_MODE_MASK = (1 << 5) - 1;

enum Cond : byte {
  EQ = 0b0000,
  NE = 0b0001,
  CSHS = 0b0010,
  CCLO = 0b0011,
  MI = 0b0100,
  PL = 0b0101,
  VS = 0b0110,
  VC = 0b0111,
  HI = 0b1000,
  LS = 0b1001,
  GE = 0b1010,
  LT = 0b1011,
  GT = 0b1100,
  LE = 0b1101,
  AL = 0b1110,
  NV = 0b1111,
};

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

struct Memory {

  void invalid_read(gword_t addr) {
    throw addr;
  }

  virtual glong_t &long_at(gword_t addr, Mode mode) = 0;
  virtual signed_glong_t &signed_long_at(gword_t addr, Mode mode) = 0;

  virtual gword_t &at(gword_t addr, Mode mode) = 0;
  virtual signed_gword_t &signed_at(gword_t addr, Mode mode) = 0;
  
  virtual gshort_t &short_at(gword_t addr, Mode mode) = 0;
  virtual signed_gshort_t &signed_short_at(gword_t addr, Mode mode) = 0;
  
  virtual byte &byte_at(gword_t addr, Mode mode) = 0;
  virtual signed_byte &signed_byte_at(gword_t addr, Mode mode) = 0;

  gword_t rotated_at(gword_t addr, Mode mode) {
    return ror<gword_t>(at(addr, mode), 8 * (addr & 0b11));
  }

};

struct SimpleMemory : public Memory {
  byte data[0x3000] = {0};

  glong_t &long_at(gword_t addr, Mode mode) override { return ((glong_t *) data)[addr / 8]; }
  signed_glong_t &signed_long_at(gword_t addr, Mode mode) override { return ((signed_glong_t *) data)[addr / 8]; }

  gword_t &at(gword_t addr, Mode mode) override { return ((gword_t *) data)[addr / 4]; }
  signed_gword_t &signed_at(gword_t addr, Mode mode) override { return ((signed_gword_t *) data)[addr / 4]; }
  
  gshort_t &short_at(gword_t addr, Mode mode) override { return ((gshort_t *) data)[addr / 2]; }
  signed_gshort_t &signed_short_at(gword_t addr, Mode mode) override { return ((signed_gshort_t *) data)[addr / 2]; }
  
  byte &byte_at(gword_t addr, Mode mode) override { return data[addr]; }
  signed_byte &signed_byte_at(gword_t addr, Mode mode) override { return ((signed_byte *) data)[addr]; }
};

struct CpuState {
  static constexpr gword_t N_FLAG = flag_mask(31);
  static constexpr gword_t Z_FLAG = flag_mask(30);
  static constexpr gword_t C_FLAG = flag_mask(29);
  static constexpr gword_t V_FLAG = flag_mask(28);
  static constexpr gword_t Q_FLAG = flag_mask(27);
  static constexpr gword_t I_FLAG = flag_mask(7);
  static constexpr gword_t F_FLAG = flag_mask(6);
  static constexpr gword_t T_FLAG = flag_mask(5);

  static constexpr gword_t ALL_FLAGS = N_FLAG | Z_FLAG | C_FLAG | V_FLAG | Q_FLAG | I_FLAG | F_FLAG | T_FLAG;

  static constexpr gword_t MODE_MASK = 0x1F;

  static inline constexpr gword_t INDEX_PC = 15;
  static inline constexpr gword_t INDEX_LR = 14;
  static inline constexpr gword_t INDEX_SP = 13;

  virtual Mode get_mode() = 0;
  virtual void set_mode(Mode mode) = 0;
  
  virtual gword_t &get_register(gword_t index, Mode mode) = 0;
  gword_t &get_register(gword_t index) {
    return get_register(index, get_mode());
  }

  virtual gword_t &get_cpsr() = 0;

  virtual gword_t &get_spsr(Mode mode) = 0;
  inline gword_t &get_spsr() {
    return get_spsr(get_mode());
  }

  Memory &memory;

  CpuState(Memory &memory) : memory(memory) { }

  gword_t get_flag(gword_t mask) {
    return bool(get_cpsr() & mask);
  }

  void set_flag(gword_t mask) {
    get_cpsr() |= mask;
  }

  void clear_flag(gword_t mask) {
    get_cpsr() &= ~mask;
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
    std::cout << std::format("cpsr: 0x{:<8x}\n", get_cpsr());
  }

  bool registers_equal(CpuState &other)  {
    for (int i = 0; i < 16; i++)
      if (this->get_register(i) != other.get_register(i))
        return false;

    return this->get_cpsr() == other.get_cpsr();
  }

  bool evaluate_cond(Cond cond) {
    gword_t cspr = get_cpsr();
    bool  n = N_FLAG & cspr,
          z = Z_FLAG & cspr,
          c = C_FLAG & cspr,
          v = V_FLAG & cspr;
    switch (cond) {
      case EQ: return z;
      case NE: return !z;
      case CSHS: return c;
      case CCLO: return !c;
      case MI: return n;
      case PL: return !n;
      case VS: return v;
      case VC: return !v;
      case HI: return c && !z;
      case LS: return !c || z;
      case GE: return n == v;
      case LT: return n != v;
      case GT: return !z && n == v;
      case LE: return z && n != v;
      case AL: return true;
      // Supposed to be unpredictable
      case NV: return false;
    }
  }

  inline glong_t &long_at(gword_t addr) {
    return memory.long_at(addr, get_mode());
  }

  inline signed_glong_t &signed_long_at(gword_t addr) {
    return memory.signed_long_at(addr, get_mode());
  }

  inline gword_t &at(gword_t addr) {
    return memory.at(addr, get_mode());
  }

  inline signed_gword_t &signed_at(gword_t addr) {
    return memory.signed_at(addr, get_mode());
  }

  inline gshort_t &short_at(gword_t addr) {
    return memory.short_at(addr, get_mode());
  }

  inline signed_gshort_t &signed_short_at(gword_t addr) {
    return memory.signed_short_at(addr, get_mode());
  }

  inline byte &byte_at(gword_t addr) {
    return memory.byte_at(addr, get_mode());
  }
  
  inline signed_byte &signed_byte_at(gword_t addr) {
    return memory.signed_byte_at(addr, get_mode());
  }

  gword_t rotated_at(gword_t addr) {
    return memory.rotated_at(addr, get_mode());
  }
};

struct ArmCpuState : public CpuState {
 
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
  
  ArmCpuState(Memory &memory) : CpuState(memory) { }
  
  gword_t &get_spsr(Mode mode) override {
    assert (mode != SYS);
    assert (mode != USR);
  
    switch (mode) {
      case FIQ: return spsr_fiq;
      case SVC: return spsr_svc;
      case ABT: return spsr_abt;
      case IRQ: return spsr_irq;
      case UND: return spsr_und;
      // this is specified as unpredictable;
      default: return cpsr;
    }
  }
  
  gword_t &get_register(gword_t index, Mode mode) override {
    assert (index < 16);
    assert (mode != IRQ);
   
    // Modes other than usr and fiq all have registers 13 and 14 banked.
    // This will point to one of those register banks, mode permitting.
    gword_t *reg_bank;
  
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
        reg_bank = reg_bank_svc;
        break;
      case ABT:
        reg_bank = reg_bank_abt;
        break;
      case IRQ:
        reg_bank = reg_bank_irq;
        break;
      case UND:
        reg_bank = reg_bank_und;
        break;
    }
    
    if (index == 15)
      return reg[index];
    else if (index < 13)
      return reg[index];
    else
      return reg_bank[index - 13];
  }
  
  gword_t &get_cpsr() override {
    return cpsr;
  }

  Mode get_mode() override {
    return (Mode) (USR | (cpsr & MODE_MASK));
  }

  void set_mode(Mode mode) override {
    cpsr &= ~MODE_MASK;
    cpsr |= (gword_t) mode;
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

  CpuStateOverride(CpuState &state, vector<variant<RegOverride, CPSROverride>> overrides) : CpuState(state.memory), state(state), overrides(std::move(overrides)) {}

  // TODO: Make this support specific CPU modes.
  gword_t &get_register(gword_t index, Mode mode) override {
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

  virtual gword_t &get_spsr(Mode mode) override {
    return state.get_spsr(mode);
  }
};

}

#endif
