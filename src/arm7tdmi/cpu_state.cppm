module;
#include <assert.h>
#include <format>
#include <iostream>
#include <variant>
#include <vector>

export module arm7tdmi:cpu_state;

import bitutil;
import types;

import :mode;
import :exception;
import :memory;

using std::format;
using std::variant;
using std::vector;

export {
  ;

  enum Cond : u8 {
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

  struct CpuState {
    static constexpr u32 N_FLAG = flag_mask(31);
    static constexpr u32 Z_FLAG = flag_mask(30);
    static constexpr u32 C_FLAG = flag_mask(29);
    static constexpr u32 V_FLAG = flag_mask(28);
    static constexpr u32 Q_FLAG = flag_mask(27);
    static constexpr u32 I_FLAG = flag_mask(7);
    static constexpr u32 F_FLAG = flag_mask(6);
    static constexpr u32 T_FLAG = flag_mask(5);

    static constexpr u32 ALL_FLAGS =
        N_FLAG | Z_FLAG | C_FLAG | V_FLAG | Q_FLAG | I_FLAG | F_FLAG | T_FLAG;

    static constexpr u32 MODE_MASK = 0x1F;

    // A mask for all of the modifiable bits on the PSR.
    // Bits other than this are marked do-not-modify and read-as-zeros
    static constexpr u32 PSR_MASK = ALL_FLAGS | MODE_MASK;

    static inline constexpr u32 INDEX_PC = 15;
    static inline constexpr u32 INDEX_LR = 14;
    static inline constexpr u32 INDEX_SP = 13;

    u32 reg[16] = {};
    u32 reg_bank_fiq[7] = {};
    u32 reg_bank_svc[2] = {};
    u32 reg_bank_abt[2] = {};
    u32 reg_bank_irq[2] = {};
    u32 reg_bank_und[2] = {};

    u32 cpsr = 0;

    u32 spsr_fiq = 0, spsr_svc = 0, spsr_abt = 0, spsr_irq = 0, spsr_und = 0;

    Memory &memory;

    CpuState(Memory &memory) : memory(memory) { memory.cpu_state = this; }

    bool is_thumb_mode() { return cpsr & CpuState::T_FLAG; }

  private:
    u32 &get_spsr(Mode mode) {
      assert(mode != SYS);
      assert(mode != USR);

      switch (mode) {
      case FIQ:
        return spsr_fiq;
      case SVC:
        return spsr_svc;
      case ABT:
        return spsr_abt;
      case IRQ:
        return spsr_irq;
      case UND:
        return spsr_und;
      // this is specified as unpredictable;
      default:
        return cpsr;
      }
    }

    u32 &get_register(u32 index, Mode mode) {
      assert(index < 16);
      assert(mode != IRQ);

      // Modes other than usr and fiq all have registers 13 and 14 banked.
      // This will point to one of those register banks, mode permitting.
      u32 *reg_bank;

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

  public:
    u32 read_register(u32 index, Mode mode) {
      u32 reg = get_register(index, mode);

      if (index == INDEX_PC) {
        if (is_thumb_mode()) {
          return reg + 4;
        } else {
          return reg + 8;
        }
      } else {
        return reg;
      }
    }

    u32 read_register(u32 index) { return read_register(index, get_mode()); }

    void write_register(u32 index, u32 value, Mode mode) {
      u32 &reg = get_register(index, mode);
      reg = value;
    }

    inline void write_register(u32 index, u32 value) {
      write_register(index, value, get_mode());
    }

    u32 read_cpsr() { return cpsr & CpuState::PSR_MASK; }

    u32 read_spsr(Mode mode) { return get_spsr(mode); }

    u32 read_spsr() { return get_spsr(get_mode()); }

    void write_cpsr(u32 new_cpsr) { cpsr = new_cpsr & PSR_MASK; }

    void write_spsr(u32 new_spsr, Mode mode) {
      u32 &spsr = get_spsr(mode);
      spsr = new_spsr;
    }

    inline void write_spsr(u32 new_spsr) { write_spsr(new_spsr, get_mode()); }

    Mode get_mode() { return (Mode)(USR | (cpsr & MODE_MASK)); }

    void set_mode(Mode mode) {
      cpsr &= ~MODE_MASK;
      cpsr |= (u32)mode;
    }

    u32 get_flag(u32 mask) {
      assert(mask & ALL_FLAGS);
      return bool(cpsr & mask);
    }

    void set_flag(u32 mask) {
      assert((mask & ALL_FLAGS) || mask == 0);
      cpsr |= mask;
    }

    void clear_flag(u32 mask) {
      assert(mask & ALL_FLAGS);
      cpsr &= ~mask;
    }

    u32 read_sp() { return read_register(INDEX_SP); }

    void write_sp(u32 value) { write_register(INDEX_SP, value); }

    u32 read_lr() { return read_register(INDEX_LR); }

    void write_lr(u32 value) { write_register(INDEX_LR, value); }

    u32 read_pc() { return read_register(INDEX_PC); }

    u32 read_current_pc() { return get_register(INDEX_PC, get_mode()); }

    void write_pc(u32 value) { write_register(INDEX_PC, value); }

    void print_registers() {
      for (int i = 0; i < 16; i++) {
        if (i % 4 == 0 && i)
          std::cout << "\n";
        u32 value = read_register(i);
        std::cout << std::format("r{:<2} : 0x{:<8x}", i, value) << "  ";
      }
      std::cout << "\n";
      std::cout << std::format("cpsr: 0x{:<8x}\n", cpsr);
    }

    bool evaluate_cond(Cond cond) {
      bool n = N_FLAG & cpsr, z = Z_FLAG & cpsr, c = C_FLAG & cpsr,
           v = V_FLAG & cpsr;
      switch (cond) {
      case EQ:
        return z;
      case NE:
        return !z;
      case CSHS:
        return c;
      case CCLO:
        return !c;
      case MI:
        return n;
      case PL:
        return !n;
      case VS:
        return v;
      case VC:
        return !v;
      case HI:
        return c && !z;
      case LS:
        return !c || z;
      case GE:
        return n == v;
      case LT:
        return n != v;
      case GT:
        return !z && n == v;
      case LE:
        return z && n != v;
      case AL:
        return true;
      // Supposed to be unpredictable
      case NV:
        return false;
      }
    }

    template <Integral I> inline I read(u32 addr, Mode mode) {
      return memory.read<I>(addr, mode);
    }

    template <Integral I> inline I read(u32 addr) {
      return read<I>(addr, get_mode());
    }

    template <Integral I> inline void write(u32 addr, I value, Mode mode) {
      return memory.write<I>(addr, value, mode);
    }

    template <Integral I> inline void write(u32 addr, I value) {
      return write(addr, value, get_mode());
    }

    inline u32 rotated_at(u32 addr) {
      return memory.rotated_at(addr, get_mode());
    }
  };
}
