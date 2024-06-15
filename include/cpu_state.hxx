#include "bitutil.hxx"
#include "types.hxx"

enum Mode : byte {
  USR = 0b10000,
  FIQ = 0b10001,
  IRQ = 0b10010,
  SVC = 0b10011,
  ABT = 0b10111,
  UND = 0b11011,
  SYS = 0b11111,
};

bool mode_is_valid(Mode mode);

const gword_t INDEX_PC = 15;
const gword_t INDEX_LR = 14;
const gword_t INDEX_SP = 13;

const gword_t STATUS_NEGATIVE_MASK = 1 << 31;
const gword_t STATUS_ZERO_MASK = 1 << 30;
const gword_t STATUS_CARRY_MASK = 1 << 29;
const gword_t STATUS_OVERFLOW_MASK = 1 << 28;
const gword_t STATUS_IRQ_DISABLE_MASK = 1 << 7;
const gword_t STATUS_FIQ_DISABLE_MASK = 1 << 6;
const gword_t STATUS_STATE_MASK = 1 << 5;
const gword_t STATUS_MODE_MASK = (1 << 5) - 1;

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

  gword_t offset_arm(gword_t status_register);

};

struct CpuState {
  static constexpr gword_t N_FLAG = flag_mask(31);
  static constexpr gword_t Z_FLAG = flag_mask(30);
  static constexpr gword_t C_FLAG = flag_mask(29);
  static constexpr gword_t V_FLAG = flag_mask(28);

  virtual gword_t &get_register(gword_t index) = 0;
  virtual gword_t &get_cpsr() = 0;
  virtual gword_t &get_spsr() = 0;

  gword_t get_flag(gword_t mask);
  gword_t &get_sp();
  gword_t &get_lr();
  gword_t &get_pc();
  
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

  virtual gword_t &get_cpsr() override;
  virtual gword_t &get_spsr() override;
  virtual gword_t &get_register(gword_t index) override;
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
