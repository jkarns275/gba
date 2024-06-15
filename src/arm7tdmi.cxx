#include <assert.h>
#include <stddef.h>
#include <bitset>

#include "arm7tdmi.hxx"

using std::make_unique;

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

gword_t Exception::offset_arm(gword_t status_register) {
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

gword_t &CpuState::get_sp() {
  return get_register(INDEX_SP);
}

gword_t &CpuState::get_lr() {
  return get_register(INDEX_LR);
}

gword_t &CpuState::get_pc() {
  return get_register(INDEX_PC);
}

gword_t CpuState::get_flag(gword_t mask) {
  return bool(get_cpsr() & mask);
}

gword_t &ArmCpuState::get_spsr() {
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

gword_t &ArmCpuState::get_register(gword_t index) {
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

gword_t &ArmCpuState::get_cpsr() {
  return cpsr;
}


// void enumerate_instructions() {
//   InstructionDefinition::initialize_definition_map();
//   ArmCpuState state;
//   int32_t i = 0;
//   for (auto mit = InstructionDefinition::DEFINITION_MAP.begin(); mit != InstructionDefinition::DEFINITION_MAP.end(); mit++) {
//     const string &name = mit->first;
//     std::cout << "Instuction: " << name << std::endl;
//     for (int i = 0; i < mit->second.size(); i++) {
//       const InstructionDefinition &d = *mit->second[i];
//       auto it = d.begin();
//       optional<gword_t> x;
// 
//       while ((x = it.get())) {
//         auto p = d.validate(x.value());
//         if (p.size() == d.pieces.size()) {
//           for (int i = p.size() - 1; i >= 0; i--) {
//             DecodedPiece &piece = p[i];
//             std::cout << piece.name << ": " << std::format("{:>{}b}", piece.piece, piece.length) << "/" << piece.length << " ";
//           }
//           std::cout << std::endl;
//           Instruction(x.value()).execute(state);
//         } else {
//           std::cout << "Failed to validate " << name << " instruction def " << i << " failed with " << p.size() << std::endl;
//           std::cout << std::format("{:>32b}", x.value()) << std::endl;
//           return;
//           break;
//         }
// 
//         it.step();
//       }
//     }
//   }
// }

bool test(Instruction &ins, CpuState &state_in, CpuState &expected_state) {
  ins.execute(state_in);
  return state_in.registers_equal(expected_state);
}

int main() {
  InstructionDefinition::initialize_definition_map();
  
  ArmCpuState state;
  const vector<const InstructionDefinition *> &definitions = DataProcessing::definitions;
  InstructionDefinition *dd = new InstructionDefinition({
    new CondPiece(), new Zeros(3), new IntegralPiece(4, "opcode", 0b0100, 0b10000), new BoolPiece("S"), new RegPiece("Rn"), new RegPiece("Rd"),
    new IntegralPiece(5, "shift amount"), new IntegralPiece(2, "shift type"), new Zeros(1), new RegPiece("Rm")
  });


  const InstructionDefinition &def = *dd;
  auto it = def.begin();
  optional<gword_t> x;

  unordered_map<string, gword_t> ttt = {
    {"shift amount", 2}, 
    {"shift type", BitShift::LEFT},
    {"S", 1},
    {"Rn", 0},
    {"Rd", 1},
    {"Rm", 2},
    {"opcode", DataProcessing::Opcode::ADD},
    {"cond", 0b1111}
  };
  gword_t ins = def.build(ttt);
  
  std::cout << std::format("{:>{}b}\n", ins, 32);
  
  ArmCpuState base_input_state;
  CpuStateOverride input_state(base_input_state, { RegOverride{0, 16}, RegOverride(2, 2) });

  DataProcessing dp(ins); 

  input_state.print_registers();
  std::cout << "--------------------\n";
  dp.execute(input_state);
  std::cout << "--------------------\n";
  input_state.print_registers();

  return 1;

  // while ((x = it.get())) {
  //   auto p = def.validate(x.value());
  //   if (p.size() == def.pieces.size()) {
  //     for (int i = p.size() - 1; i >= 0; i--) {
  //       DecodedPiece &piece = p[i];
  //       std::cout << piece.name << ": " << std::format("{:>{}b}", piece.piece, piece.length) << "/" << piece.length << " ";
  //     }
  //     std::cout << std::endl;
  //   }
  //   DataProcessing dp(x.value());
  //   dp.execute(state);
  //   std::cout << std::format("Instruction: {:>32b}", x.value()) << std::endl;
  //   for (int i = 0; i < 16; i++) {
  //     if (i % 4 == 0 && i)
  //       std::cout << "\n";
  //     std::cout << std::format("r{:<2}: {:<8x}", i, state.get_register(i)) << "  ";
  //   }
  //   std::cout << "\n";
  //   std::cout << "\n";
  //   
  //   it.step();
  // }

}
