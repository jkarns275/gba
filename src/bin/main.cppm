module;
#include <assert.h>
#include <stddef.h>
#include <bitset>
#include <iostream>
#include <format>
#include <memory>
#include <string>
#include <optional>
#include <vector>
#include <unordered_map>
#include <type_traits>

export module main;

import types;
import bitutil;
import arm7tdmi.arm;

using std::make_unique;

bool test(Instruction &ins, CpuState &state_in, CpuState &expected_state) {
  ins.execute(state_in);
  return state_in.registers_equal(expected_state);
}

int main() {
  initialize_definition_map();
  
  ArmCpuState state;
  const std::vector<const InstructionDefinition *> &definitions = DataProcessing::definitions;
  InstructionDefinition *dd = new InstructionDefinition({
    new CondPiece(), new Zeros(3), new IntegralPiece(4, "opcode", 0b0100, 0b10000), new BoolPiece("S"), new RegPiece("Rn"), new RegPiece("Rd"),
    new IntegralPiece(5, "shift amount"), new IntegralPiece(2, "shift type"), new Zeros(1), new RegPiece("Rm")
  });


  const InstructionDefinition &def = *dd;
  auto it = def.begin();
  std::optional<gword_t> x;

  std::unordered_map<std::string, gword_t> ttt = {
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
