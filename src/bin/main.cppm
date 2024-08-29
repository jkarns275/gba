module;
#include <assert.h>
#include <bitset>
#include <format>
#include <iostream>
#include <memory>
#include <optional>
#include <stddef.h>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <vector>

#include <spdlog/spdlog.h>

export module main;

import types;
import bitutil;
import arm7tdmi;
import arm7tdmi.instruction;
import arm7tdmi.arm;
import arm7tdmi.thumb;

using std::make_unique;
using std::unique_ptr;

int main() {
  // initialize_definition_map();

  std::unique_ptr<GBAMemory> memory =
      std::make_unique<GBAMemory>("/Users/josh/GBA Roms/gba.bios");

  CpuState state(*memory);

  int nex = 0;
  for (;;) {
    u32 pc = state.read_current_pc();

    unique_ptr<Ins> instruction;
    if (state.is_thumb_mode()) {
      u16 ins = memory->read<u16>(pc, Mode::SVC);
      spdlog::info("        :  {:#018b}", ins);

      instruction = std::move(ThumbInstruction(ins).instruction);

      spdlog::info("{:04x}:  {:04x}        {}", pc, ins,
                   instruction->disassemble());
    } else {
      u32 word = memory->read<u32>(pc, Mode::SVC);
      spdlog::info("        :  {:#034b}", word);

      ArmInstruction ins(word);
      instruction = std::move(ins.instruction);

      spdlog::info("{:08x}:  {:08x}        {}", pc, word,
                   instruction->disassemble());
    }

    if (!instruction)
      break;

    getchar();
    if (state.is_thumb_mode() || state.evaluate_cond(instruction->cond)) {
      instruction->execute(state);
      state.print_registers();
      nex += 1;
    }

    if (state.read_current_pc() == pc) {
      if (state.is_thumb_mode()) {
        state.write_register(15, state.read_current_pc() + 2);
      } else {
        state.write_register(15, state.read_current_pc() + 4);
      }
    }
  }

  spdlog::info("Executed {:#08x} instructions", nex);
  return 0;
}
