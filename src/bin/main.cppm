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
#include <unordered_set>
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
using std::unordered_set;

struct App {
  unique_ptr<GBAMemory> memory;
  CpuState state;

  unordered_set<u32> breakpoints;

  bool paused = true;

  App(std::string bios_location)
      : memory(make_unique<GBAMemory>(bios_location)), state(*this->memory) {}

  std::string last_command;

  void read_command() {
    while (1) {
      std::string line;
      std::getline(std::cin, line);

      std::stringstream ss(line);
      std::string command;

      ss >> command;

      if (command.size() == 0)
        command = last_command;

      last_command = command;
      if (command == "n" || command == "next") {
        return;
      } else if (command == "b" || command == "break") {
        u32 breakpoint;
        ss >> std::hex >> breakpoint;
        spdlog::info("Breakpoint 0x{:08x}", breakpoint);
        breakpoints.insert(breakpoint);
        continue;
      } else if (command == "br" || command == "break_remove") {
        u32 breakpoint;
        ss >> std::hex >> breakpoint;
        spdlog::info("Removing breakpoint 0x{:08x}", breakpoint);
        breakpoints.erase(breakpoint);
        continue;
      } else if (command == "c" || command == "continue") {
        paused = false;
        return;
      } else if (command == "r" || command == "read") {
        std::string size;
        ss >> size;
        u32 addr;
        ss >> std::hex >> addr;

        if (size == "b") {
          u8 p = state.read<u8>(addr);
          spdlog::info("byte at 0x{:^08x}: 0x{:^02x}", addr, (u32)p);
        } else if (size == "s") {
          u16 p = state.read<u16>(addr);
          spdlog::info("byte at 0x{:^08x}: 0x{:^04x}", addr, p);
        } else if (size == "w") {
          u32 p = state.read<u32>(addr);
          spdlog::info("byte at 0x{:^08x}: 0x{:^08x}", addr, p);
        } else if (size == "l") {
          u16 p = state.read<u16>(addr);
          spdlog::info("byte at 0x{:^08x}: 0x{:^016x}", addr, p);
        }
      }

      spdlog::error("Invalid command '{}'", line);
      print_help();
    }
  }

  void print_help() {}

  unique_ptr<Ins> decode_thumb(u32 pc) {
    u16 ins = memory->read<u16>(pc, Mode::SVC);
    // if (paused)
    spdlog::info("        :  {:#018b}", ins);

    unique_ptr<Ins> instruction = std::move(ThumbInstruction(ins).instruction);

    // if (paused)
    spdlog::info("{:04x}:  {:04x}        {}", pc, ins,
                 instruction->disassemble());

    return instruction;
  }

  unique_ptr<Ins> decode(u32 pc) {
    u32 word = memory->read<u32>(pc, Mode::SVC);
    // if (paused)
    spdlog::info("        :  {:#034b}", word);

    ArmInstruction ins(word);
    unique_ptr<Ins> instruction = std::move(ins.instruction);

    // if (paused)
    spdlog::info("{:08x}:  {:08x}        {}", pc, word,
                 instruction->disassemble());

    return instruction;
  }

  void run() {
    for (;;) {
      u32 pc = state.read_current_pc();
      if (breakpoints.count(pc))
        paused = true;

      unique_ptr<Ins> instruction;
      if (state.is_thumb_mode()) {
        instruction = decode_thumb(pc);
      } else {
        instruction = decode(pc);
      }

      if (!instruction) {
        spdlog::error("Encountered invalid instruction.");
        break;
      }

      if (paused) {
        read_command();
      }

      if (state.is_thumb_mode() || state.evaluate_cond(instruction->cond)) {
        instruction->execute(state);
      }
      // if (paused)
      state.print_registers();

      if (state.read_current_pc() == pc) {
        if (state.is_thumb_mode()) {
          state.write_register(15, state.read_current_pc() + 2);
        } else {
          state.write_register(15, state.read_current_pc() + 4);
        }
      }
    }
  }
};

int main() {
  std::string bios_location = "/Users/josh/GBA Roms/gba.bios";

  App app(bios_location);
  app.run();

  return 0;
}
