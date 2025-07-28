module;
#include <charconv>
#include <iostream>
#include <memory>
#include <unordered_set>_set>_set>

#include <spdlog/spdlog.h>

export module arm7tdmi.debugger;

import arm7tdmi;
import arm7tdmi.instruction;
import arm7tdmi.thumb;
import arm7tdmi.arm;

using std::unique_ptr;
using std::unordered_set;

export {
  ;

  struct Arm7TdmiDebugger {
    std::unique_ptr<Memory> memory;
    CpuState state;
    unordered_set<u32> breakpoints;
    bool paused = true;
    std::string last_command;

    Arm7TdmiDebugger(std::unique_ptr<Memory> memory)
        : memory(std::move(memory)), state(*this->memory) {}

    void decode_thumb(u32 pc) {
      u16 ins = memory->read<u16>(pc, Mode::SVC);

      unique_ptr<Ins> instruction = thumb_decode(ins);

      // spdlog::info("        :  {:#018b}", ins);
      spdlog::info("{:08X}:  {:04X}        {}", pc, ins,
                   instruction->disassemble());
    }

    void decode_arm(u32 pc) {
      u32 ins = memory->read<u32>(pc, Mode::SVC);

      unique_ptr<Ins> instruction = arm_decode(ins);

      // spdlog::info("        :  {:#034b}", word);
      spdlog::info("{:08X}:  {:08X}        {}", pc, ins,
                   instruction->disassemble());
    }

    void run() {
      for (;;) {
        u32 pc = state.read_current_pc();

        if (breakpoints.count(pc))
          paused = true;

        if (paused) {
          state.print_state();

          if (state.is_thumb_mode()) {
            decode_thumb(pc);
          } else {
            decode_arm(pc);
          }

          read_command();
        }

        if (state.is_thumb_mode()) {
          state.cycles(thumb_execute(state.read<u16>(pc, Mode::SVC), state));
        } else {
          u32 ins = state.read<u32>(pc, Mode::SVC);
          state.cycles(state.evaluate_cond((Cond)get_cond(ins))
                           ? arm_execute(ins, state)
                           : 1);
        }

        if (state.read_current_pc() == pc) {
          if (state.is_thumb_mode()) {
            state.write_register(15, state.read_current_pc() + 2);
          } else {
            state.write_register(15, state.read_current_pc() + 4);
          }
        }
      }
    }

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
            spdlog::info("short at 0x{:^08x}: 0x{:^04x}", addr, p);
          } else if (size == "w") {
            u32 p = state.read<u32>(addr);
            spdlog::info("word at 0x{:^08x}: 0x{:^08x}", addr, p);
          } else if (size == "l") {
            u16 p = state.read<u16>(addr);
            spdlog::info("long at 0x{:^08x}: 0x{:^016x}", addr, p);
          } else {
            u32 nbytes = -1;
            std::from_chars(size.data(), size.data() + size.size(), nbytes);

            if (nbytes <= 0) {
              return;
            }

            u32 lo = addr & (~0xF);
            u32 hi = addr + nbytes;
            spdlog::info("Reading {} bytes from 0x{:08x} to 0x{:08x}", nbytes,
                         lo, hi);
            for (u32 i = lo; i < hi; i += 0x10) {
              std::string data;
              for (u32 j = 0; j < 0x10; j++) {
                data += fmt::format("{:02x} ", state.read<u8>(i + j));
              }

              spdlog::info("0x{:08x}: {}", i, data);
            }
          }

          return;
        }

        spdlog::error("Invalid command '{}'", line);
      }
    }
  };
}
