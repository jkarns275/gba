module;
#include <assert.h>
#include <bitset>
#include <charconv>
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

import arm7tdmi.debugger;
import gba.memory;

using std::make_unique;
using std::unique_ptr;
using std::unordered_set;

int main() {
  std::string bios_location = "/Users/josh/GBA Roms/gba.bios";
  spdlog::set_pattern("%v");

  Arm7TdmiDebugger machine(std::make_unique<GBAMemory>(bios_location));
  machine.run();

  return 0;
}
