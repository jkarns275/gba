module;
#include <format>
#include <memory>
#include <vector>

#include <spdlog/spdlog.h>

export module arm7tdmi.arm;

import arm7tdmi;
import arm7tdmi.instruction;

export import :branch;
export import :clz;
export import :data_processing;
export import :mov;
export import :mul;
export import :operands;
export import :swap;
export import :swi;

using std::format;
using std::make_unique;
using std::string;
using std::unique_ptr;

export {
  ;

  struct UndefinedInstruction : public Ins {
    UndefinedInstruction(u32 instruction) : Ins(instruction) {}

    u8 execute(CpuState &state) override { return 0; }

    std::string disassemble() override {
      return std::format("{:#8x} <UNRECOGNIZED INSTRUCTION>", nibbles.word);
    }
  };

  inline constexpr u32 get_cond(u32 i) { return i >> 28; }

  template <typename I> u8 inline execute(u32 ins, CpuState &state) {
    return I(ins).execute(state);
  }

  unique_ptr<Ins> data_processing_000x(u32 i) {
    Nibbles nibbles(i);
    if (((i >> 23) & 0b11) == 0b10 &&
        !(i & flag_mask(DataProcessing::S_FLAG))) {
      switch (nibbles[1] | (nibbles[5] << 4)) {
      case 0b00000000:
      case 0b01000000:
        // Move status register to register
        return make_unique<MovStatusToReg>(i);
      case 0b00100000:
      case 0b01100000:
        // Move register to status
        return make_unique<MovToStatus>(i);
      case 0b01100001:
        // CLZ
        return make_unique<CountLeadingZeros>(i);
      case 0b00100001:
        // Branch and link / exchange
      case 0b00100011:
        // Branch exchange
        return make_unique<BranchExchange>(i);
      default:
        // Drop through to normal case
      }
    }

    if ((nibbles[1] & 0b1001) == 0b1001) {
      if (nibbles[6] == 0b0000) {
        if (nibbles[5] & 0b1000)
          return make_unique<MulLong>(i);
        else
          return make_unique<MulShort>(i);
      } else if (nibbles[1] == 0b1001) {
        return make_unique<SingleDataSwap>(i);
      } else {
        return make_unique<LoadStore>(i);
      }
    } else {
      return make_unique<DataProcessing>(i);
    }
  }

  u8 execute_data_processing_000x(u32 i, CpuState &state) {
    Nibbles nibbles(i);
    if (((i >> 23) & 0b11) == 0b10 &&
        !(i & flag_mask(DataProcessing::S_FLAG))) {
      switch (nibbles[1] | (nibbles[5] << 4)) {
      case 0b00000000:
      case 0b01000000:
        // Move status register to register
        return execute<MovStatusToReg>(i, state);
      case 0b00100000:
      case 0b01100000:
        // Move register to status
        return execute<MovToStatus>(i, state);
      case 0b01100001:
        // CLZ
        return execute<CountLeadingZeros>(i, state);
      case 0b00100001:
        // Branch and link / exchange
      case 0b00100011:
        // Branch exchange
        return execute<BranchExchange>(i, state);
      default:
        // Drop through to normal case
      }
    }

    if ((nibbles[1] & 0b1001) == 0b1001) {
      if (nibbles[6] == 0b0000) {
        if (nibbles[5] & 0b1000)
          return execute<MulLong>(i, state);
        else
          return execute<MulShort>(i, state);
      } else if (nibbles[1] == 0b1001) {
        return execute<SingleDataSwap>(i, state);
      } else {
        return execute<LoadStore>(i, state);
      }
    } else {
      return execute<DataProcessing>(i, state);
    }
  }
  unique_ptr<Ins> data_processing_0011(u32 i) {
    Nibbles nibbles(i);
    switch (nibbles[5]) {
    case 0b0000:
    case 0b0100:
      return make_unique<UndefinedInstruction>(i);
    case 0b0010:
    case 0b0110:
      return make_unique<MovToStatus>(i);
    default:
      return make_unique<DataProcessing>(i);
    }
  }

  u8 execute_data_processing_0011(u32 i, CpuState &state) {
    Nibbles nibbles(i);
    switch (nibbles[5]) {
    case 0b0000:
    case 0b0100:
      return execute<UndefinedInstruction>(i, state);
    case 0b0010:
    case 0b0110:
      return execute<MovToStatus>(i, state);
    default:
      return execute<DataProcessing>(i, state);
    }
  }

  unique_ptr<Ins> load_store_offset_011x(u32 i) {
    if (i & flag_mask(4))
      return make_unique<UndefinedInstruction>(i);
    else
      return make_unique<LoadStoreOffset>(i);
  }

  u8 execute_load_store_offset_011x(u32 i, CpuState &state) {
    if (i & flag_mask(4))
      return execute<UndefinedInstruction>(i, state);
    else
      return execute<LoadStoreOffset>(i, state);
  }

  unique_ptr<Ins> load_store_multiple(u32 i) {
    if (get_cond(i) == 0b1111)
      return make_unique<UndefinedInstruction>(i);
    else
      return make_unique<LoadStoreMultiple>(i);
  }

  u8 execute_load_store_multiple(u32 i, CpuState &state) {
    if (get_cond(i) == 0b1111)
      return execute<UndefinedInstruction>(i, state);
    else
      return execute<LoadStoreMultiple>(i, state);
  }

  inline constexpr u32 decode_index(u32 ins) { return (ins >> 24) & 0xF; }

  // Maps the 6th nibble of an instruction to a factory function.
  const std::function<std::unique_ptr<Ins>(u32 ins)> arm_decode_map[] = {
      data_processing_000x,                   // 0b0000
      data_processing_000x,                   // 0b0001
      make_unique<DataProcessing, u32>,       // 0b0010
      data_processing_0011,                   // 0b0011
      make_unique<LoadStoreOffset, u32>,      // 0b0100
      make_unique<LoadStoreOffset, u32>,      // 0b0101
      load_store_offset_011x,                 // 0b0110
      load_store_offset_011x,                 // 0b0111
      load_store_multiple,                    // 0b1000
      load_store_multiple,                    // 0b1001
      make_unique<BranchWithLink, u32>,       // 0b1010
      make_unique<BranchWithLink, u32>,       // 0b1011
      make_unique<UndefinedInstruction, u32>, // 0b1100 coprocessor instructions
      make_unique<UndefinedInstruction, u32>, // 0b1101 coprocessor instructions
      make_unique<UndefinedInstruction, u32>, // 0b1110 coprocessor instructions
      make_unique<SoftwareInterrupt, u32>,    // 0b1111
  };

  std::unique_ptr<Ins> arm_decode(u32 ins) {
    return arm_decode_map[decode_index(ins)](ins);
  }

  using InstructionExecutor = std::function<u8(u32 ins, CpuState &state)>;
  // Maps the 6th nibble of an instruction to a factory function.
  const InstructionExecutor arm_executor_map[] = {
      execute_data_processing_000x,   // 0b0000
      execute_data_processing_000x,   // 0b0001
      execute<DataProcessing>,        // 0b0010
      execute_data_processing_0011,   // 0b0011
      execute<LoadStoreOffset>,       // 0b0100
      execute<LoadStoreOffset>,       // 0b0101
      execute_load_store_offset_011x, // 0b0110
      execute_load_store_offset_011x, // 0b0111
      execute_load_store_multiple,    // 0b1000
      execute_load_store_multiple,    // 0b1001
      execute<BranchWithLink>,        // 0b1010
      execute<BranchWithLink>,        // 0b1011
      execute<UndefinedInstruction>,  // 0b1100 coprocessor instructions
      execute<UndefinedInstruction>,  // 0b1101 coprocessor instructions
      execute<UndefinedInstruction>,  // 0b1110 coprocessor instructions
      execute<SoftwareInterrupt>,     // 0b1111
  };

  u8 arm_execute(u32 ins, CpuState &state) {
    return arm_executor_map[decode_index(ins)](ins, state);
  }

  void initialize_definition_map() {
    auto &map = InstructionDefinition::DEFINITION_MAP;

    map.insert({"MulShort", {MulShort::definition}});
    map.insert({"MulLong", {MulLong::definition}});
    map.insert({"SingleDataSwap", {SingleDataSwap::definition}});
    map.insert({"LoadStore", LoadStore::definitions});
    map.insert({"DataProcessing", DataProcessing::definitions});
    map.insert({"MovStatusToReg", {MovStatusToReg::definition}});
    map.insert({"MovToStatus", {MovToStatus::definition}});
    map.insert({"BranchExchange", {BranchExchange::definition}});
    map.insert({"CountLeadingZeros", {CountLeadingZeros::definition}});
    map.insert({"LoadStoreOffset", LoadStoreOffset::definitions});
    map.insert({"LoadStoreMultiple", {LoadStoreMultiple::definition}});
    map.insert({"BranchWithLink", {BranchWithLink::definition}});
    map.insert({"SoftwareInterrupt", {SoftwareInterrupt::definition}});
  }
}
