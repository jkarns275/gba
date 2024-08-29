module;
#include <spdlog/spdlog.h>

export module arm7tdmi:memory;

import types;
import bitutil;

import :mode;

using std::optional;
using std::string;

export {
  ;

  struct CpuState;

  class Memory {

    void invalid_read(u32 addr) { throw addr; }

    virtual u8 *u8_at(u32 addr, u32 width, Mode mode) = 0;
    virtual const u8 *const_u8_at(u32 addr, u32 width, Mode mode) = 0;

  public:
    CpuState *cpu_state = nullptr;

    template <Integral I> inline I read(u32 addr, Mode mode) {
      const u8 *p = const_u8_at(addr, sizeof(I), mode);
      assert(p != nullptr);
      return *reinterpret_cast<const I *>(p);
    }

    template <Integral I> inline void write(u32 addr, I value, Mode mode) {
      u8 *p = u8_at(addr, sizeof(I), mode);
      *reinterpret_cast<I *>(p) = value;
    }

    u32 rotated_at(u32 addr, Mode mode) {
      return ror<u32>(read<u32>(addr, mode), 8 * (addr & 0b11));
    }
  };

  struct SimpleMemory : public Memory {
    u8 data[0x3000] = {};

    u8 *u8_at(u32 addr, u32 width, Mode mode) override { return &data[addr]; }
    const u8 *const_u8_at(u32 addr, u32 width, Mode mode) override { return &data[addr]; }
  };

}
