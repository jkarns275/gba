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

    // NOTE: These interfaces precludes reading types larger than 64 bits.
    // This won't be a problem for the GBA, and probably won't be an issue for
    // even the NDS and 3DS. I think it can also be made faster, but it probably
    // works just fine.
    virtual void write_impl(u32 addr, u64 value, u32 width, Mode mode) = 0;
    virtual u64 read_impl(u32 addr, u32 width, Mode mode) const = 0;

  public:
    CpuState *cpu_state = nullptr;

    template <Integral I> inline I read(u32 addr, Mode mode) {
      static_assert(sizeof(I) <= sizeof(u64));
      return (I)read_impl(addr, sizeof(I), mode);
    }

    template <Integral I> inline void write(u32 addr, I value, Mode mode) {
      write_impl(addr, (u64)value, sizeof(I), mode);
    }

    u32 rotated_at(u32 addr, Mode mode) {
      return ror<u32>(read<u32>(addr, mode), 8 * (addr & 0b11));
    }
  };

  struct SimpleMemory : public Memory {
    u8 data[0x3000] = {};

    void write_impl(u32 addr, u64 value, u32 width, Mode mode) override {
      u8 *p = &data[addr];
      switch (width) {
      case 1:
        *p = value;
        break;
      case 2:
        *(u16 *)p = value;
        break;
      case 4:
        *(u32 *)p = value;
        break;
      case 8:
        *(u64 *)p = value;
        break;
      default:
        assert(false);
      }
    }

    u64 read_impl(u32 addr, u32 width, Mode mode) const override {
      const u8 *p = &data[addr];
      switch (width) {
      case 1:
        return *p;
      case 2:
        return *(const u16 *)p;
      case 4:
        return *(const u32 *)p;
      case 8:
        return *(const u64 *)p;
      default:
        assert(false);
      }
    }

    // u8 *u8_at(u32 addr, u32 width, Mode mode) override { return &data[addr];
    // } const u8 *const_u8_at(u32 addr, u32 width, Mode mode) override {
    //   return &data[addr];
    // }
  };
}
