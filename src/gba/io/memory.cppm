module;
#include <assert.h>

export module gba.io:memory;

export import :register_map;

import types;

export {
  ;
  struct IORam {

    static constexpr u32 IO_RAM_SIZE = 0x400;

    u8 io_ram[IO_RAM_SIZE + 8] = {};
    u8 io_ram_ext[8] = {};

    void write(u32 offset, u64 value, u32 width) {
      // TODO:
    }

    u64 read(u32 offset, u32 width) const {
      // Ignore last bit of offset.
      offset &= ~1;
    }

    // if (offset < IO_RAM_SIZE) {
    //   p = &io_ram.io_ram[offset];
    // } else {
    //   u32 mod = offset % 0x10000;

    //   if (mod == 0x800)
    //     p = &io_ram.io_ram_ext[0];
    //   else if (offset == 0x804)
    //     p = &io_ram.io_ram_ext[4];
    //   else
    //     return 0;
    // }
    u16 read_reg(u32 offset) const {
      assert(offset < IO_RAM_SIZE);

      switch (offset) {

      default:
        return io_ram[offset];
        return io_ram[offset];
        return io_ram[offset];
      }
    }
  };
}
