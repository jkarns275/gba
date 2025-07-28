module;
#include <assert.h>

export module gba.io:memory;

export import :register_map;

import types;

static inline constexpr u32 IO_RAM_SIZE = 0x400;
export {
  ;

  struct IORamMasks {
    static constexpr u32 MASK_BUFFER_SIZE = 0x400 + 8;

    u8 read_mask_data[MASK_BUFFER_SIZE];
    u8 write_mask_data[MASK_BUFFER_SIZE];

    constexpr IORamMasks() {
      u32 *read_mask_data_u32 = (u32 *)read_mask_data;
      u32 *write_mask_data_u32 = (u32 *)write_mask_data;

      for (int offset = 0; offset < MASK_BUFFER_SIZE / sizeof(u32); offset++) {
        read_mask_data_u32[offset] = read_mask_at_offset(offset);
        write_mask_data_u32[offset] = write_mask_at_offset(offset);
      }
    }

    u64 write_mask_at(u32 offset) const {
      return *(u64 *)&write_mask_data[offset];
    }
    u64 read_mask_at(u32 offset) const {
      return *(u64 *)&read_mask_data[offset];
    }

    static constexpr u32 read_mask_at_offset(u32 offset) {
      switch (offset) {
      case DISPSTAT:
        return (VerticalCounter::READ_MASK << 16) | 0xFFFF;
      case BG0HOFS:
      case BG0VOFS:
      case BG1HOFS:
      case BG1VOFS:
      case BG2HOFS:
      case BG2VOFS:
      case BG3HOFS:
      case BG3VOFS:
      case BG2PA:
      case BG2PB:
      case BG2PC:
      case BG2PD:
      case BG2X:
      case BG2Y:
      case BG3PA:
      case BG3PB:
      case BG3PC:
      case BG3PD:
      case BG3X:
      case BG3Y:
      case WIN0H:
      case WIN1H:
      case WIN0V:
      case WIN1V:
      case MOSAIC:
        return 0;
      default:
        return ~0;
      }
    }

    static constexpr u32 write_mask_at_offset(u32 offset) {
      switch (offset) {
      case VCOUNT:
      case DISPSTAT:
        return (u32)DisplayStatus::WRITE_MASK;
      case BG0CNT:
      case BG1CNT:
      case BG2CNT:
      case BG3CNT:
        return BGControl::WRITE_MASK | (BGControl::WRITE_MASK << 16);
      default:
        return ~0;
      }
    }
  };

  struct IORam {

    IORamMasks masks;

    u8 io_ram[IO_RAM_SIZE + 8] = {};
    u8 io_ram_ext[16] = {};

    void write(u32 offset, u64 value, u32 width) {
      u8 *target = nullptr;
      u64 mask;

      if (offset < 0x412) {
        target = &io_ram[offset];
        mask = masks.write_mask_at(offset);
      } else if ((offset & 0xFFFF) < 0x808 && (offset & 0xFFFF) >= 0x800) {
        target = &io_ram_ext[offset & 0xF];
        mask = ~0;
      }

      if (target == nullptr) {
        return;
      }

      u64 masked_value = value & mask;

      switch (width) {
      case 1:
        *target = masked_value;
        break;
      case 2:
        *(u16 *)target = (u16)masked_value;
        break;
      case 4:
        *(u32 *)target = (u32)masked_value;
        break;
      case 8:
        *(u64 *)target = (u64)masked_value;
        break;
      }
    }

    u64 read(u32 offset, u32 width) const {
      u8 const *target = nullptr;
      u64 mask;

      if (offset < 0x412) {
        target = &io_ram[offset];
        mask = masks.read_mask_at(offset);
      } else if ((offset & 0xFFFF) < 0x808 && (offset & 0xFFFF) >= 0x800) {
        target = &io_ram_ext[offset & 0xF];
        mask = ~0;
      }

      if (target == nullptr) {
        return 0xFEEDBEEF;
      }

      return *target & mask;
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
