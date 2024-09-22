module;
#include <assert.h>
#include <fstream>
#include <iostream>
#include <optional>
#include <string>

#include <spdlog/spdlog.h>

export module gba.memory;

import gba.io;
import arm7tdmi;
import types;

using std::optional;
using std::string;

export {
  ;

  struct GBAMemory : public Memory {
    static constexpr u32 MEMORY_OFFSET_MASK = ~0xFF000000;
    static constexpr u32 MEMORY_BLOCK_MASK = 0x0F000000;

    static constexpr u32 SYSTEM_ROM_BASE = 0x00000000;
    static constexpr u32 SYSTEM_ROM_SIZE = 0x4000;

    static constexpr u32 EW_RAM_BASE = 0x02000000;
    static constexpr u32 EW_RAM_SIZE = 0x40000;

    static constexpr u32 IW_RAM_BASE = 0x03000000;
    static constexpr u32 IW_RAM_SIZE = 0x8000;

    static constexpr u32 IO_RAM_BASE = 0x04000000;

    static constexpr u32 PALETTE_RAM_BASE = 0x05000000;
    static constexpr u32 PALETTE_RAM_SIZE = 0x400;

    static constexpr u32 VIDEO_RAM_BASE = 0x06000000;
    static constexpr u32 VIDEO_RAM_SIZE = 0x18000;

    static constexpr u32 OAM_BASE = 0x07000000;
    static constexpr u32 OAM_SIZE = 0x400;

    static constexpr u32 GAME_PAK_ROM_BASE = 0x08000000;
    static constexpr u32 GAME_PAK_IMG_1_BASE = 0x0A000000;
    static constexpr u32 GAME_PAK_IMG_2_BASE = 0x0C000000;
    static constexpr u32 GAME_PAK_ROM_SIZE = 0x2000000;

    static constexpr u32 GAME_PAK_FLASH_BASE = 0x0E000000;
    static constexpr u32 GAME_PAK_FLASH_SIZE = 0x10000;

    u8 system_rom[SYSTEM_ROM_SIZE + 8];
    u8 ew_ram[EW_RAM_SIZE + 8] = {};
    u8 iw_ram[IW_RAM_SIZE + 8] = {};
    // Lots of read and write masking going on w/ memory mapped io registers.
    IORam io_ram;

    u8 palette_ram[PALETTE_RAM_SIZE + 8] = {};
    u8 video_ram[VIDEO_RAM_SIZE + 8] = {};
    u8 oam[OAM_SIZE + 8] = {};
    u8 game_pak_rom[GAME_PAK_ROM_SIZE + 8] = {};
    u8 game_pak_flash[GAME_PAK_FLASH_SIZE + 8] = {};

    GBAMemory(optional<string> bios_path) : Memory() {
      if (bios_path == std::nullopt)
        return;

      spdlog::info("BIOS Path: {}", *bios_path);
      std::ifstream file(*bios_path, std::ios_base::binary);

      file.seekg(0, std::ios::end);
      size_t length = file.tellg();
      file.seekg(0, std::ios::beg);

      spdlog::info("BIOS size: {}", length);

      if (length > SYSTEM_ROM_SIZE) {
        spdlog::critical("Supplied bios file is too big!");
        spdlog::critical("File size: {}", length);
        spdlog::critical("System ROM size: {}", SYSTEM_ROM_SIZE);
        exit(1);
      }

      file.read((char *)&system_rom[0], length);
      spdlog::info("Read BIOS into system rom: {}", length);
    }

    /**
     * Information for memory access timing emulation.
     *
     * Address Bus Width and CPU Read/Write Access Widths
     * Shows the Bus-Width, supported read and write widths, and the clock
     * cycles for 8/16/32bit accesses.
     *
     *   Region        Bus   Read      Write     Cycles
     *   BIOS ROM      32    8/16/32   -         1/1/1
     *   Work RAM 256K 16    8/16/32   8/16/32   3/3/6 **
     *   Work RAM 32K  32    8/16/32   8/16/32   1/1/1
     *   I/O           32    8/16/32   8/16/32   1/1/1
     *   Palette RAM   16    8/16/32   16/32     1/1/2 *
     *   VRAM          16    8/16/32   16/32     1/1/2 *
     *   OAM           32    8/16/32   16/32     1/1/1 *
     *   GamePak ROM   16    8/16/32   -         5/5/8 **
     *   GamePak Flash 16    8/16/32   16/32     5/5/8
     *   GamePak SRAM  8     8         8         5     **
     *
     * Timing Notes:
     *
     *   *   Plus 1 cycle if GBA accesses video memory at the same time.
     *   **  Default waitstate settings, see System Control chapter.
     *   *** Separate timings for sequential, and non-sequential accesses.
     *   One cycle equals approx. 59.59ns (ie. 16.78MHz clock).
     */

    // Used to avoid weirdness with endianness.
    u64 read_at(const u8 *src, u32 width) const {
      switch (width) {
      case 1:
        return *src;
      case 2:
        return *(const u16 *)src;
      case 3:
        return *(const u32 *)src;
      case 4:
        return *(const u64 *)src;
      }
    }

    u64 read_impl(u32 addr, u32 width, Mode mode) const override {
      u32 loc = addr & MEMORY_BLOCK_MASK;
      u32 offset = addr & MEMORY_OFFSET_MASK;

      const u8 *p = nullptr;

      switch (loc) {
      case SYSTEM_ROM_BASE:
        cpu_state->cycles(1);
        // TODO: If we read past 0x0003FFF, we need to return the next
        // instruction. for some reason that is what the console does.
        if (offset < SYSTEM_ROM_SIZE) {
          p = &system_rom[offset];
        } else {
          return 0;
        }
        break;

      case EW_RAM_BASE:
        cpu_state->cycles(width > 2 ? 6 : 3);
        p = &ew_ram[offset % EW_RAM_SIZE];
        break;

      case IW_RAM_BASE:
        cpu_state->cycles(1);
        p = &iw_ram[offset % IW_RAM_SIZE];
        break;

      case IO_RAM_BASE:
        cpu_state->cycles(1);

        return io_ram.read(offset, width);
        // There are no fields that are "write only", so we can read this data
        // normally.

      case PALETTE_RAM_BASE:
        cpu_state->cycles(1 + (width >> 2));
        p = &palette_ram[offset % PALETTE_RAM_SIZE];
        break;

      case VIDEO_RAM_BASE:
        cpu_state->cycles(1 + (width >> 2));
        p = &video_ram[offset % VIDEO_RAM_SIZE];
        break;

      case OAM_BASE:
        cpu_state->cycles(1);
        p = &oam[offset % OAM_SIZE];
        break;

      // These are all the same but they have different wait states
      case GAME_PAK_ROM_BASE:
      case GAME_PAK_IMG_1_BASE:
      case GAME_PAK_IMG_2_BASE:
        cpu_state->cycles(width > 2 ? 8 : 5);
        p = &game_pak_rom[offset];
        break;
      case GAME_PAK_FLASH_BASE:
        cpu_state->cycles(5 * width);
        p = &game_pak_flash[offset];
        break;
      default:
        spdlog::critical("Read requested from invalid memory address: {}",
                         addr);
        return 0;
      }

      assert(p != nullptr);

      return read_at(p, width);
    }

    void write_at(u8 *dst, u64 value, u32 width) {
      switch (width) {
      case 1:
        *dst = (u8)value;
        break;
      case 2:
        *(u16 *)dst = (u16)value;
        break;
      case 4:
        *(u32 *)dst = (u32)value;
        break;
      case 8:
        *(u64 *)dst = value;
        break;
      }
    }

    void write_impl(u32 addr, u64 value, u32 width, Mode mode) override {
      u32 loc = addr & MEMORY_BLOCK_MASK;
      u32 offset = addr & MEMORY_OFFSET_MASK;

      u8 *p = nullptr;

      switch (loc) {
      case SYSTEM_ROM_BASE:
        // No writing to ROM
        cpu_state->cycles(1);
        return;
      case EW_RAM_BASE:
        cpu_state->cycles(width > 2 ? 6 : 3);
        p = &ew_ram[offset % EW_RAM_SIZE];
        break;

      case IW_RAM_BASE:
        cpu_state->cycles(1);
        p = &iw_ram[offset % IW_RAM_SIZE];
        break;

      case IO_RAM_BASE:
        cpu_state->cycles(1);
        io_ram.write(offset, value, width);
        return;
        // if (offset < IO_RAM_SIZE) {
        //   p = &io_ram[offset];
        // } else {
        //   u32 mod = offset % 0x10000;

        //   if (mod == 0x800)
        //     p = &io_ram_ext[0];
        //   else if (offset == 0x804)
        //     p = &io_ram_ext[4];
        //   else
        //     return;
        // }
        // break;

      case PALETTE_RAM_BASE:
        cpu_state->cycles(1 + (width >> 2));
        p = &palette_ram[offset % PALETTE_RAM_SIZE];
        break;

      case VIDEO_RAM_BASE:
        cpu_state->cycles(1 + (width >> 2));
        p = &video_ram[offset % VIDEO_RAM_SIZE];
        break;

      case OAM_BASE:
        cpu_state->cycles(1);
        p = &oam[offset % OAM_SIZE];
        break;

      // These are all the same but they have different wait states
      case GAME_PAK_ROM_BASE:
      case GAME_PAK_IMG_1_BASE:
      case GAME_PAK_IMG_2_BASE:
        cpu_state->cycles(width > 2 ? 8 : 5);
        return;
      case GAME_PAK_FLASH_BASE:
        cpu_state->cycles(5 * width);
        p = &game_pak_flash[offset];
        break;
      default:
        spdlog::critical("Read requested from invalid memory address: {}",
                         addr);
        return;
      }

      write_at(p, value, width);
    }
  };
}
