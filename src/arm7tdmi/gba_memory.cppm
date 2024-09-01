module;
#include <assert.h>
#include <fstream>
#include <iostream>
#include <optional>
#include <string>

#include <spdlog/spdlog.h>

export module arm7tdmi:gba_memory;

import types;

import :memory;
import :mode;
import :cpu_state;

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
    static constexpr u32 IO_RAM_SIZE = 0x400;

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

    u8 system_rom[SYSTEM_ROM_SIZE];
    u8 ew_ram[EW_RAM_SIZE] = {};
    u8 iw_ram[IW_RAM_SIZE] = {};
    u8 io_ram[IO_RAM_SIZE] = {};
    u8 io_ram_ext[8] = {};

    u8 palette_ram[PALETTE_RAM_SIZE] = {};
    u8 video_ram[VIDEO_RAM_SIZE] = {};
    u8 oam[OAM_SIZE] = {};
    u8 game_pak_rom[GAME_PAK_ROM_SIZE] = {};
    u8 game_pak_flash[GAME_PAK_FLASH_SIZE] = {};
    u8 junk[0xFFFF] = {};

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

    u8 *_junk() {
      u8 *r = &junk[0];
      *reinterpret_cast<u32 *>(r) = 0;
      return r;
    }

    u8 *_u8_at_impl(u32 addr, u32 width, Mode mode) {
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
      u32 loc = addr & MEMORY_BLOCK_MASK;
      u32 offset = addr & MEMORY_OFFSET_MASK;

      switch (loc) {
      case SYSTEM_ROM_BASE:
        cpu_state->cycles(1);
        // TODO: If we read past 0x0003FFF, we need to return the next
        // instruction. for some reason that is what the console does.
        if (offset < SYSTEM_ROM_SIZE) {
          return &system_rom[offset];
        } else {
          u8 *r = &junk[0];
          *reinterpret_cast<u32 *>(r) = 0;
          return r;
        }

      case EW_RAM_BASE:
        cpu_state->cycles(width > 2 ? 6 : 3);
        return &ew_ram[offset % EW_RAM_SIZE];

      case IW_RAM_BASE:
        cpu_state->cycles(1);
        return &iw_ram[offset % IW_RAM_SIZE];

      case IO_RAM_BASE:
        cpu_state->cycles(1);
        if (offset < IO_RAM_SIZE) {
          return &io_ram[offset];
        } else {
          u32 mod = offset % 0x10000;

          if (mod == 0x800)
            return &io_ram_ext[0];
          else if (offset == 0x804)
            return &io_ram_ext[4];
          else
            break;
        }

      case PALETTE_RAM_BASE:
        cpu_state->cycles(1 + (width >> 2));
        return &palette_ram[offset % PALETTE_RAM_SIZE];

      case VIDEO_RAM_BASE:
        cpu_state->cycles(1 + (width >> 2));
        return &video_ram[offset % VIDEO_RAM_SIZE];

      case OAM_BASE:
        cpu_state->cycles(1);
        return &oam[offset % OAM_SIZE];

      // These are all the same but they have different wait states
      case GAME_PAK_ROM_BASE:
      case GAME_PAK_IMG_1_BASE:
      case GAME_PAK_IMG_2_BASE:
        cpu_state->cycles(width > 2 ? 8 : 5);
        return &game_pak_rom[offset];
      case GAME_PAK_FLASH_BASE:
        cpu_state->cycles(5 * width);
        return &game_pak_flash[offset];
      }

      spdlog::critical("Read requested from invalid memory address: {}", addr);
      return &junk[0];
    }

    // TODO: Do something when we try to write to ROM
    u8 *u8_at(u32 addr, u32 width, Mode mode) override {
      return _u8_at_impl(addr, width, mode);
    }
    const u8 *const_u8_at(u32 addr, u32 width, Mode mode) override {
      return _u8_at_impl(addr, width, mode);
    }
  };
}
