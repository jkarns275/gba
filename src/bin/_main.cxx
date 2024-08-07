#include <stdint.h>
#include <concepts>

import types;
import bitutil;

struct Memory {
  static const u32 MEMORY_OFFSET_MASK = ~0xFF000000;
  static const u32 MEMORY_BLOCK_MASK = 0xFF000000;

  static const u32 SYSTEM_ROM_BASE = 0x00000000;
  static const u32 SYSTEM_ROM_SIZE = 0x4000;

  static const u32 EW_RAM_BASE = 0x02000000;
  static const u32 EW_RAM_SIZE = 0x40000;

  static const u32 IW_RAM_BASE = 0x03000000;
  static const u32 IW_RAM_SIZE = 0x8000;

  static const u32 IO_RAM_BASE = 0x04000000;
  static const u32 IO_RAM_SIZE = 0x400;

  static const u32 PALETTE_RAM_BASE = 0x05000000;
  static const u32 PALETTE_RAM_SIZE = 0x400;

  static const u32 VIDEO_RAM_BASE = 0x06000000;
  static const u32 VIDEO_RAM_SIZE = 0x18000;

  static const u32 OAM_BASE = 0x07000000;
  static const u32 OAM_SIZE = 0x400;

  static const u32 GAME_PAK_ROM_BASE = 0x08000000;
  static const u32 GAME_PAK_IMAGE_1_BASE = 0x0A000000;
  static const u32 GAME_PAK_IMAGE_2_BASE = 0x0C000000;
  static const u32 GAME_PAK_ROM_SIZE = 0x2000000;

  u8 system_rom[SYSTEM_ROM_SIZE];
  u8 ew_ram[EW_RAM_SIZE];
  u8 iw_ram[IW_RAM_SIZE];
  u8 io_ram[IO_RAM_SIZE];
  u8 io_ram_ext[8];

  u8 palette_ram[PALETTE_RAM_SIZE];
  u8 video_ram[VIDEO_RAM_SIZE];
  u8 oam[OAM_SIZE];
  u8 game_pak_rom[GAME_PAK_ROM_SIZE];

  void invalid_read(u32 addr) { throw addr; }

  u32 read(u32 addr) {
    /**
     * Information for memory access timing emulation.
     *
     * Address Bus Width and CPU Read/Write Access Widths
     * Shows the Bus-Width, supported read and write widths, and the clock cycles for 8/16/32bit accesses.
     * 
     *   Region        Bus   Read      Write     Cycles
     *   BIOS ROM      32    8/16/32   -         1/1/1
     *   Work RAM 32K  32    8/16/32   8/16/32   1/1/1
     *   I/O           32    8/16/32   8/16/32   1/1/1
     *   OAM           32    8/16/32   16/32     1/1/1 *
     *   Work RAM 256K 16    8/16/32   8/16/32   3/3/6 **
     *   Palette RAM   16    8/16/32   16/32     1/1/2 *
     *   VRAM          16    8/16/32   16/32     1/1/2 *
     *   GamePak ROM   16    8/16/32   -         5/5/8 ** / ***
     *   GamePak Flash 16    8/16/32   16/32     5/5/8 ** / ***
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
        // TODO: If we read past 0x0003FFF, we need to return the next instruction.
        // for some reason that is what the console does.
        if (offset < SYSTEM_ROM_SIZE)
          return system_rom[offset];
        else
          return 0;

      case EW_RAM_BASE:
        return ew_ram[offset % EW_RAM_SIZE];
        
      case IW_RAM_BASE:
        return iw_ram[offset % IW_RAM_SIZE];

      case IO_RAM_BASE:
        if (offset < IO_RAM_SIZE) {
          return io_ram[offset];
        } else {
          u32 mod = offset % 0x10000;

          if (mod == 0x800)
            return io_ram_ext[0];
          else if (offset == 0x804)
            return io_ram_ext[4];
          else
            invalid_read(addr);
        }

      case PALETTE_RAM_BASE:
        return palette_ram[offset % PALETTE_RAM_SIZE];

      case VIDEO_RAM_BASE:
        return video_ram[offset % VIDEO_RAM_SIZE];

      case OAM_BASE:
        return oam[offset % OAM_SIZE];

      // These are all the same but they have different wait states
      case GAME_PAK_ROM_BASE:
      case GAME_PAK_IMAGE_1_BASE:
      case GAME_PAK_IMAGE_2_BASE:
        return game_pak_rom[offset];
    }
  }

  inline u32 simple_mirrored_read(u32 addr, u8 *u8s, u32 size) {
    u32 offset = 0;
  }
};
