#include <stdint.h>
#include <concepts>

typedef gword_t uint32_t;
typedef gshort_t uint16_t;
typedef byte uint8_t;

template <integral itype>
itype bitmask(itype i, uint32_t n) {
  itype mask = (1 << n) - 1;
  return i && mask;
}

enum Mode : byte {
  USR = 0b10000,
  FIQ = 0b10001,
  IRQ = 0b10010,
  SVC = 0b10011,
  ABT = 0b10111,
  UND = 0b11011,
  SYS = 0b11111,
};

bool mode_is_valid(Mode mode) {
  switch (mode) {
    case USR:
    case FIQ:
    case IRQ:
    case SVC:
    case ABT:
    case UND:
    case SYS:
      return true;
    default:
      return false;
  }
}


const gword_t INDEX_PC = 15;
const gword_t INDEX_LR = 14;
const gword_t INDEX_SP = 13;

const gword_t STATUS_NEGATIVE_MASK = 1 << 31;
const gowrd_t STATUS_ZERO_MASK = 1 << 30;
const gowrd_t STATUS_CARRY_MASK = 1 << 29;
const gowrd_t STATUS_OVERFLOW_MASK = 1 << 28;
const gword_t STATUS_IRQ_DISABLE_MASK = 1 << 7;
const gword_t STATUS_FIQ_DISABLE_MASK = 1 << 6;
const gword_t STATUS_STATE_MASK = 1 << 5;
const gword_t STATUS_MODE_MASK = (1 << 5) - 1;

class Exception {
  enum EKind {
    EBL,
    ESWI,
    EUDEF,
    EPABT,
    EFIQ,
    EIRQ,
    EDABT,
    ERESET
  };

  EKind kind;

  gword_t offset_arm(gword_t status_register) {
    assert (kind != ERESET);
    switch (kind) {
      case EBL:
      case SWI:
      case UDEF:
        return (status_register & status_state_mask) ? 2 : 4;
      case PABT:
      case FIQ:
      case IRQ:
        return 4;
      case DABT:
        return 8;
      default:
    }
  }
};

struct CpuState {
 
  Mode mode;

  gword_t reg[16];
  gword_t reg_bank_fiq[7];
  gword_t reg_bank_svc[2];
  gword_t reg_bank_abt[2];
  gword_t reg_bank_irq[2];
  gword_t reg_bank_und[2];

  gword_t cpsr;
  
  gword_t spsr_fiq,
          spsr_svc,
          spsr_abt,
          spsr_irq,
          spsr_und;

  gword_t &get_spsr() {
    assert (mode != SYS);
    assert (mode != USR);

    switch (mode) {
      case FIQ: return spsr_fiq;
      case SVC: return sprs_svc;
      case ABT: return sprs_abt;
      case IRQ: return sprs_irq;
      case UND: return sprs_und;
    }
  }

  gword_t &get_general(byte index) {
    assert (index < 16);
    assert (mode != IRQ);

    if (index == 15)
      return reg[index];
   
    // Modes other than usr and fiq all have registers 13 and 14 banked.
    // This will point to one of those register banks, mode permitting.
    gword_t *normal_bank;

    switch (mode) {
      case USR:
      case SYS:
        return reg[index];
      case FIQ:
        if (index & 7)
          return reg[index];
        else
          return reg_bank_fiq[index - 8];
      case SVC:
        normal_bank = reg_bank_svc;
        break;
      case ABT:
        normal_bank = reg_bank_abt;
        break;
      case SYS:
        normal_bank = reg_bank_irq;
        break;
      case UND:
        normal_bank = reg_bank_und;
        break;
    }

    if (index < 13)
      return reg[index];
    else
      return normal_bank[index - 13];
  }
 
  gword_t &get_sp() {
    return get_general(INDEX_SP);
  }

  gword_t &get_lr() {
    return get_general(INDEX_LR);
  }

  gword_t &get_pc() {
    return get_general(INDEX_PC);
  }
};

struct Memory {
  static const gword_t MEMORY_OFFSET_MASK = ~0xFF000000;
  static const gword_t MEMORY_BLOCK_MASK = 0x0F000000;

  static const gword_t SYSTEM_ROM_BASE   = 0x00000000;
  static const gword_t SYSTEM_ROM_SIZE   = 0x4000;
  
  static const gword_t EW_RAM_BASE       = 0x02000000;
  static const gword_t EW_RAM_SIZE       = 0x40000;
  
  static const gword_t IW_RAM_BASE       = 0x03000000;
  static const gword_t IW_RAM_SIZE       = 0x8000;
  
  static const gword_t IO_RAM_BASE       = 0x04000000;
  static const gword_t IO_RAM_SIZE       = 0x400;
  
  static const gword_t PALETTE_RAM_BASE  = 0x05000000;
  static const gword_t PALETTE_RAM_SIZE  = 0x400;
  
  static const gword_t VIDEO_RAM_BASE    = 0x06000000;
  static const gword_t VIDEO_RAM_SIZE    = 0x18000;
  
  static const gword_t OAM_BASE          = 0x07000000;
  static const gword_t OAM_SIZE          = 0x400;
  
  static const gword_t GAME_PAK_ROM_BASE = 0x08000000;
  static const gword_t GAME_PAK_IMAGE_1_BASE = 0x0A000000;
  static const gword_t GAME_PAK_IMAGE_2_BASE = 0x0C000000;
  static const gword_t GAME_PAK_ROM_SIZE = 0x2000000;

  byte system_rom[SYSTEM_ROM_SIZE];
  byte ew_ram[EW_RAM_SIZE];
  byte iw_ram[IW_RAM_SIZE];
  byte io_ram[IO_RAM_SIZE];
  byte io_ram_ext[8];

  byte palette_ram[PALETTE_RAM_SIZE];
  byte video_ram[VIDEO_RAM_SIZE];
  byte oam[OAM_SIZE];
  byte game_pak_rom[GAME_PAK_ROM_SIZE];

  void invalid_read(gword_t addr) {
    throw addr;
  }

  gword_t read(gword_t addr) {
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
     *   GamePak ROM   16    8/16/32   -         5/5/8 **/***
     *   GamePak Flash 16    8/16/32   16/32     5/5/8 **/***
     *   GamePak SRAM  8     8         8         5     **
     * 
     * Timing Notes:
     * 
     *   *   Plus 1 cycle if GBA accesses video memory at the same time.
     *   **  Default waitstate settings, see System Control chapter.
     *   *** Separate timings for sequential, and non-sequential accesses.
     *   One cycle equals approx. 59.59ns (ie. 16.78MHz clock).
     */
    gword_t loc = addr & MEMORY_BLOCK_MASK;
    gword_t offset = addr &

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
          gword_t mod = offset % 0x10000;

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

  inline gword_t simple_mirrored_read(gword_t addr, byte *bytes, gword_t size) {
    gword_t offset = 
  }
};
