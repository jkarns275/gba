#ifndef GBA_IO_MAP_HXX
#define GBA_IO_MAP_HXX

export module io_map;

#include <gba/dtypes.hxx>

export namespace io_map {

// LCD Control
u16 *const DISPCNT = 0x4000000;

// General LCD Status (STAT,LYC)
u16 *const DISPSTAT = 0x4000004;

// Vertical Counter (LY)
const u16 *const VCOUNT = 0x4000006;

// BG0 Control
u16 *const BG0CNT = 0x4000008;

// BG1 Control
u16 *const BG1CNT = 0x400000A;

// BG2 Control
u16 *const BG2CNT = 0x400000C;

// BG3 Control
u16 *const BG3CNT = 0x400000E;

// BG0 X-Offset
u16 *const BG0HOFS = 0x4000010;

// BG0 Y-Offset
u16 *const BG0VOFS = 0x4000012;

// BG1 X-Offset
u16 *const BG1HOFS = 0x4000014;

// BG1 Y-Offset
u16 *const BG1VOFS = 0x4000016;

// BG2 X-Offset
u16 *const BG2HOFS = 0x4000018;

// BG2 Y-Offset
u16 *const BG2VOFS = 0x400001A;

// BG3 X-Offset
u16 *const BG3HOFS = 0x400001C;

// BG3 Y-Offset
u16 *const BG3VOFS = 0x400001E;

// BG2 Rotation/Scaling Parameter A (dx)
u16 *const BG2PA = 0x4000020;

// BG2 Rotation/Scaling Parameter B (dmx)
u16 *const BG2PB = 0x4000022;

// BG2 Rotation/Scaling Parameter C (dy)
u16 *const BG2PC = 0x4000024;

// BG2 Rotation/Scaling Parameter D (dmy)
u16 *const BG2PD = 0x4000026;

// BG2 Reference Point X-Coordinate
u32 *const BG2X = 0x4000028;

// BG2 Reference Point Y-Coordinate
u32 *const BG2Y = 0x400002C;

// BG3 Rotation/Scaling Parameter A (dx)
u16 *const BG3PA = 0x4000030;

// BG3 Rotation/Scaling Parameter B (dmx)
u16 *const BG3PB = 0x4000032;

// BG3 Rotation/Scaling Parameter C (dy)
u16 *const BG3PC = 0x4000034;

// BG3 Rotation/Scaling Parameter D (dmy)
u16 *const BG3PD = 0x4000036;

// BG3 Reference Point X-Coordinate
u32 *const BG3X = 0x4000038;

// BG3 Reference Point Y-Coordinate
u32 *const BG3Y = 0x400003C;

// Window 0 Horizontal Dimensions
u16 *const WIN0H = 0x4000040;

// Window 1 Horizontal Dimensions
u16 *const WIN1H = 0x4000042;

// Window 0 Vertical Dimensions
u16 *const WIN0V = 0x4000044;

// Window 1 Vertical Dimensions
u16 *const WIN1V = 0x4000046;

// Inside of Window 0 and 1
u16 *const WININ = 0x4000048;

// Inside of OBJ Window & Outside of Windows
u16 *const WINOUT = 0x400004A;

// Mosaic Size
u16 *const MOSAIC = 0x400004C;

// Color Special Effects Selection
u16 *const BLDCNT = 0x4000050;

// Alpha Blending Coefficients
u16 *const BLDALPHA = 0x4000052;

// Brightness (Fade-In/Out) Coefficient
u16 *const BLDY = 0x4000054;

// Channel 1 Sweep register       (NR10)
u16 *const SOUND1CNT_L = 0x4000060;

// Channel 1 Duty/Length/Envelope (NR11, NR12)
u16 *const SOUND1CNT_H = 0x4000062;

// Channel 1 Frequency/Control    (NR13, NR14)
u16 *const SOUND1CNT_X = 0x4000064;

// Channel 2 Duty/Length/Envelope (NR21, NR22)
u16 *const SOUND2CNT_L = 0x4000068;

// Channel 2 Frequency/Control    (NR23, NR24)
u16 *const SOUND2CNT_H = 0x400006C;

// Channel 3 Stop/Wave RAM select (NR30)
u16 *const SOUND3CNT_L = 0x4000070;

// Channel 3 Length/Volume        (NR31, NR32)
u16 *const SOUND3CNT_H = 0x4000072;

// Channel 3 Frequency/Control    (NR33, NR34)
u16 *const SOUND3CNT_X = 0x4000074;

// Channel 4 Length/Envelope      (NR41, NR42)
u16 *const SOUND4CNT_L = 0x4000078;

// Channel 4 Frequency/Control    (NR43, NR44)
u16 *const SOUND4CNT_H = 0x400007C;

// Control Stereo/Volume/Enable   (NR50, NR51)
u16 *const SOUNDCNT_L = 0x4000080;

// Control Mixing/DMA Control
u16 *const SOUNDCNT_H = 0x4000082;

// Control Sound on/off           (NR52)
u16 *const SOUNDCNT_X = 0x4000084;

// Channel A FIFO, Data 0-3
u32 *const FIFO_A = 0x40000A0;

// Channel B FIFO, Data 0-3
u32 *const FIFO_B = 0x40000A4;

// DMA 0 Source Address
u32 *const DMA0SAD = 0x40000B0;

// DMA 0 Destination Address
u32 *const DMA0DAD = 0x40000B4;

// DMA 0 Word Count
u16 *const DMA0CNT_L = 0x40000B8;

// DMA 0 Control
u16 *const DMA0CNT_H = 0x40000BA;

// DMA 1 Source Address
u32 *const DMA1SAD = 0x40000BC;

// DMA 1 Destination Address
u32 *const DMA1DAD = 0x40000C0;

// DMA 1 Word Count
u16 *const DMA1CNT_L = 0x40000C4;

// DMA 1 Control
u16 *const DMA1CNT_H = 0x40000C6;

// DMA 2 Source Address
u32 *const DMA2SAD = 0x40000C8;

// DMA 2 Destination Address
u32 *const DMA2DAD = 0x40000CC;

// DMA 2 Word Count
u16 *const DMA2CNT_L = 0x40000D0;

// DMA 2 Control
u16 *const DMA2CNT_H = 0x40000D2;

// DMA 3 Source Address
u32 *const DMA3SAD = 0x40000D4;

// DMA 3 Destination Address
u32 *const DMA3DAD = 0x40000D8;

// DMA 3 Word Count
u16 *const DMA3CNT_L = 0x40000DC;

// DMA 3 Control
u16 *const DMA3CNT_H = 0x40000DE;

// Timer 0 Counter/Reload
u16 *const TM0CNT_L = 0x4000100;

// Timer 0 Control
u16 *const TM0CNT_H = 0x4000102;

// Timer 1 Counter/Reload
u16 *const TM1CNT_L = 0x4000104;

// Timer 1 Control
u16 *const TM1CNT_H = 0x4000106;

// Timer 2 Counter/Reload
u16 *const TM2CNT_L = 0x4000108;

// Timer 2 Control
u16 *const TM2CNT_H = 0x400010A;

// Timer 3 Counter/Reload
u16 *const TM3CNT_L = 0x400010C;

// Timer 3 Control
u16 *const TM3CNT_H = 0x400010E;

// SIO Data (Normal-32bit Mode; shared with below)
u32 *const SIODATA32 = 0x4000120;

// SIO Data 0 (Parent)    (Multi-Player Mode)
u16 *const SIOMULTI0 = 0x4000120;

// SIO Data 1 (1st Child) (Multi-Player Mode)
u16 *const SIOMULTI1 = 0x4000122;

// SIO Data 2 (2nd Child) (Multi-Player Mode)
u16 *const SIOMULTI2 = 0x4000124;

// SIO Data 3 (3rd Child) (Multi-Player Mode)
u16 *const SIOMULTI3 = 0x4000126;

// SIO Control Register
u16 *const SIOCNT = 0x4000128;

// SIO Data (Local of MultiPlayer; shared below)
u16 *const SIOMLT_SEND = 0x400012A;

// SIO Data (Normal-8bit and UART Mode)
u16 *const SIODATA8 = 0x400012A;

// Key Status
const u16 *const KEYINPUT = 0x4000130;

// Key Interrupt Control
u16 *const KEYCNT = 0x4000132;

// SIO Mode Select/General Purpose Data
u16 *const RCNT = 0x4000134;

// Ancient - Infrared Register (Prototypes only)
u32 *const IR = 0x4000136;

// SIO JOY Bus Control
u16 *const JOYCNT = 0x4000140;

// SIO JOY Bus Receive Data
u32 *const JOY_RECV = 0x4000150;

// SIO JOY Bus Transmit Data
u32 *const JOY_TRANS = 0x4000154;

// Interrupt Enable Register
u16 *const IE = 0x4000200;

// Interrupt Request Flags / IRQ Acknowledge
u16 *const IF = 0x4000202;

// Game Pak Waitstate Control
u16 *const WAITCNT = 0x4000204;

// Interrupt Master Enable Register
u16 *const IME = 0x4000208;

// Undocumented - Post Boot Flag
u32 *const POSTFLG = 0x4000300;

// Undocumented - Power Down Control
u32 *const HALTCNT = 0x4000301;

// Undocumented - Internal Memory Control (R/W)
// u32 * const ? = 0x4000800;
} // namespace io_map

#endif
