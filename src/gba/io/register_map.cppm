module;

export module gba.io:register_map;

import bitutil;
import types;

export {
  ;

  // template <UnsignedInteger offset> struct RegisterDefinition {
  //   void write(itype new_value) {}
  //   itype read() { return 0; }

  //   constexpr itype read_mask() { return 0; }
  //   constexpr itype write_mask() { return 0; }
  // };

  // LCD Control
  const u32 DISPCNT = 0x4000000;

  // All of the structs defined here assume little endian hardware order,
  // matching the GBA. To support little endian we will have to add a switched
  // definition of the struct, reversing the order of the fields for big endian.

  struct __attribute__((packed)) DisplayControl {
    u8 mode : 3;
    bool cgb_mode : 1;
    u8 display_frame_select : 1;
    bool hblank_interval_free : 1;
    bool one_dimensional_vram_mapping : 1;
    bool forced_blank : 1;
    bool display_bg0 : 1;
    bool display_bg1 : 1;
    bool display_bg2 : 1;
    bool display_bg3 : 1;
    bool display_obj : 1;
    bool display_window0 : 1;
    bool display_window1 : 1;
    bool display_obj_window : 1;
  };

  static_assert(sizeof(DisplayControl) == 2);

  // Undocumented - Green Swap
  const u32 GRNSWP = 0x4000002;

  struct __attribute__((packed)) GreenSwap {
    bool swapped : 1;
    u16 not_used : 15;
  };

  static_assert(sizeof(GreenSwap) == 2);

  // General LCD Status (STAT,LYC)
  const u32 DISPSTAT = 0x4000004;

  struct __attribute__((packed)) DisplayStatus {
    static constexpr u16 WRITE_MASK = 0b1111111100111000;

    bool vblank_flag : 1;
    bool hblank_flag : 1;
    bool vcounter_flag : 1;
    bool vblank_irq_enable : 1;
    bool hblank_irq_enable : 1;
    bool vcounter_irq_enable : 1;
    u8 : 2; // Not used.
    u8 vcount_setting : 8;
  };

  static_assert(sizeof(DisplayStatus) == 2);

  // Vertical Counter (LY)
  const u32 VCOUNT = 0x4000006;

  struct __attribute__((packed)) VerticalCounter {
    static constexpr u16 WRITE_MASK = 0;
    static constexpr u16 READ_MASK = 0x1FF;

    u8 current_scanline;
    u8 not_used;
  };

  static_assert(sizeof(VerticalCounter) == 2);

  // BG0 Control
  const u32 BG0CNT = 0x4000008;

  // BG1 Control
  const u32 BG1CNT = 0x400000A;

  // BG2 Control
  const u32 BG2CNT = 0x400000C;

  // BG3 Control
  const u32 BG3CNT = 0x400000E;

  // Different in NDS
  struct __attribute__((packed)) BGControl {
    static constexpr u16 WRITE_MASK = 0b1111111111001111;

    u8 bg_priority : 2;
    u8 character_base_block : 2;
    u8 : 2; // Not used
    bool mosaic_enabled : 1;
    bool pallete_size_flag : 1;
    u8 screen_base_block : 5;
    bool display_area_overflow : 1;
    u8 screen_size : 2;
  };

  static_assert(sizeof(BGControl) == 2);

  // BG0 X-Offset
  const u32 BG0HOFS = 0x4000010;

  // BG0 Y-Offset
  const u32 BG0VOFS = 0x4000012;

  // BG1 X-Offset
  const u32 BG1HOFS = 0x4000014;

  // BG1 Y-Offset
  const u32 BG1VOFS = 0x4000016;

  // BG2 X-Offset
  const u32 BG2HOFS = 0x4000018;

  // BG2 Y-Offset
  const u32 BG2VOFS = 0x400001A;

  // BG3 X-Offset
  const u32 BG3HOFS = 0x400001C;

  // BG3 Y-Offset
  const u32 BG3VOFS = 0x400001E;

  struct __attribute__((packed)) BGOffset {
    static constexpr u16 READ_MASK = 0;

    u16 offset : 9;
    u8 padding : 7;
  };

  static_assert(sizeof(BGOffset) == 2);

  // BG2 Rotation/Scaling Parameter A (dx)
  const u32 BG2PA = 0x4000020;

  // BG2 Rotation/Scaling Parameter B (dmx)
  const u32 BG2PB = 0x4000022;

  // BG2 Rotation/Scaling Parameter C (dy)
  const u32 BG2PC = 0x4000024;

  // BG2 Rotation/Scaling Parameter D (dmy)
  const u32 BG2PD = 0x4000026;

  struct __attribute__((packed)) BGOrientation {
    static constexpr u16 READ_MASK = 0;

    u8 fractional : 8;
    u8 integer : 7;
    bool sign : 1;
  };

  static_assert(sizeof(BGOrientation) == 2);

  // BG2 Reference Point X-Coordinate
  const u32 BG2X = 0x4000028;

  // BG2 Reference Point Y-Coordinate
  const u32 BG2Y = 0x400002C;

  struct __attribute__((packed)) BGReferencePoint {
    static constexpr u32 READ_MASK = 0;

    u8 fractional : 8;
    u32 integer : 19;
    bool sign : 1;
    u8 not_used : 4;
  };

  static_assert(sizeof(BGReferencePoint) == 4);

  // BG3 Rotation/Scaling Parameter A (dx)
  const u32 BG3PA = 0x4000030;

  // BG3 Rotation/Scaling Parameter B (dmx)
  const u32 BG3PB = 0x4000032;

  // BG3 Rotation/Scaling Parameter C (dy)
  const u32 BG3PC = 0x4000034;

  // BG3 Rotation/Scaling Parameter D (dmy)
  const u32 BG3PD = 0x4000036;

  // BG3 Reference Point X-Coordinate
  const u32 BG3X = 0x4000038;

  // BG3 Reference Point Y-Coordinate
  const u32 BG3Y = 0x400003C;

  // Window 0 Horizontal Dimensions
  const u32 WIN0H = 0x4000040;

  // Window 1 Horizontal Dimensions
  const u32 WIN1H = 0x4000042;

  // Window 0 Vertical Dimensions
  const u32 WIN0V = 0x4000044;

  // Window 1 Vertical Dimensions
  const u32 WIN1V = 0x4000046;

  struct __attribute__((packed)) WindowDimension {
    static constexpr u16 READ_MASK = 0;

    u8 d2;
    u8 d1;
  };

  static_assert(sizeof(WindowDimension) == 2);

  // Inside of Window 0 and 1
  const u32 WININ = 0x4000048;

  struct __attribute__((packed)) WindowControl {
    bool bg0_enabled : 1;
    bool bg1_enabled : 1;
    bool bg2_enabled : 1;
    bool bg3_enabled : 1;
    bool obj_enabled : 1;
    bool color_effect_enabled : 1;
    u8 : 2; // Unused
  };

  static_assert(sizeof(WindowControl) == 1);

  struct __attribute__((packed)) InternalWindowControl {
    WindowControl w0;
    WindowControl w1;
  };

  static_assert(sizeof(InternalWindowControl) == 2);

  // Inside of OBJ Window & Outside of Windows
  const u32 WINOUT = 0x400004A;

  struct __attribute__((packed)) OuterWindowControl {
    WindowControl outside;
    WindowControl obj;
  };

  static_assert(sizeof(OuterWindowControl) == 2);

  // Mosaic Size
  const u32 MOSAIC = 0x400004C;

  struct __attribute__((packed)) MosaicSize {
    static constexpr u32 READ_MASK = 0;

    u8 bg_hsize : 4;
    u8 bg_vsize : 4;
    u8 obj_hsize : 4;
    u8 obj_vsize : 4;
    u32 : 16;
  };

  static_assert(sizeof(MosaicSize) == 4);

  // Color Special Effects Selection
  const u32 BLDCNT = 0x4000050;

  struct __attribute__((packed)) BlendControl {
    enum BlendEffect {
      NONE = 0,
      ALPHA = 1,
      BRIGHTNESS_INC = 2,
      BRIGHTNESS_DEC = 3
    };

    bool bg0_t0 : 1;
    bool bg1_t0 : 1;
    bool bg2_t0 : 1;
    bool bg3_t0 : 1;
    bool obj_t0 : 1;
    bool bd_t0 : 1;
    BlendEffect effect : 2;
    bool bg0_t1 : 1;
    bool bg1_t1 : 1;
    bool bg2_t1 : 1;
    bool bg3_t1 : 1;
    bool obj_t1 : 1;
    bool bd_t1 : 1;
    u8 : 2;
  };

  static_assert(sizeof(BlendControl) == 2);

  // Alpha Blending Coefficients
  const u32 BLDALPHA = 0x4000052;

  struct __attribute__((packed)) AlphaBlendingCoeff {
    u8 eva_coeff : 5;
    u8 : 3;
    u8 evb_coeff : 5;
    u8 : 3;
  };

  static_assert(sizeof(AlphaBlendingCoeff) == 2);

  // Brightness (Fade-In/Out) Coefficient
  const u32 BLDY = 0x4000054;

  // Write only
  struct __attribute__((packed)) BrightnessCoeff {
    static constexpr u32 READ_MASK = 0;

    u8 coeff : 5;
    u32 : 27;
  };

  static_assert(sizeof(BrightnessCoeff) == 4);

  // Channel 1 Sweep register       (NR10)
  const u32 SOUND1CNT_L = 0x4000060;

  // Channel 1 Duty/Length/Envelope (NR11, NR12)
  const u32 SOUND1CNT_H = 0x4000062;

  // Channel 1 Frequency/Control    (NR13, NR14)
  const u32 SOUND1CNT_X = 0x4000064;

  // Channel 2 Duty/Length/Envelope (NR21, NR22)
  const u32 SOUND2CNT_L = 0x4000068;

  // Channel 2 Frequency/Control    (NR23, NR24)
  const u32 SOUND2CNT_H = 0x400006C;

  // Channel 3 Stop/Wave RAM select (NR30)
  const u32 SOUND3CNT_L = 0x4000070;

  // Channel 3 Length/Volume        (NR31, NR32)
  const u32 SOUND3CNT_H = 0x4000072;

  // Channel 3 Frequency/Control    (NR33, NR34)
  const u32 SOUND3CNT_X = 0x4000074;

  // Channel 4 Length/Envelope      (NR41, NR42)
  const u32 SOUND4CNT_L = 0x4000078;

  // Channel 4 Frequency/Control    (NR43, NR44)
  const u32 SOUND4CNT_H = 0x400007C;

  // Control Stereo/Volume/Enable   (NR50, NR51)
  const u32 SOUNDCNT_L = 0x4000080;

  // Control Mixing/DMA Control
  const u32 SOUNDCNT_H = 0x4000082;

  // Control Sound on/off           (NR52)
  const u32 SOUNDCNT_X = 0x4000084;

  // Channel A FIFO, Data 0-3
  const u32 FIFO_A = 0x40000A0;

  // Channel B FIFO, Data 0-3
  const u32 FIFO_B = 0x40000A4;

  // DMA 0 Source Address
  const u32 DMA0SAD = 0x40000B0;

  // DMA 0 Destination Address
  const u32 DMA0DAD = 0x40000B4;

  // DMA 0 Word Count
  const u32 DMA0CNT_L = 0x40000B8;

  // DMA 0 Control
  const u32 DMA0CNT_H = 0x40000BA;

  // DMA 1 Source Address
  const u32 DMA1SAD = 0x40000BC;

  // DMA 1 Destination Address
  const u32 DMA1DAD = 0x40000C0;

  // DMA 1 Word Count
  const u32 DMA1CNT_L = 0x40000C4;

  // DMA 1 Control
  const u32 DMA1CNT_H = 0x40000C6;

  // DMA 2 Source Address
  const u32 DMA2SAD = 0x40000C8;

  // DMA 2 Destination Address
  const u32 DMA2DAD = 0x40000CC;

  // DMA 2 Word Count
  const u32 DMA2CNT_L = 0x40000D0;

  // DMA 2 Control
  const u32 DMA2CNT_H = 0x40000D2;

  // DMA 3 Source Address
  const u32 DMA3SAD = 0x40000D4;

  // DMA 3 Destination Address
  const u32 DMA3DAD = 0x40000D8;

  // DMA 3 Word Count
  const u32 DMA3CNT_L = 0x40000DC;

  // DMA 3 Control
  const u32 DMA3CNT_H = 0x40000DE;

  // Timer 0 Counter/Reload
  const u32 TM0CNT_L = 0x4000100;

  // Timer 0 Control
  const u32 TM0CNT_H = 0x4000102;

  // Timer 1 Counter/Reload
  const u32 TM1CNT_L = 0x4000104;

  // Timer 1 Control
  const u32 TM1CNT_H = 0x4000106;

  // Timer 2 Counter/Reload
  const u32 TM2CNT_L = 0x4000108;

  // Timer 2 Control
  const u32 TM2CNT_H = 0x400010A;

  // Timer 3 Counter/Reload
  const u32 TM3CNT_L = 0x400010C;

  // Timer 3 Control
  const u32 TM3CNT_H = 0x400010E;

  // SIO Data (Normal-32bit Mode; shared with below)
  const u32 SIODATA32 = 0x4000120;

  // SIO Data 0 (Parent)    (Multi-Player Mode)
  const u32 SIOMULTI0 = 0x4000120;

  // SIO Data 1 (1st Child) (Multi-Player Mode)
  const u32 SIOMULTI1 = 0x4000122;

  // SIO Data 2 (2nd Child) (Multi-Player Mode)
  const u32 SIOMULTI2 = 0x4000124;

  // SIO Data 3 (3rd Child) (Multi-Player Mode)
  const u32 SIOMULTI3 = 0x4000126;

  // SIO Control Register
  const u32 SIOCNT = 0x4000128;

  // SIO Data (Local of MultiPlayer; shared below)
  const u32 SIOMLT_SEND = 0x400012A;

  // SIO Data (Normal-8bit and UART Mode)
  const u32 SIODATA8 = 0x400012A;

  // Key Status
  const u32 KEYINPUT = 0x4000130;

  // Key Interrupt Control
  const u32 KEYCNT = 0x4000132;

  // SIO Mode Select/General Purpose Data
  const u32 RCNT = 0x4000134;

  // Ancient - Infrared Register (Prototypes only)
  const u32 IR = 0x4000136;

  // SIO JOY Bus Control
  const u32 JOYCNT = 0x4000140;

  // SIO JOY Bus Receive Data
  const u32 JOY_RECV = 0x4000150;

  // SIO JOY Bus Transmit Data
  const u32 JOY_TRANS = 0x4000154;

  // Interrupt Enable Register
  const u32 IE = 0x4000200;

  // Interrupt Request Flags / IRQ Acknowledge
  const u32 IF = 0x4000202;

  // Game Pak Waitstate Control
  const u32 WAITCNT = 0x4000204;

  // Interrupt Master Enable Register
  const u32 IME = 0x4000208;

  // Undocumented - Post Boot Flag
  const u32 POSTFLG = 0x4000300;

  // Undocumented - Power Down Control
  const u32 HALTCNT = 0x4000301;

  // Undocumented - Internal Memory Control (R/W)
  const u32 UNK = 0x4000800;
}
