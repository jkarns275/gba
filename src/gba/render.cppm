export module gba.render;

import bitutil;
import types;

export {
  ;

  struct __attribute__((packed)) TextBackgroundTile {
    u16 tile_number : 10;
    bool horizontal_flip : 1;
    bool vertical_flip : 1;
    u8 palette_number : 4;
  };
  static_assert(sizeof(TextBackgroundTile) == 2);

  struct __attribute__((packed)) PaletteColor {
    u8 red : 5;
    u8 green : 5;
    u8 blue : 5;
    u8 _unused : 1;
  };
  static_assert(sizeof(PaletteColor) == 2);

  struct __attribute__((packed)) Attribute0 {
    u8 y : 8;
    bool trans_enabled : 1;
    bool double_size : 1;
    u8 target : 2;
    bool mosaic_enabled : 1;
    enum { COLOR16 = 0, COLOR256 = 1 } color_mode : 1;
    u8 size_msb : 2;
  };
  static_assert(sizeof(Attribute0) == 2);

  struct __attribute__((packed)) Attribute1Standard {
    u16 x : 9;
    u8 : 3;
    bool horizontal_flip : 1;
    bool vertical_flip : 1;
    u8 size_lsb : 2;
  };
  static_assert(sizeof(Attribute1Standard) == 2);

  struct __attribute__((packed)) Attribute1Transformed {
    u16 x : 9;
    u8 rotation_index : 5;
    u8 size_lsb : 2;
  };
  static_assert(sizeof(Attribute1Transformed) == 2);

  struct __attribute__((packed)) Attribute2 {
    u16 fraction : 10;
    u8 integer : 2;
    u8 palette_number : 4;
  };
  static_assert(sizeof(Attribute2) == 2);

  struct __attribute__((packed)) Attribute3 {
    u8 fraction : 8;
    u8 integer : 7;
    bool sign : 1;
  };
  static_assert(sizeof(Attribute3) == 2);

  struct SpriteSize {
    u8 width;
    u8 height;
  };

  inline constexpr SpriteSize SPRITE_SIZES[12] = {
      {8, 8},   {16, 16}, {32, 32}, {64, 64}, {16, 8},  {32, 8},
      {32, 16}, {64, 32}, {8, 16},  {8, 32},  {16, 32}, {32, 64}};

  struct __attribute__((packed)) Sprite {
    Attribute0 a0;
    union {
      Attribute1Standard standard;
      Attribute1Transformed transformed;
    } a1;
    Attribute2 a2;
    Attribute3 a3;

    SpriteSize sprite_size() const {
      u8 index = (a0.size_msb << 2) | a1.standard.size_lsb;
      if (index < 12)
        return SPRITE_SIZES[index];
      else
        return SPRITE_SIZES[a1.standard.size_lsb];
    }
  };
}
