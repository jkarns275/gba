module;
#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
#include <array>
#include <functional>
#include <memory>

export module gba.render;

import bitutil;
import types;

import gba.io;
import arm7tdmi;

using std::make_unique;

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

  enum RenderMode {
    TILE0 = 0,
    TILE1 = 1,
    TILE2 = 2,
    BMP3 = 3,
    BMP4 = 4,
    BMP5 = 5,
  };

  const u32 SCREEN_WIDTH = 240;
  const u32 SCREEN_HEIGHT = 160;

  union GbaBgrColor {
    struct {
      u8 junk : 1;
      u8 b : 5;
      u8 g : 5;
      u8 r : 5;
    };
    u16 color;
  };

  union Color {
    struct __attribute__((packed)) {
      u8 b, g, r, a;
    };
    u32 color;

    Color(u32 color) : color(color) {}
    Color(u8 a, u8 r, u8 g, u8 b) : a(a), r(r), g(g), b(b) {}
  };
  static_assert(sizeof(Color) == 4);

  struct GbaScreen {
    SDL_Texture *texture;
    Color *buffer;
    GbaScreen(SDL_Renderer *renderer)
        : buffer(reinterpret_cast<Color *>(
              new u32[SCREEN_WIDTH * SCREEN_HEIGHT]{})) {
      texture = SDL_CreateTexture(
          renderer,
          SDL_PIXELFORMAT_ARGB8888, // or whatever format your data uses
          SDL_TEXTUREACCESS_STREAMING, SCREEN_WIDTH, SCREEN_HEIGHT);
      update();
    }

    /**
     * Writes the data in the buffer to the texture.
     */
    void update() {
      void *pixels;
      int pitch;

      if (SDL_LockTexture(texture, NULL, &pixels, &pitch) == 0) {
        // Update pixel data here
        memcpy(pixels, static_cast<void *>(buffer), SCREEN_HEIGHT * pitch);
        SDL_UnlockTexture(texture);
      }
    }

    inline void write(u32 x, u32 y, GbaBgrColor color) {
      buffer[y * SCREEN_WIDTH + x] =
          Color(0xFF, color.r * 8, color.g * 8, color.b * 8);
    }

    ~GbaScreen() {
      delete buffer;
      SDL_DestroyTexture(texture);
    }
  };

  struct GbaRenderContext {
    CpuState &state;

    GbaScreen bg0, bg1, bg2;

    GbaRenderContext(SDL_Renderer *renderer, CpuState &state)
        : state(state), bg0(renderer), bg1(renderer), bg2(renderer) {}
  };

  struct GbaRenderer {
    static void render_frame(CpuState &state) {
      const DisplayControl disp_cnt = static_cast<DisplayControl>(
          state.memory.read<u16>(DISPCNT, Mode::SYS));
    }

    DisplayControl disp_cnt;

    GbaRenderer(DisplayControl disp_cnt) : disp_cnt(disp_cnt) {}
    virtual ~GbaRenderer() {}

    virtual void render(CpuState &state) = 0;
  };

  struct NopRenderer : public GbaRenderer {
    using GbaRenderer::GbaRenderer;

    NopRenderer(DisplayControl dc) : GbaRenderer(dc) {}
    ~NopRenderer() {}

    void render(CpuState &state) override {}
  };

  struct Bmp3Renderer : public GbaRenderer {
    using GbaRenderer::GbaRenderer;

    Bmp3Renderer(DisplayControl dc) : GbaRenderer(dc) {}
    ~Bmp3Renderer() {}

    void render(CpuState &state) override {}
  };

  inline const std::function<std::unique_ptr<GbaRenderer>(DisplayControl)>
      renderer_map[] = {
          make_unique<NopRenderer, DisplayControl>,
          make_unique<NopRenderer, DisplayControl>,
          make_unique<NopRenderer, DisplayControl>,
          make_unique<Bmp3Renderer, DisplayControl>,
          make_unique<NopRenderer, DisplayControl>,
          make_unique<NopRenderer, DisplayControl>,
  };
}
