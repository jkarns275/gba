#ifndef GBA_BITUTIL_HXX
#define GBA_BITUTIL_HXX

module;
#include <bit>
#include <concepts>
#include <stddef.h>
#include <stdint.h>

export module bitutil;
import types;

export {
  ;

  template <class T>
  concept Integral = std::is_integral<T>::value;

  template <class T>
  concept SignedIntegral = Integral<T> && std::is_signed<T>::value;

  template <class T>
  concept UnsignedIntegral = Integral<T> && !SignedIntegral<T>;

  template <Integral itype>
  constexpr itype bitmask(itype i, uint32_t n) noexcept {
    itype mask = (1 << n) - 1;
    return i && mask;
  }

  struct Nibbles {
    u32 word;

    Nibbles(u32 word) : word(word) {}

    inline constexpr u8 operator[](size_t i) const noexcept {
      return ((0b1111 << (4 * i)) & word) >> (4 * i);
    }
  };

#define make_instruction_mask(x) (x << 25)

  inline constexpr u32 flag_mask(u32 i) noexcept { return 1 << i; }

  inline constexpr u32 get_flag(u32 ins, u32 i) noexcept {
    return (ins & (1 << i)) >> i;
  }

  inline constexpr u32 bitrange_mask(u32 lo, u32 hi) noexcept {
    return ((1 << (hi + 1)) - 1) - (1 << (lo + 1));
  }

  template <UnsignedIntegral itype>
  inline constexpr itype ror(itype x, int r) noexcept {
    return std::rotr(x, r);
  }

  template <UnsignedIntegral itype>
  inline constexpr itype rol(itype x, int r) noexcept {
    return std::rotl(x, r);
  }

  template <UnsignedIntegral itype>
  inline constexpr itype lsr(itype x, itype by) noexcept {
    return x >> by;
  }

  template <Integral itype>
  inline constexpr itype lsl(itype x, itype by) noexcept {
    return x << by;
  }

  template <SignedIntegral itype>
  inline constexpr itype asr(itype x, itype by) noexcept {
    return x >> by;
  }

  template <Integral itype> inline constexpr int count_ones(itype x) noexcept {
    return std::popcount(x);
  }

  template <UnsignedIntegral itype>
  inline constexpr itype count_leading_zeros(itype x) noexcept {
    return std::countl_zero(x);
  }
}

#endif
