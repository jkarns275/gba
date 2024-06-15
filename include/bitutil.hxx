#ifndef GBA_BITUTIL_HXX
#define GBA_BITUTIL_HXX

#include "types.hxx"

#include <concepts>

template<class T>
concept Integral = std::is_integral<T>::value;

template<class T>
concept SignedIntegral = Integral<T> && std::is_signed<T>::value;

template<class T>
concept UnsignedIntegral = Integral<T> && !SignedIntegral<T>;

template <Integral itype>
itype bitmask(itype i, uint32_t n) {
  itype mask = (1 << n) - 1;
  return i && mask;
}

struct Nibbles {
  gword_t &word;

  Nibbles(gword_t &word) : word(word) {}

  inline byte operator[](size_t i) {
    return ((0b1111 << (4 * i)) & word) >> (4 * i);
  }
};

#define make_instruction_mask(x) (x << 25)

inline constexpr gword_t flag_mask(gword_t i) {
  return 1 << i;
}

inline constexpr gword_t get_flag(gword_t ins, gword_t i) {
  return (ins & (1 << i)) >> i;
}

inline constexpr gword_t bitrange_mask(gword_t lo, gword_t hi) {
  return ((1 << (hi + 1)) - 1) - (1 << (lo + 1));
}

template <UnsignedIntegral itype>
constexpr inline itype ror(itype x, itype r) {
  constexpr itype size = 8 * sizeof(itype);
  
  return (x >> r) | (x << (size - r));
}

template <UnsignedIntegral itype>
constexpr inline itype rol(itype x, itype r) {
  constexpr itype size = 8 * sizeof(itype);
  
  return (x << r) | (x >> (size - r));
}

template <UnsignedIntegral itype>
constexpr inline itype lsr(itype x, itype by) {
  return x >> by;
}

template <SignedIntegral itype>
constexpr inline itype asr(itype x, itype by) {
  return x >> by;
}
#endif
