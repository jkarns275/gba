module;

#include <concepts>
#include <stdint.h>

export module types;

export {
  ;
  template <class T>
  concept Integral = std::is_integral<T>::value;

  template <class T>
  concept SignedIntegral = Integral<T> && std::is_signed<T>::value;

  template <class T>
  concept UnsignedIntegral = Integral<T> && !SignedIntegral<T>;

  typedef uint64_t u64;
  typedef int64_t i64;
  typedef uint32_t u32;
  typedef int32_t i32;
  typedef uint16_t u16;
  typedef int16_t i16;
  typedef uint8_t u8;
  typedef int8_t i8;
}
