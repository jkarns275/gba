#ifndef GBA_TYPES_HXX
#define GBA_TYPES_HXX
module;

#include <stdint.h>

export module types;

export {
  ;
  typedef uint64_t glong_t;
  typedef int64_t signed_glong_t;
  typedef uint32_t gword_t;
  typedef int32_t signed_gword_t;
  typedef uint16_t gshort_t;
  typedef int16_t signed_gshort_t;
  typedef uint8_t byte;
  typedef int8_t signed_byte;
}

#endif
