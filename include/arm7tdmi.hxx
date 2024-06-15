#ifndef GBA_ARM7TDMI_HXX
#define GBA_ARM7TDMI_HXX

#include <assert.h>

#include <format>
#include <iostream>

#include <memory>
using std::unique_ptr;

#include <optional>
using std::optional;

#include <string>
using std::string;

#include <unordered_map>
using std::unordered_map;

#include <utility>
using std::pair;

#include <variant>
using std::variant;

#include <vector>
using std::vector;

#include "bitutil.hxx"
#include "types.hxx"

const gword_t DATA_PROC_INS = make_instruction_mask(0b001);
const gword_t INS = make_instruction_mask(0b001);




#endif
