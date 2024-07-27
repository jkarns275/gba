# Gameboy Advanced Emulator
This codebase contains code for a GBA emulator. It is written in modern C++ and uses the C++ module system -- compiler support is quite poor for the module system currently AFAIK. Clang 18.1.7 has worked for me so far, but GCC 14.1.0 has not.

The goal of this project is largely to create a well documented, comprehensible, and tested emulator to serve as an example. There is a strong focus on readability at the expense of performance -- there is no doubt that this will not be the fastest possible GBA emulator, but that really shouldn't be a problem given the fact that the ARM7TDMI is a very old chip.

The following features are in development:
- CPU:
  - ~~ARM instructions (behavior)~~
  - ~~ARM instructions (tests)~~
  - ~~THUMB instructions (behavior)~~
  - Cycle accurate memory accesses
  - Cycle accurate instruction execution
  - Execution pipeline
- Graphics:
  - Create list of graphics features
- Sound
  - Create list of sound features
- Extras:
  - Peripheral support (e.g. Camera)
  - Link support
  - Assembler / Disassembler
  - Debugger
  - Reimplemented BIOS

# Build
Requires a relatively recent version of CMake:
```
mkdir build && cd build
cmake -G Ninja ..
ninja
```

On Mac, xcode clang doesn't work. Install clang with homebrew and use:
```
CXX=/opt/homebrew/opt/llvm/bin/clang++ cmake -G Ninja ..
```

# Testing
Right now the CPU instructions have some limited "unit" tests, where the unit is individual CPU instructions. They can be ran with:
```
build/test/arm7tdmi_tests
```
