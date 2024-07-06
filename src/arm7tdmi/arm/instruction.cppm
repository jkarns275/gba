module;
#include <iostream>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

export module arm7tdmi.arm;

import arm7tdmi.instruction;

export import arm7tdmi.arm.branch;
export import arm7tdmi.arm.clz;
export import arm7tdmi.arm.data_processing;
export import arm7tdmi.arm.mov;
export import arm7tdmi.arm.mul;
export import arm7tdmi.arm.operands;
export import arm7tdmi.arm.swap;
export import arm7tdmi.arm.swi;

using std::string;
using std::unordered_map;
using std::vector;
using std::variant;

export {
  ;

struct UndefinedInstruction : public Ins {
  gword_t instruction;

  UndefinedInstruction(gword_t instruction) : Ins(instruction) {
    // assert(validate_instruction(UndefinedInstruction::DEFINITIONS, instruction));
  }
};

struct ArmInstruction {
  Cond cond;

  typedef variant<
    MulShort,
    MulLong,
    SingleDataSwap,
    LoadStore,
    DataProcessing,
    MovStatusToReg,
    MovToStatus,
    BranchExchange,
    CountLeadingZeros,
    UndefinedInstruction,
    LoadStoreOffset,
    LoadStoreMultiple,
    BranchWithLink,
    SoftwareInterrupt
  > InsAlg; 

  InsAlg instruction; 

  ArmInstruction(gword_t instruction) : instruction(UndefinedInstruction(-1)) {
    Nibbles nibbles(instruction);

    cond = (Cond) nibbles[7];

    if (cond == 0b1111) {
      this->instruction = UndefinedInstruction(instruction);
      return;
    }
    
    gword_t masked_opcode = (instruction >> 24) & 0b11001;
    switch (nibbles[6]) {
      case 0b0000:
      case 0b0001: {
        switch (nibbles[1] & 0b1001) {
          case 0b0000:
          case 0b1000:
            if (masked_opcode == 0b10000) {
              // misc ins 3-3
              if (nibbles[1] == 0b0000) {
                if (nibbles[5] & 0b0010) {
                  this->instruction = MovToStatus(instruction);
                } else {
                  this->instruction = MovStatusToReg(instruction);
                }
              } else {
                // Enhanced DSP Multiplication - undefined in ARMv4
                this->instruction = UndefinedInstruction(instruction);
              }
            } else {
              this->instruction = DataProcessing(instruction);
            }
            break;

          case 0b0001:
            if (masked_opcode == 0b10000) {
             // misc ins 3-3 
              gword_t op = nibbles[5] << 4;
              op |= nibbles[1];

              switch (op) {
                case 0b00100001:
                  this->instruction = BranchExchange(instruction);
                  break;

                case 0b01100001:
                  this->instruction = CountLeadingZeros(instruction);
                  break;

                case 0b00100011:
                  this->instruction = UndefinedInstruction(instruction);
                  break;

                case 0b00000101:
                case 0b00100101:
                case 0b01000101:
                case 0b01100101:
                  // Enhanced DSP additive - undefined in ARMv4
                  this->instruction = UndefinedInstruction(instruction);
                  break;

                case 0b00100111:
                  // SWBreak - undefined in ARMv4
                  this->instruction = UndefinedInstruction(instruction);
                  break;
              }
            } else {
              this->instruction = DataProcessing(instruction);
            }
          case 0b1001:
            // misc ins 2-2
            if (nibbles[1] == 0b1001) {
              // mul / mullong / single swap
              if (nibbles[6] & 0b1) {
                this->instruction = SingleDataSwap(instruction);
              } else {
                if (nibbles[5] & 0b1000) {
                  this->instruction = MulShort(instruction);
                } else {
                  this->instruction = MulLong(instruction);
                }
              }
            } else {
              this->instruction = LoadStore(instruction);
            }
            break;
        }
        break;
      }
      
      case 0b0010:
      case 0b0011: {
        gword_t opcode = (instruction >> 20) & 0b11011;
        switch (opcode) {
          case 0b10000:
            this->instruction = UndefinedInstruction(instruction);
            break;
          case 0b10010:
            this->instruction = MovToStatus(instruction);
            break;
          default:
            this->instruction = DataProcessing(instruction);
        }
        break;
      }

      case 0b0100:
      case 0b0101:
        this->instruction = LoadStoreOffset(instruction);
        break;

      case 0b0110:
      case 0b0111:
        if (nibbles[1] & 1) {
          this->instruction = UndefinedInstruction(instruction);
        } else {
          this->instruction = LoadStoreOffset(instruction);
        }
        break;

      case 0b1000:
      case 0b1001:
        this->instruction = LoadStoreMultiple(instruction);
        break;

      case 0b1010:
      case 0b1011:
        this->instruction = BranchWithLink(instruction);
        break;

      // Coprocessor instructions
      case 0b1100:
      case 0b1101:
      case 0b1110:
        break;

      case 0b1111:
        this->instruction = SoftwareInterrupt(instruction);
        break;
    }
  }

  void execute(CpuState &cpu_state) {
    std::visit([&](Ins &ins) {
      ins.execute(cpu_state);
    }, instruction);
  }
};

void initialize_definition_map() {
  auto &map = InstructionDefinition::DEFINITION_MAP;

  map.insert({"MulShort", {MulShort::definition}});
  map.insert({"MulLong", {MulLong::definition}});
  map.insert({"SingleDataSwap", {SingleDataSwap::definition}});
  map.insert({"LoadStore", LoadStore::definitions});
  map.insert({"DataProcessing", DataProcessing::definitions});
  map.insert({"MovStatusToReg", {MovStatusToReg::definition}});
  map.insert({"MovToStatus", {MovToStatus::definition}});
  map.insert({"BranchExchange", {BranchExchange::definition}});
  map.insert({"CountLeadingZeros", {CountLeadingZeros::definition}});
  map.insert({"LoadStoreOffset", LoadStoreOffset::definitions});
  map.insert({"LoadStoreMultiple", {LoadStoreMultiple::definition}});
  map.insert({"BranchWithLink", {BranchWithLink::definition}});
  map.insert({"SoftwareInterrupt", {SoftwareInterrupt::definition}});
  
}

}
