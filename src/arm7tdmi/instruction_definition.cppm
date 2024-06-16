#ifndef GBA_ARM7TDMI_INSTRUCTION_DEFINITION
#define GBA_ARM7TDMI_INSTRUCTION_DEFINITION

module;
#include <optional>
using std::optional;

#include <string>
using std::string;

#include <unordered_map>
using std::unordered_map;

#include <vector>
using std::vector;

export module arm7tdmi.instruction_definition;

import types;

export {

struct biterator {
  gword_t min;
  gword_t max;
  gword_t i;

  biterator(gword_t min, gword_t max) : min(min), max(max), i(min) {}
  biterator(gword_t max) : min(0), max(max), i(min) {}

  biterator &step() {
    i += 1;
    return *this;
  }

  optional<gword_t> get() {
    if (i >= min && i < max)
      return i;
    else {
      return std::nullopt;
    }
  }

  void reset() {
    i = min;
  }
};

struct DecodedPiece {
  const gword_t piece, length;
  string name;

  DecodedPiece(gword_t piece, gword_t length, string name) : piece(piece), length(length), name(std::move(name)) {}
};

struct InsPiece {
  const gword_t nbits;
  gword_t ins_size = 32;

  InsPiece(gword_t nbits) : nbits(nbits) {}

  virtual void build(unordered_map<string, gword_t> &values, gword_t &instruction) const = 0;
  virtual bool advance(vector<DecodedPiece> &pieces, gword_t &instruction, gword_t &bits_consumed) = 0;

  virtual biterator iterator() {
    return biterator(2);
  }

  virtual ~InsPiece() {}
};

struct BoolPiece : public InsPiece {
  string name; 

  BoolPiece(string &&name) : InsPiece(1), name(name) {}
  
  void build(unordered_map<string, gword_t> &values, gword_t &instruction) const override {
    bool v = bool(values.at(name));
    instruction <<= 1;
    instruction |= v;
  }

  bool advance(vector<DecodedPiece> &pieces, gword_t &instruction, gword_t &bits_consumed) override {
    if (bits_consumed + 1 > ins_size)
      return false;

    gword_t bit = instruction & 1;
    pieces.emplace_back(bit, 1, name);

    instruction >>= 1;
    bits_consumed += 1;

    return true;
  }
  
  biterator iterator() override {
    return biterator(0, 2);
  }

};

struct IntegralPiece : public InsPiece {
  string name;
  gword_t iterator_min, iterator_max;
  
  IntegralPiece(int nbits, string &&name) : InsPiece(nbits), name(name), iterator_max(1 << nbits) {}
  IntegralPiece(int nbits, string &&name, gword_t iterator_max) : InsPiece(nbits), name(name), iterator_min(0), iterator_max(iterator_max) {}
  IntegralPiece(int nbits, string &&name, gword_t iterator_min, gword_t iterator_max) : InsPiece(nbits), name(name), iterator_min(iterator_min), iterator_max(iterator_max) {}

  bool advance(vector<DecodedPiece> &pieces, gword_t &instruction, gword_t &bits_consumed) override {
    if (bits_consumed + nbits > ins_size)
      return false;

    gword_t bits = instruction & ((1 << nbits) - 1);

    pieces.emplace_back(bits, nbits, name);

    instruction >>= nbits;
    bits_consumed += nbits;

    return true;
  }
  
  void build(unordered_map<string, gword_t> &values, gword_t &instruction) const override {
    gword_t v = values.at(name);
    instruction <<= nbits;
    instruction |= v;
  }
  
  biterator iterator() override {
    return biterator(iterator_min, iterator_max);
  }
};

struct RegPiece : public IntegralPiece {
  RegPiece(string &&_name) : IntegralPiece(4, std::move(_name)) {}
};

struct ValuePiece : public InsPiece {
  gword_t value;

  ValuePiece(gword_t value, int nbits) : InsPiece(nbits), value(value) {}
  
  bool advance(vector<DecodedPiece> &pieces, gword_t &instruction, gword_t &bits_consumed) override {
    if (bits_consumed + nbits > ins_size)
      return false;

    gword_t value = this->value;
    
    for (int i = 0; i < nbits; i++) {
      gword_t bit = instruction & 1;
      gword_t target = value & 1;

      if (bit != target)
        return false;

      instruction >>= 1;
      value >>= 1;
    }

    bits_consumed += nbits;
    
    pieces.emplace_back(value, nbits, "<value>");

    return true;
  }
  
  void build(unordered_map<string, gword_t> &values, gword_t &instruction) const override {
    instruction <<= nbits;
    instruction |= value;

  } 
  
  biterator iterator() override {
    return biterator(value, value + 1);
  }
};

struct Zeros : public ValuePiece {
  Zeros(int nbits) : ValuePiece(0, nbits) {}
  
  biterator iterator() override {
    return biterator(0, 1);
  }
};

struct Ones : public ValuePiece {
  Ones(int nbits) : ValuePiece((1 << nbits) - 1, nbits) {}
  
  biterator iterator() override {
    gword_t mask = (1 << nbits) - 1;
    return biterator(mask, mask + 1);
  }
  
};

struct CondPiece : public InsPiece {
  CondPiece() : InsPiece(4) {}

  bool advance(vector<DecodedPiece> &pieces, gword_t &instruction, gword_t &bits_consumed) override {
    if (bits_consumed + 4 > ins_size)
      return false;

    gword_t bits = instruction & 0xF;
    if (bits == 0xF)
      return false;

    instruction >>= 4;
    bits_consumed += 4;

    pieces.emplace_back(bits, nbits, "cond");
    
    return true;
  }

  void build(unordered_map<string, gword_t> &values, gword_t &instruction) const override {
    auto f = values.find("cond");
    gword_t v = f == values.end() ? 0 : f->second;
    instruction <<= nbits;
    instruction |= v;
  } 
  
  biterator iterator() override {
    // return biterator(0, 1 << nbits);
    return biterator(0, 1);
  }
};

struct InstructionDefinition {
  static inline unordered_map<string, vector<const InstructionDefinition *>> DEFINITION_MAP;

  static void initialize_definition_map();

  vector<InsPiece *> pieces;
  int ins_size = 32;

  InstructionDefinition(vector<InsPiece *> pieces) : pieces(std::move(pieces)) {
    for (int i = 0; i < this->pieces.size(); i++){}
  }

  InstructionDefinition(const InstructionDefinition &other) : pieces(std::move(other.pieces)) {}

  ~InstructionDefinition() {
    for (int i = 0; i < pieces.size(); i++)
      delete pieces[i];
  }

  vector<DecodedPiece> validate(gword_t instruction) const {
    gword_t icopy = instruction;
    gword_t bits_consumed = 0;

    vector<DecodedPiece> decoded_pieces;

    for (int i = pieces.size() - 1; i >= 0; i--)
      if (!pieces[i]->advance(decoded_pieces, icopy, bits_consumed))
        return decoded_pieces;
    
    // Invalid definition probably
    if (bits_consumed != ins_size)
      return decoded_pieces;

    return decoded_pieces;
  }

  gword_t build(unordered_map<string, gword_t> &values) const {
    gword_t instruction = 0;
    
    for (int i = 0; i < pieces.size(); i++) {
      pieces[i]->build(values, instruction);
    }

    return instruction;
  }

  struct iterator {
    vector<biterator> iters;
    const vector<InsPiece *> &pieces;

    iterator(const InstructionDefinition &def) : pieces(def.pieces) {
      for (int i = 0; i < pieces.size(); i++) {
        iters.push_back(pieces[i]->iterator());
      }
    }

    optional<gword_t> get() {
      gword_t out = 0;

      for (int i = 0; i < iters.size(); i++) {
        optional<gword_t> bits = iters[i].get();
        if (!bits)
          return std::nullopt;
        
        out |= *bits;
        if (i < iters.size() - 1)
          out <<= pieces[i + 1]->nbits;
      }

      return out;
    }

    iterator &step() {
      for (int i = pieces.size() - 1; i >= 0; i--) {
        iters[i].step();
        optional<gword_t> bits = iters[i].get();

        if (bits) {
          break;
        } else if (i) {
          iters[i].reset();
        } else {
          // Final iterator returned none which means it has reached the end
          return *this;
        }
      }

      return *this;
    }
  };

  iterator begin() const {
    return iterator(*this);
  }

};

bool validate_instruction(const vector<InstructionDefinition> &definitions, gword_t instruction) {
  for (auto it = definitions.begin(); it != definitions.end(); it++) {
    auto p = it->validate(instruction);
    if (p.size() == it->pieces.size())
      return true;
  }

  return false;
}

void InstructionDefinition::initialize_definition_map() {
  InstructionDefinition::DEFINITION_MAP = {
    {"MulShort", vector({MulShort::definition})},
    {"MulLong", vector({MulLong::definition})},
    {"SingleDataSwap", vector({SingleDataSwap::definition})},
    {"Load", vector(Load::definitions)},
    {"DataProcessing", vector(DataProcessing::definitions)},
    {"MovStatusToReg", vector({MovStatusToReg::definition})},
    {"MovRegToStatus", vector({MovRegToStatus::definition})},
    {"BranchExchange", vector({BranchExchange::definition})},
    {"CondLeadingZeros", vector({CountLeadingZeros::definition})},
    {"EnhancedDSPAdditive", vector({EnhancedDSPAdditive::definition})},
    {"SWBreak", vector({SWBreak::definition})},
    {"EnhancedDSPMultiplicative", vector({EnhancedDSPMultiplicative::definition})},
    {"MovImmToStatusReg", vector({MovImmToStatusReg::definition})},
    {"LoadStoreOffset", vector(LoadStoreOffset::definitions)},
    {"LoadStoreMultiple", vector({LoadStoreMultiple::definition})},
    {"BranchWithLink", vector({BranchWithLink::definition})},
    {"SoftwareInterrupt", vector({SoftwareInterrupt::definition})}
  };
}

}

#endif
