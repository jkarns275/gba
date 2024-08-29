module;
#include <algorithm>
#include <iostream>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

export module arm7tdmi.instruction;

export import bitutil;
export import types;

import arm7tdmi;

using std::optional;
using std::string;
using std::unordered_map;
using std::vector;

export {
  ;

  struct Ins {
    Nibbles nibbles;
    Cond cond;

    Ins(u32 instruction)
        : nibbles(instruction), cond((Cond)(instruction >> 28)) {}
    virtual ~Ins() {}

    virtual void execute(CpuState &cpu_state) = 0;
    virtual string disassemble() { return "<UNIMPLEMENTED DISASSEMBLY>"; }
  };

  template <class T>
  concept IsIns = std::is_base_of<Ins, T>::value;

  struct biterator {
    u32 min;
    u32 max;
    u32 i;

    biterator(u32 min, u32 max) : min(min), max(max), i(min) {}
    biterator(u32 max) : min(0), max(max), i(min) {}

    biterator &step() {
      i += 1;
      return *this;
    }

    optional<u32> get() {
      if (i >= min && i < max)
        return i;
      else {
        return std::nullopt;
      }
    }

    void reset() { i = min; }
  };

  struct DecodedPiece {
    const u32 piece, length;
    string name;

    DecodedPiece(u32 piece, u32 length, string name)
        : piece(piece), length(length), name(std::move(name)) {}
  };

  struct InsPiece {
    const u32 nbits;

    InsPiece(u32 nbits) : nbits(nbits) {}

    virtual void build(unordered_map<string, u32> &values,
                       u32 &instruction) const = 0;
    virtual bool advance(vector<DecodedPiece> &pieces, u32 &instruction,
                         u32 &bits_consumed) = 0;
    virtual optional<string> get_name() const { return std::nullopt; }

    virtual biterator iterator() { return biterator(2); }

    virtual u32 get_instruction_size() const { return 32; }

    virtual ~InsPiece() {}
  };

  struct NamedInsPiece : public InsPiece {
    const string name;

    NamedInsPiece(u32 nbits, string &&name) : InsPiece(nbits), name(name) {}

    optional<string> get_name() const override { return name; }
  };

  struct TNamedInsPiece : public NamedInsPiece {
    using NamedInsPiece::NamedInsPiece;

    u32 get_instruction_size() const override { return 16; }
  };

  struct BoolPiece : public NamedInsPiece {

    BoolPiece(string &&name) : NamedInsPiece(1, std::move(name)) {}

    void build(unordered_map<string, u32> &values,
               u32 &instruction) const override {
      bool v = bool(values.at(name));
      instruction <<= 1;
      instruction |= v;
    }

    bool advance(vector<DecodedPiece> &pieces, u32 &instruction,
                 u32 &bits_consumed) override {
      if (bits_consumed + 1 > get_instruction_size())
        return false;

      u32 bit = instruction & 1;
      pieces.emplace_back(bit, 1, name);

      instruction >>= 1;
      bits_consumed += 1;

      return true;
    }

    optional<string> get_name() const override { return name; }

    biterator iterator() override { return biterator(0, 2); }
  };

  struct TBoolPiece : public BoolPiece {
    using BoolPiece::BoolPiece;

    u32 get_instruction_size() const override { return 16; }
  };

  struct IntegralPiece : public NamedInsPiece {
    u32 iterator_min, iterator_max;

    IntegralPiece(int nbits, string &&name)
        : NamedInsPiece(nbits, std::move(name)), iterator_max(1 << nbits) {}
    IntegralPiece(int nbits, string &&name, u32 iterator_max)
        : NamedInsPiece(nbits, std::move(name)), iterator_min(0),
          iterator_max(iterator_max) {}
    IntegralPiece(int nbits, string &&name, u32 iterator_min, u32 iterator_max)
        : NamedInsPiece(nbits, std::move(name)), iterator_min(iterator_min),
          iterator_max(iterator_max) {}

    bool advance(vector<DecodedPiece> &pieces, u32 &instruction,
                 u32 &bits_consumed) override {
      if (bits_consumed + nbits > get_instruction_size())
        return false;

      u32 bits = instruction & ((1 << nbits) - 1);

      pieces.emplace_back(bits, nbits, name);

      instruction >>= nbits;
      bits_consumed += nbits;

      return true;
    }

    void build(unordered_map<string, u32> &values,
               u32 &instruction) const override {
      u32 v = values.at(name);
      instruction <<= nbits;
      instruction |= v & ((1 << nbits) - 1);
    }

    biterator iterator() override {
      return biterator(iterator_min, iterator_max);
    }
  };

  struct TIntegralPiece : public IntegralPiece {
    using IntegralPiece::IntegralPiece;

    u32 get_instruction_size() const override { return 16; }
  };

  struct RegPiece : public IntegralPiece {
    RegPiece(string &&name) : IntegralPiece(4, std::move(name)) {}
  };

  struct TRegPiece : public IntegralPiece {
    TRegPiece(string &&name) : IntegralPiece(3, std::move(name)) {}

    u32 get_instruction_size() const override { return 16; }
  };

  struct ValuePiece : public InsPiece {
    u32 value;

    ValuePiece(u32 value, int nbits) : InsPiece(nbits), value(value) {}

    bool advance(vector<DecodedPiece> &pieces, u32 &instruction,
                 u32 &bits_consumed) override {
      if (bits_consumed + nbits > get_instruction_size())
        return false;

      u32 value = this->value;

      for (u32 i = 0; i < nbits; i++) {
        u32 bit = instruction & 1;
        u32 target = value & 1;

        if (bit != target)
          return false;

        instruction >>= 1;
        value >>= 1;
      }

      bits_consumed += nbits;

      pieces.emplace_back(value, nbits, "<value>");

      return true;
    }

    void build(unordered_map<string, u32> &values,
               u32 &instruction) const override {
      instruction <<= nbits;
      instruction |= value;
    }

    biterator iterator() override { return biterator(value, value + 1); }
  };

  struct TValuePiece : public ValuePiece {
    using ValuePiece::ValuePiece;

    u32 get_instruction_size() const override { return 16; }
  };

  struct Zeros : public ValuePiece {
    Zeros(int nbits) : ValuePiece(0, nbits) {}

    biterator iterator() override { return biterator(0, 1); }
  };

  struct TZeros : public Zeros {
    using Zeros::Zeros;

    u32 get_instruction_size() const override { return 16; }
  };

  struct Ones : public ValuePiece {
    Ones(int nbits) : ValuePiece((1 << nbits) - 1, nbits) {}

    biterator iterator() override {
      u32 mask = (1 << nbits) - 1;
      return biterator(mask, mask + 1);
    }
  };

  struct TOnes : public Ones {
    using Ones::Ones;

    u32 get_instruction_size() const override { return 16; }
  };

  struct CondPiece : public InsPiece {
    CondPiece() : InsPiece(4) {}

    bool advance(vector<DecodedPiece> &pieces, u32 &instruction,
                 u32 &bits_consumed) override {
      if (bits_consumed + 4 > get_instruction_size())
        return false;

      u32 bits = instruction & 0xF;
      if (bits == 0xF)
        return false;

      instruction >>= 4;
      bits_consumed += 4;

      pieces.emplace_back(bits, nbits, "cond");

      return true;
    }

    void build(unordered_map<string, u32> &values,
               u32 &instruction) const override {
      auto f = values.find("cond");
      u32 v = f == values.end() ? 0 : f->second;
      instruction <<= nbits;
      instruction |= v & 0xF;
    }

    optional<string> get_name() const override { return "cond"; }

    biterator iterator() override {
      // return biterator(0, 1 << nbits);
      return biterator(0, 1);
    }
  };

  struct InstructionDefinition {
    static inline unordered_map<string, vector<const InstructionDefinition *>>
        DEFINITION_MAP;
    static inline unordered_map<string, vector<const InstructionDefinition *>>
        THUMB_DEFINITION_MAP;

    vector<InsPiece *> pieces;

    InstructionDefinition(vector<InsPiece *> pieces)
        : pieces(std::move(pieces)) {
      for (size_t i = 0; i < this->pieces.size(); i++) {
      }
    }

    InstructionDefinition(const InstructionDefinition &other)
        : pieces(std::move(other.pieces)) {}

    ~InstructionDefinition() {
      for (size_t i = 0; i < pieces.size(); i++)
        delete pieces[i];
    }

    virtual u32 get_instruction_size() const { return 32; }

    void print_definition() const {
      vector<int> sizes;
      vector<string> names;

      std::cout << "+";
      for (int i = 0; i < pieces.size(); i++) {
        optional<string> name = pieces[i]->get_name();
        if (name) {
          names.push_back(*name);
        } else {
          names.push_back("");
        }

        sizes.push_back(
            std::max<u32>(names[i].size() + 3, pieces[i]->nbits * 3));
        for (int j = 0; j < sizes[i] + 3 - (i == pieces.size() - 1); j++) {
          std::cout << '-';
        }
      }
      std::cout << "+\n";

      for (int i = 0; i < pieces.size(); i++) {
        if (names[i].size() == 0)
          std::cout << std::format(
              "| {:^{}} ", std::format("{}/{}", names[i], pieces[i]->nbits),
              sizes[i]);
        else
          std::cout << std::format("| {:^{}} ", "", sizes[i]);
      }
      std::cout << "|\n";

      std::cout << '|';
      for (int i = 0; i < pieces.size(); i++) {
        if (names[i].size()) {
          std::cout << std::format(
              " {:^{}} ", std::format("{}/{}", names[i], pieces[i]->nbits),
              sizes[i]);
        } else {
          for (int j = 0; j < sizes[i] + 2; j++)
            std::cout << '-';
        }
        std::cout << '|';
      }

      std::cout << '\n';

      for (int i = 0; i < pieces.size(); i++) {
        string bits;
        if (!names[i].size()) {
          u32 n = *pieces[i]->iterator().get();
          for (int j = pieces[i]->nbits - 1; j >= 0; j--) {
            if (n & (1 << j)) {
              bits += " 1 ";
            } else {
              bits += " 0 ";
            }
          }
        }
        std::cout << std::format("| {:^{}} ", bits, sizes[i]);
      }
      std::cout << "|\n";

      std::cout << "+";
      for (int i = 0; i < pieces.size(); i++) {
        for (int j = 0; j < sizes[i] + 3 - (i == pieces.size() - 1); j++) {
          std::cout << '-';
        }
      }
      std::cout << "+\n";
    }

    vector<DecodedPiece> validate(u32 instruction) const {
      u32 icopy = instruction;
      u32 bits_consumed = 0;

      vector<DecodedPiece> decoded_pieces;

      for (int i = pieces.size() - 1; i >= 0; i--) {
        if (!pieces[i]->advance(decoded_pieces, icopy, bits_consumed))
          return decoded_pieces;
      }

      // Invalid definition probably
      if (bits_consumed != get_instruction_size())
        return decoded_pieces;

      return decoded_pieces;
    }

    u32 build(unordered_map<string, u32> &values) const {
      u32 instruction = 0;

      for (size_t i = 0; i < pieces.size(); i++) {
        pieces[i]->build(values, instruction);
        auto name = pieces[i]->get_name();
      }

      return instruction;
    }

    unordered_map<string, u32> generate_value_map() const {
      unordered_map<string, u32> map;

      for (size_t i = 0; i < pieces.size(); i++) {
        optional<string> name = pieces[i]->get_name();
        if (name) {
          map.insert({std::move(*name), 0});
        }
      }

      return map;
    }

    struct iterator {
      vector<biterator> iters;
      const vector<InsPiece *> &pieces;

      iterator(const InstructionDefinition &def) : pieces(def.pieces) {
        for (size_t i = 0; i < pieces.size(); i++) {
          iters.push_back(pieces[i]->iterator());
        }
      }

      optional<u32> get() {
        u32 out = 0;

        for (size_t i = 0; i < iters.size(); i++) {
          optional<u32> bits = iters[i].get();
          if (!bits)
            return std::nullopt;

          out |= *bits;
          if (i < iters.size() - 1)
            out <<= pieces[i + 1]->nbits;
        }

        return out;
      }

      iterator &step() {
        for (size_t i = pieces.size() - 1; i >= 0; i--) {
          iters[i].step();
          optional<u32> bits = iters[i].get();

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

    iterator begin() const { return iterator(*this); }
  };

  struct TInstructionDefinition : public InstructionDefinition {
    using InstructionDefinition::InstructionDefinition;

    u32 get_instruction_size() const override { return 16; }
  };

  bool validate_instruction(const vector<InstructionDefinition> &definitions,
                            u32 instruction) {
    for (auto it = definitions.begin(); it != definitions.end(); it++) {
      auto p = it->validate(instruction);
      if (p.size() == it->pieces.size())
        return true;
    }

    return false;
  }
}
