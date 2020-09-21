//// Parsers for loading virtual object code.

// In module 2, you add parsers `parseR1LIT` to this file.
// The other parsers may serve as examples you can build on.

#define _GNU_SOURCE

#include <assert.h>
#include <stdio.h>

#include "iformat.h"
#include "iparsers.h"
#include "vmstate.h"
#include "vmstring.h"

#define SEE(R) do { if ((R) > *maxreg) *maxreg = (R); } while(0)

Instruction parseR3(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  uint8_t regZ = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX); SEE(regY); SEE(regZ);
  return eR3(opcode, regX, regY, regZ);
}

Instruction parseR2(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX); SEE(regY);
  return eR2(opcode, regX, regY);
}

Instruction parseR1(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX);
  return eR1(opcode, regX);
}

Instruction parseR0(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  (void)maxreg;
  assert(operands == NULL);
  return eR0(opcode);
}

Instruction parseR1U16(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  (void)maxreg;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint32_t immediate = tokens_get_int(&operands, NULL);
  assert(operands == NULL);
  assert(immediate == (uint16_t) immediate);
  SEE(regX);
  return eR1U16(opcode, regX, immediate);
}

Instruction parseR2U8(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  uint8_t k    = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX); SEE(regY);
  return eR3(opcode, regX, regY, k);
}

Instruction parseR0I24(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  (void)maxreg;
  int32_t immediate = tokens_get_int(&operands, NULL);
  assert(immediate == ((immediate << 8) >> 8));
  assert(operands == NULL);
  return eR0I24(opcode, immediate);
}


static Name truename, falsename, nilname, emptyname, stringname;

static void initnames(void) {
  if (truename == NULL) {
    truename     = strtoname("true");
    falsename    = strtoname("false");
    nilname      = strtoname("nil");
    emptyname    = strtoname("emptylist");
    stringname   = strtoname("string");
  }
}

static Value tokens_get_literal(Tokens *p, const char *original)
{
    switch (first_token_type(*p)) {
        case TNAME: {
            Name name = tokens_get_name(p, original);
            if (name == truename)
                return mkBooleanValue(true);
            if (name == falsename)
                return mkBooleanValue(false);
            if (name == nilname)
                return nilValue;
            if (name == emptyname)
                return emptylistValue;
            if (name == stringname) {
                uint32_t len = tokens_get_int(p, original);
                char *buf = malloc(len);
                assert(buf != NULL);
                for (uint32_t i = 0; i < len; i++)
                    buf[i] = (char)tokens_get_byte(p, original);
                VMString vmstr = Vmstring_new(buf, len);
                free(buf);
                return mkStringValue(vmstr);
            }
            assert(0);
        }
        case TU32: {
            uint32_t n = tokens_get_int(p, original);
            return mkNumberValue((double)n);
        }
        case TDOUBLE: {
            double n = tokens_get_number(p, original);
            return mkNumberValue(n);
        }
        default: {
            assert(!"Parsing error");
        }
    }
}

Instruction parseR1LIT(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  initnames(); // before comparing names, you must call this function
  uint8_t reg = tokens_get_byte(&operands, NULL);
  Value literal = tokens_get_literal(&operands, NULL);
  uint16_t lit_index = literal_slot(vm, literal);
  assert(operands == NULL);
  SEE(reg);
  return eR1U16(opcode, reg, lit_index);
}
