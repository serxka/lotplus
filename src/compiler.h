#ifndef _COMPILER_H_
#define _COMPILER_H_

#include <execinfo.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "enums.h"
#include "containers.h"

#define print_backtrace() do { if (!(getenv("BACKTRACE") != NULL && !strcmp(getenv("BACKTRACE"), "true"))) break; \
                               void *addresses[10] = {0}; \
                               size_t amount = backtrace(addresses, 10); \
                               char **symbols = backtrace_symbols(addresses, amount); \
                               fprintf(stderr, "stack trace:\n"); \
                               for (size_t _pbt_i = 0; _pbt_i < amount; ++_pbt_i) \
                                       fprintf(stderr, "\t%ld) %s\n", _pbt_i, symbols[_pbt_i]); \
                              } while(0)

#define panic(fmt, ...) do { fprintf(stderr, "[panic: %s@%s:%d] " fmt "\n", __func__, __FILE__, __LINE__, ## __VA_ARGS__); \
                             print_backtrace(); \
                             exit(1); } while(0)

#define warning(fmt, ...) do { fprintf(stderr, "[warning: %s@%s:%d] " fmt "\n", __func__, __FILE__, __LINE__, ## __VA_ARGS__); \
                             print_backtrace(); \
                             exit(1); } while(0)
                             
extern char lex_tstr[512];
extern uint64_t lex_ival;
extern double lex_dval;
extern table_t symbols;

void lex_setup(const char*);
token_t lex_next(void);

node_t *parse(void);

#endif // _COMPILER_H_
