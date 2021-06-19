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

// Global variables about the current state of the program
extern const char *current_src;

// The lexer will return one of these token structs at a time
typedef struct token_s {
	// The type of the token, this is a value from the enum above, it represents how the union should be read - if at all
	token_type_t type;
	uint64_t span_s; // This is the start of the span where the token was read
	uint64_t span_e; // this is the matching end
	union {
		str_t string;
		char character;
		int64_t sint;
		uint64_t uint;
	};
}token_t;

// This will set the lexer up with a file to begin work on
void lex_begin(const char *src);
token_t lex_next(void);
uint64_t lex_linenum(uint64_t cursor);

node_t *parse_unit();

#endif // _COMPILER_H_
