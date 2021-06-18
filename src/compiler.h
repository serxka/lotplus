#ifndef _COMPILER_H_
#define _COMPILER_H_

#include <execinfo.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

typedef enum {
	// asd 1234 "asd" 'a' null true false
	T_IDENT, T_INT, T_STR, T_CHR, T_NULL, T_TRUE, T_FALSE,
	// : ; = == -> . & .. $ , @ @> @<
	T_COL, T_SEMI, T_SET, T_EQU, T_ARW, T_DOT, T_AMPR, T_RNG, T_DOLR, T_COMMA, T_PTR, T_DEREF, T_REF,
	// - + * / %
	T_SUB, T_ADD,  T_MUL, T_DIV, T_MOD,
	// { }  [ ]  ( )  < >
	T_LBRC, T_RBRC, T_LBRA, T_RBRA, T_LPAR, T_RPAR, T_LANG, T_RANG,
	// ret module import export struct enum union priv extern operator
	T_RET, T_MODUL, T_IMPOR, T_EXPOR, T_STRUC, T_ENUM, T_UNION, T_PRIV, T_EXTRN, T_OP,
	// for while in self
	T_FOR, T_WHILE, T_IN, T_SELF,
	// end of file
	T_EOF,
}token_type_t;

static const char *token_debug_str[] __attribute__((used)) = {
	"ident", "int", "string", "char", "null", "true", "false",
	":", ";", "=", "==", "->", ".", "&", "..", "$", ",", "@", "@>", "@<",
	"-", "+", "*", "/", "%",
	"{", "}", "[", "]", "(", ")", "<", ">",
	"ret", "module", "import", "export",  "struct", "enum", "union", "priv", "extern", "operator",
	"for", "while", "in", "self",
	"EOF"
};

// The lexer will return one of these token structs at a time
typedef struct token_s {
        // The type of the token, this is a value from the enum above, it represents how the union should be read - if at all
        token_type_t type;
        uint32_t span_s; // This is the start of the span where the token was read
        uint32_t span_e; // this is the matching end
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

extern const char *current_src;

#endif // _COMPILER_H_
