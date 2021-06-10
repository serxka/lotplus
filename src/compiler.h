#ifndef _COMPILER_H_
#define _COMPILER_H_

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
	// asd 1234 12.34 "asd" 'a' null true false
	T_IDENT, T_INT, T_FLT, T_STR, T_CHR, T_NULL, T_TRUE, T_FALSE,
	// : ; = == -> . * & .. $ , @
	T_COL, T_SEMI, T_SET, T_EQU, T_ARW, T_DOT, T_ASTR, T_AMPR, T_RNG, T_DOLR, T_COMMA, T_AT,
	// - +
	T_SUB, T_ADD,
	// { }  [ ]  ( )  < >
	T_LBRC, T_RBRC, T_LBRA, T_RBRA, T_LPAR, T_RPAR, T_LANG, T_RANG,
	// ret module import export struct enum union priv extern operator
	T_RET, T_MODUL, T_IMPOR, T_EXPOR,  T_STRUC, T_ENUM, T_UNION, T_PRIV, T_EXTRN, T_OP,
	// for while in self
	T_FOR, T_WHILE, T_IN, T_SELF,
	// end of file
	T_EOF
} token_t;

void lex_setup(const char*);
token_t lex_next(void);

extern char lex_tstr[512];
extern uint64_t lex_ival;
extern double lex_dval;

#define panic(fmt, ...) do { printf("[panic: %s@%s:%d] " fmt "\n", __func__, __FILE__, __LINE__, ## __VA_ARGS__); \
                             exit(1); } while(0)

#define warning(fmt, ...) do { printf("[warning: %s@%s:%d] " fmt "\n", __func__, __FILE__, __LINE__, ## __VA_ARGS__); \
                             exit(1); } while(0)


#endif // _COMPILER_H_
