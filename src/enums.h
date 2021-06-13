#ifndef _ENUMS_H_
#define _ENUMS_H_

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

typedef enum ast_op_e {
	// Nodes
	A_UNIT = 0, A_FN, A_PARA, A_LIST,
	// Leafs
	A_IDENT = 64 , A_TYPE, A_SYMB, A_MODULE,
}ast_op_t;

static const char *token_debug_str[] __attribute__((used)) = {
	"T_IDENT", "T_INT", "T_FLT", "T_STR", "T_CHR", "T_NUL", "T_TRUE", "T_FALSE",
	"T_COL", "T_SEMI", "T_SET", "T_EQU", "T_ARW", "T_DOT", "T_ASTR", "T_AMPR", "T_RNG", "T_DOLR", "T_COMMA", "T_AT",
	"T_SUB", "T_ADD",
	"T_LBRC", "T_RBRC", "T_LBRA", "T_RBRA", "T_LPAR", "T_RPAR", "T_LANG", "T_RANG",
	"T_RET", "T_MODUL", "T_IMPOR", "T_EXPOR",  "T_STRUC", "T_ENUM", "T_UNION", "T_PRIV", "T_EXTRN", "T_OP",
	"T_FOR", "T_WHILE", "T_IN", "T_SELF",
	"T_EOF"
};

static const char *ast_op_debug_str[] __attribute__((used)) = {
	[0] = "translation\nunit", "fn", "parameter", "list",
	[64] = "identifier", "type", "symbol", "module",
};

#endif // _ENUMS_H_
