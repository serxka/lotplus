#ifndef _ENUMS_H_
#define _ENUMS_H_

typedef enum {
	// INV asd 1234 "asd" 'a' null true false
	T_INV, T_IDENT, T_INT, T_STR, T_CHR, T_NULL, T_TRUE, T_FALSE,
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
	"(invalid)", "ident", "int", "string", "char", "null", "true", "false",
	":", ";", "=", "==", "->", ".", "&", "..", "$", ",", "@", "@>", "@<",
	"-", "+", "*", "/", "%",
	"{", "}", "[", "]", "(", ")", "<", ">",
	"ret", "module", "import", "export",  "struct", "enum", "union", "priv", "extern", "operator",
	"for", "while", "in", "self",
	"EOF"
};

typedef enum ast_kind_e {
	A_UNIT = 0, A_FN, A_BLCK, A_VAR,
	A_EQU,
	A_IDENT = 128, A_SYMBLS,
}ast_kind_t;

static const char *ast_kind_debug_str[] __attribute__((used)) = {
	[0] = "UNIT", "FN", "BLOCK", "VARIABLE", "=",
	[128] = "IDENTIFIER", "SYMBOLS"
};

#endif // _ENUMS_H_
