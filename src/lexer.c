#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "compiler.h"

char lex_tstr[512];
uint64_t lex_ival;
double lex_dval;

static const char *lex_file;
static size_t cursor;
static size_t line;

void lex_setup(const char *file) {
	cursor = 0;
	lex_dval = 0.0;
	lex_file = file;
	lex_ival = 0;
	line = 1;
	memset(lex_tstr, 0, sizeof(lex_tstr));
}

static inline bool ident_begin(char c) {
	if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_')
		return true;
	else
		return false;
}

static inline bool ident_continue(char c) {
	if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || (c >= '0' && c <= '9'))
		return true;
	else
		return false;
}

static inline char escaped_char(char end_c) {
	char c = lex_file[++cursor];
	if (c == '\\') {
		c = lex_file[++cursor];
		switch (c) {
			case 't':
				return '\t';
			case 'n':
				return '\n';
			case '\\':
				return '\\';
			case '"':
				return '"';
			case '\'':
				return '\'';
			default:
				panic("unknown escaped character (%c) at %ld:%ld", c, cursor, line);
		}
	} else if (c == '\0') {
		panic("character stream ended abruptly at %ld:%ld", cursor, line);
	} else if (c == end_c) {
		++cursor;
		return '\0';
	} else {
		return c;
	}
}

static token_t string_scan(void) {
	char c;
	size_t i = 0;
	while ((c = escaped_char('"')))
		lex_tstr[i++] = c;
	lex_tstr[i] = '\0';
	return T_STR;
}

static token_t char_scan(void) {
	char c = escaped_char('\'');
	if (c == '\0') {
		panic("zero length char at %ld:%ld", cursor, line);
	} else {
		lex_tstr[0] = c;
		lex_tstr[1] = '\0';
		return T_CHR;
	}
}

static token_t ident_scan(void) {
	char c;
	size_t i = 0;
	lex_tstr[i++] = lex_file[cursor];
	while (ident_continue(c = lex_file[++cursor]))
		lex_tstr[i++] = c;
	lex_tstr[i] = '\0';
	
	switch (lex_tstr[0]) {
		case 'e':
			if (!strcmp(lex_tstr, "enum"))
				return T_ENUM;
			else if (!strcmp(lex_tstr, "export"))
				return T_EXPOR;
			else if (!strcmp(lex_tstr, "extern"))
				return T_EXTRN;
			break;
		case 'f':
			if (!strcmp(lex_tstr, "false"))
				return T_FALSE;
			else if (!strcmp(lex_tstr, "for"))
				return T_FOR;
			break;
		case 'i':
			if (!strcmp(lex_tstr, "import"))
				return T_IMPOR;
			else if (!strcmp(lex_tstr, "in"))
				return T_IN;
			break;
		case 'm':if (!strcmp(lex_tstr, "module"))
				return T_MODUL;
			break;
		case 'n':
			if (!strcmp(lex_tstr, "null"))
				return T_NULL;
			break;
		case 'o':
			if (!strcmp(lex_tstr, "operator"))
				return T_OP;
			break;
		case 'p':
			if (!strcmp(lex_tstr, "priv"))
				return T_PRIV;
			break;
		case 'r':
			if (!strcmp(lex_tstr, "ret"))
				return T_RET;
			break;
		case 's':
			if (!strcmp(lex_tstr, "self"))
				return T_SELF;
			else if (!strcmp(lex_tstr, "struct"))
				return T_STRUC;
			break;
		case 't':
			if (!strcmp(lex_tstr, "true"))
				return T_TRUE;
			break;
		case 'u':
			if (!strcmp(lex_tstr, "union"))
				return T_UNION;
			break;
		case 'w':
			if (!strcmp(lex_tstr, "while"))
				return T_WHILE;
			break;
	}
	return T_IDENT;
}

/// PISS
static token_t number_scan(void) {
	char c;
	size_t i = 0;
	lex_tstr[i++] = lex_file[cursor];
	while (ident_continue(c = lex_file[++cursor]))
		lex_tstr[i++] = c;
	lex_tstr[i] = '\0';
	
	if (strchr(lex_tstr, '.') == NULL) { // integer
		lex_ival = atoi(lex_tstr);
		return T_INT;
	} else { // double
		lex_dval = atof(lex_tstr);
		return T_FLT;
	}
}

// Skip ahead white-space
static inline bool whitespace(void) {
	char c = lex_file[cursor];
	if (c == ' ' || c == '\t' || c == '\r') {
		++cursor;
		return true;
	} else if (c == '\n') {
		++cursor;
		++line;
		return true;
	} else {
		return false;
	}
}

static inline void comment(void) {
	char c;
	while ((c = lex_file[cursor++]) != '\n')
		;
	++line;
}

token_t lex_next(void) {
	while (whitespace());
	if (lex_file[cursor] == 0)
		return T_EOF;
	char c = lex_file[cursor];
	
	switch (c) {
		case ':':
			++cursor;
			return T_COL;
		case ';':
			++cursor;
			return T_SEMI;
		case '=':
			if (lex_file[++cursor] == '=') {
				++cursor;
				return T_EQU;
			} else {
				return T_SET;
			}
		case '-':
			if (lex_file[++cursor] == '>') {
				++cursor;
				return T_ARW;
			} else {
				return T_SUB;
			}
		case '.':
			if (lex_file[++cursor] == '.') {
				++cursor;
				return T_RNG;
			} else {
				return T_DOT;
			}
		case '*':
			++cursor;
			return T_ASTR;
		case '&':
			++cursor;
			return T_AMPR;
		case '$':
			++cursor;
			return T_DOLR;
		case ',':
			++cursor;
			return T_COMMA;
		case '@':
			++cursor;
			return T_AT;
		case '+':
			++cursor;
			return T_ADD;
		case '{':
			++cursor;
			return T_LBRC;
		case '}':
			++cursor;
			return T_RBRC;
		case '[':
			++cursor;
			return T_LBRA;
		case ']':
			++cursor;
			return T_RBRA;
		case '(':
			++cursor;
			return T_LPAR;
		case ')':
			++cursor;
			return T_RPAR;
		case '<':
			++cursor;
			return T_LANG;
		case '>':
			++cursor;
			return T_RANG;
		case '"':
			return string_scan();
		case '\'':
			return char_scan();
		case '#':
			comment();
			return lex_next();
		default:
			if (ident_begin(c))
				return ident_scan();
			else if (isdigit(c))
				return number_scan();
			else
				panic("unknown character (%c) %d at %ld:%ld", c, (unsigned int)c, cursor, line);
	}
}
