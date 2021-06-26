#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "compiler.h"

static const struct {
	const char* name;
	size_t len;
	token_type_t type;
} keyword_tokens[] = {
	{"ret", 3, T_RET}, {"module", 6, T_MODUL}, {"import", 6, T_IMPOR}, {"export", 6, T_EXPOR}, {"struct", 6, T_STRUC}, {"enum", 4, T_ENUM}, {"union", 5, T_UNION}, 
	{"priv", 4, T_PRIV}, {"extern", 6, T_EXTRN}, {"operator", 8, T_OP},
};

#define KEYWORD_TOKEN_LEN (sizeof(keyword_tokens)/sizeof(keyword_tokens[0]))

const char *current_src;
static uint64_t cursor_last;
static uint64_t cursor;

void lex_begin(const char *src) {
	cursor = 0;
	cursor_last = 0;
	current_src = src;
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

static inline char escaped_char(char final_c) {
	char c = current_src[cursor];
	if (c == '\\') {
		c = current_src[++cursor]; // current char
		++cursor; // set it up for the next
		switch (c) {
			case 't':
				return '\t';
			case 'n':
				return '\n';
			case 'r':
				return '\r';
			case '\\':
				return '\\';
			case '"':
				return '"';
			case '\'':
				return '\'';
			default:
				panic("unknown escaped character (%c) at %ld:%ld", c, lex_column(cursor), lex_linenum(cursor));
		}
	} else if (c == 0) {
		panic("character stream ended abruptly at %ld:%ld", lex_column(cursor), lex_linenum(cursor));
	}
	++cursor;
	return c == final_c ? 0 : c;
}

static str_t string_scan(void) {
	char c;
	str_t string = str_empty();
	while ((c = escaped_char('"')) != 0)
		str_push(&string, c);
	return string;
}

static char char_scan(void) {	
	char c = escaped_char('\'');
	if (c == 0)
		panic("zero length char at %ld:%ld", lex_column(cursor), lex_linenum(cursor));
	if (current_src[cursor++] != '\'')
		panic("oversized char at %ld:%ld", lex_column(cursor), lex_linenum(cursor));
	return c;
}

static void ident_scan(token_t *token) {
	// Iterate the whole identifier
	uint64_t lc = token->span_s;
	while (ident_continue(current_src[++lc]))
		;
	// Create a fat string from it
	str_t ident = str_fat(current_src + token->span_s, lc - token->span_s);
	// Set our file cursor appropriately
	cursor = lc;
	token->type = T_IDENT;
	token->string = ident;

	for (uint64_t i = 0; i < KEYWORD_TOKEN_LEN; ++i) {
		if (keyword_tokens[i].len != ident.len)
			continue;
		if(!strncmp(keyword_tokens[i].name, ident.d, keyword_tokens[i].len)) {
			token->type = keyword_tokens[i].type;
			return;
		}
	}
}

// Rewrite this properly
static void number_scan(token_t *token) {
	uint64_t lc = token->span_s;
	while (ident_continue(current_src[++lc]))
		;
	char atoi_tmp[33];
	size_t len = lc - token->span_s;
	strncpy(atoi_tmp, current_src + token->span_s, len);
	atoi_tmp[len] = 0;
	cursor = lc;

	token->type = T_INT;
	token->sint = atol(atoi_tmp);
}

// Skip ahead white-space in the file
static inline bool skip1_whitespace(void) {
	const char c = current_src[cursor];
	if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
		++cursor;
		return true;
	} else {
		return false;
	}
}

// Run a comment until it is complete
static inline void while_comment(void) {
	char c;
	while ((c = current_src[cursor++]) != '\n')
		;
}

#define SET_TYPE(t) token.type = t; break

token_t lex_next(void) {
	cursor_last = cursor;
	
	// Skip ahead of whitespace while true
	while (skip1_whitespace())
		;
	// See if we have reached the end of file
	if (current_src[cursor] == 0)
		return (token_t){.type = T_EOF, .span_s = cursor, .span_e = cursor};

	token_t token = {0};
	token.span_s = cursor;

	const char c = current_src[cursor++];
	switch (c) {
		case ':':
			SET_TYPE(T_COL);
		case ';':
			SET_TYPE(T_SEMI);
		case '*':
			SET_TYPE(T_MUL);
		case '&':
			SET_TYPE(T_AMPR);
		case '$':
			SET_TYPE(T_DOLR);
		case ',':
			SET_TYPE(T_COMMA);
		case '/':
			SET_TYPE(T_DIV);
		case '{':
			SET_TYPE(T_LBRC);
		case '}':
			SET_TYPE(T_RBRC);
		case '[':
			SET_TYPE(T_LBRA);
		case ']':
			SET_TYPE(T_RBRA);
		case '(':
			SET_TYPE(T_LPAR);
		case ')':
			SET_TYPE(T_RPAR);
		case '<':
			SET_TYPE(T_LANG);
		case '>':
			SET_TYPE(T_RANG);
		case '+':
			if (current_src[cursor] == '+') {
				++cursor;
				SET_TYPE(T_INC);
			} else {
				SET_TYPE(T_ADD);
			}
		case '=':
			if (current_src[cursor] == '=') {
				++cursor;
				SET_TYPE(T_EQU);
			} else {
				SET_TYPE(T_SET);
			}
		case '-':
			if (current_src[cursor] == '>') {
				++cursor;
				SET_TYPE(T_ARW);
			}else if (current_src[cursor] == '-') {
				++cursor;
				SET_TYPE(T_DEC);
			} else {
				SET_TYPE(T_SUB);
			}
		case '.':
			if (current_src[cursor] == '.') {
				++cursor;
				SET_TYPE(T_RNG);
			} else {
				SET_TYPE(T_DOT);
			}
		case '@':
			switch (current_src[cursor]) {
				case '<':
					++cursor;
					SET_TYPE(T_REF);
				case '>':
					++cursor;
					SET_TYPE(T_DEREF);
			}
			SET_TYPE(T_PTR);
		case '#':
			while_comment();
			return lex_next();
		case '"':
			token.string = string_scan();
			SET_TYPE(T_STR);
		case '\'':
			token.character = char_scan();
			SET_TYPE(T_CHR);
		default: // If something else, it is either a keyword/identifer/number or an unknown character
			if (ident_begin(c)) 
				ident_scan(&token);
			else if (isdigit(c))
				number_scan(&token);
			else
				panic("unknown character '%c' (%d) at %ld:%ld", c, (unsigned int)c, lex_column(cursor), lex_linenum(cursor));
	}

	token.span_e = cursor;
	return token;
}

#undef SET_TYPE

void lex_unstep(void) {
	cursor = cursor_last;
}

uint64_t lex_column(uint64_t cursor) {
	uint64_t col = 1;
	for (uint64_t i = 0; i <= cursor; ++i) {
		++col;
		if(current_src[i] == '\n')
			col = 0;
	}
	return col;
}

// Return the line number for a cursor location in the file
uint64_t lex_linenum(uint64_t cursor) {
	uint64_t line = 1;
	for (uint64_t i = 0; i < cursor; ++i)
		if(current_src[i] == '\n')
			++line;
	return line;
}
