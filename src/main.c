#include <stdio.h>
#include <stdlib.h>

#include "compiler.h"

char *load_file(const char *path) {
	FILE *f = fopen(path, "r");
	if (f == NULL)
		panic("failed to open file: %s", path);
	fseek(f, 0, SEEK_END);
	size_t len = ftell(f);
	fseek(f, 0, SEEK_SET);
	char *contents = calloc(len, sizeof(char));
	fread(contents, 1, len, f);
	fclose(f);
	contents[len] = 0;
	return contents;
}

static const char *token_debug_str[] = {
	"T_IDENT", "T_INT", "T_FLT", "T_STR", "T_CHR", "T_NUL", "T_TRUE", "T_FALSE",
	"T_COL", "T_SEMI", "T_SET", "T_EQU", "T_ARW", "T_DOT", "T_ASTR", "T_AMPR", "T_RNG", "T_DOLR", "T_COMMA", "T_AT",
	"T_SUB", "T_ADD",
	"T_LBRC", "T_RBRC", "T_LBRA", "T_RBRA", "T_LPAR", "T_RPAR", "T_LANG", "T_RANG",
	"T_RET", "T_MODUL", "T_IMPOR", "T_EXPOR",  "T_STRUC", "T_ENUM", "T_UNION", "T_PRIV", "T_EXTRN", "T_OP",
	"T_FOR", "T_WHILE", "T_IN", "T_SELF",
	"T_EOF"
};

int main(int argc, char *argv[]) {
	for (int i = 1; i < argc; ++i) {
		char *file = load_file(argv[i]);
		lex_setup(file);
		token_t t;
		while ((t = lex_next()) != T_EOF) {
			if (t == T_IDENT || t == T_STR || t == T_CHR || t == T_INT)
				printf("[%s \"%s\"], ", token_debug_str[t], lex_tstr);
			else
				printf("[%s], ", token_debug_str[t]);
		}
		free(file);
	}	
	return 0;
}
