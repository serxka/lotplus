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

int main(int argc, char *argv[]) {
	for (int i = 1; i < argc; ++i) {
		char *file = load_file(argv[i]);
		lex_setup(file);
		// token_t t;
		// while ((t = lex_next()) != T_EOF) {
		// 	if (t == T_IDENT || t == T_STR || t == T_CHR || t == T_INT)
		// 		printf("[%s \"%s\"], ", token_debug_str[t], lex_tstr);
		// 	else
		// 		printf("[%s], ", token_debug_str[t]);
		// }
		parse();
		free(file);
	}
	return 0;
}
