#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

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

// void print_node(FILE *f, node_t *n, uint64_t *i) {
// 	if ((n->op & 0x40) == 0) {
// 		fprintf(f, "\tn%ld [label=\"%s\"];\n", *i, ast_op_debug_str[n->op]);
// 		uint64_t this_i = *i;
// 		if (n->lh != NULL) {
// 			fprintf(f, "\tn%ld -- n%ld;\n", *i, (*i) + 1);
// 			*i += 1;
// 			print_node(f, n->lh, i);
// 		}
// 		if (n->rh != NULL) {
// 			fprintf(f, "\tn%ld -- n%ld;\n", this_i, (*i) + 1);
// 			*i += 1;
// 			print_node(f, n->rh, i);
// 		}
// 	} else {
// 		if (n->op == A_SYMB || n->op == A_IDENT || n->op == A_MODULE)
// 			fprintf(f, "\tn%ld [label=\"%s\\n'%s'\"];\n", *i, ast_op_debug_str[n->op], (char*)n->leaf);
// 		else
// 			fprintf(f, "\tn%ld [label=\"%s\"];\n", *i, ast_op_debug_str[n->op]);
// 	}
// }

// void print_ast(const char *path, node_t *root) {
// 	uint64_t i = 0;
// 	FILE *f = fopen(path, "w");
// 	if (f == NULL)
// 		panic("failed to create new file: %s", path);
// 	fprintf(f, "graph {\n");
// 	print_node(f, root, &i);
// 	fprintf(f, "}\n");
// }

int main(int argc, char *argv[]) {
	for (int i = 1; i < argc; ++i) {
		const char *file = load_file(argv[i]);
		lex_begin(file);

		token_t t = lex_next();
		uint16_t depth = 0;
		bool new_line = false;
		while(t.type != T_EOF) {
			if (t.type == T_LBRC)
				++depth;
			else if (t.type == T_RBRC)
				--depth;

			if (new_line) {
				for (int i = 0; i < depth; ++i)
					putchar('\t');
				new_line = false;
			}
			switch (t.type) {
				case T_IDENT:
					printf("%.*s ", (int)t.string.len, t.string.d);
					break;
				case T_STR:
					// line_len += printf("\"%s: '%.*s' \", ", token_debug_str[t.type], (int)t.string.len, t.string.d);
					printf("\"%.*s\"", (int)t.string.len, t.string.d);
					break;
				case T_CHR:
					// line_len += printf("\"%s\": '%c', ", token_debug_str[t.type], t.character);
					printf("'%c'", t.character);
					break;
				case T_INT:
					// line_len += printf("\"%s\": %ld, ", token_debug_str[t.type], t.sint);
					printf("%ld ", t.sint);
					break;
				case T_SEMI:
				case T_LBRC:
					printf("%s\n", token_debug_str[t.type]);
					new_line = true;
					break;
				default:
					// line_len += printf("\"%s\", ", token_debug_str[t.type]);
					printf("%s ", token_debug_str[t.type]);
					break;
			}
			t = lex_next();
		}		
		printf("\n\n");

		free((void*)file);
	}
	return 0;
}
