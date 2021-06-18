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

void print_node(FILE *f, node_t *n, uint64_t *i) {
	if ((n->op & 0x40) == 0) {
		fprintf(f, "\tn%ld [label=\"%s\"];\n", *i, ast_op_debug_str[n->op]);
		uint64_t this_i = *i;
		if (n->lh != NULL) {
			fprintf(f, "\tn%ld -- n%ld;\n", *i, (*i) + 1);
			*i += 1;
			print_node(f, n->lh, i);
		}
		if (n->rh != NULL) {
			fprintf(f, "\tn%ld -- n%ld;\n", this_i, (*i) + 1);
			*i += 1;
			print_node(f, n->rh, i);
		}
	} else {
		if (n->op == A_SYMB || n->op == A_IDENT || n->op == A_MODULE)
			fprintf(f, "\tn%ld [label=\"%s\\n'%s'\"];\n", *i, ast_op_debug_str[n->op], (char*)n->leaf);
		else
			fprintf(f, "\tn%ld [label=\"%s\"];\n", *i, ast_op_debug_str[n->op]);
	}
}

void print_ast(const char *path, node_t *root) {
	uint64_t i = 0;
	FILE *f = fopen(path, "w");
	if (f == NULL)
		panic("failed to create new file: %s", path);
	fprintf(f, "graph {\n");
	print_node(f, root, &i);
	fprintf(f, "}\n");
}

int main(int argc, char *argv[]) {
	for (int i = 1; i < argc; ++i) {
		char *file = load_file(argv[i]);
		lex_setup(file);
		// token_t t;
		// while ((t = lex_next()) != T_EOF) {
		// 	if (t == T_IDENT || t == T_STR || t == T_CHR)
		// 		printf("[%s \"%s\"], ", token_debug_str[t], lex_tstr);
		// 	else if(t == T_INT)
		// 		printf("[%s \"%ld\"], ", token_debug_str[t], lex_ival);
		// 	else
		// 		printf("[%s], ", token_debug_str[t]);
		// }
		node_t *ast = parse();
		print_ast("ast.dot", ast);
		// table_iter_reset(&symbols);
		// for (size_t i = 0; table_next(); ++i) {
		// 	printf("symbol at %ld\n", i);
		// }
		free(file);
	}
	return 0;
}
