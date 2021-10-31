PROG = lpc

CFLAGS = -O2 -g -Wall -Wextra -pedantic -rdynamic
CPPFLAGS =

C_OBJS = $(patsubst %.c,%.o,$(wildcard src/*.c))

all: ${PROG}

${PROG}: ${C_OBJS}
	${CC} ${CFLAGS} -o $@ $^

%.o: %.c
	${CC} ${CFLAGS} ${CPPFLAGS} -c -MD -o $@ $<

clean:
	rm -f ${C_OBJS} ${C_OBJS:.o=.d}
	rm -f ${PROG}
	rm -f ast.dot

clang-format:
	@for src in $(shell find . -name '*.c' -or -name '*.h') ; do \
		clang-format -style=file -i $$src -verbose ; \
	done

.PHONY = all clean clang-format
