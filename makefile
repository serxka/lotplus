PROG = lc

CFLAGS = -O2 -g -Wall -Wextra -pedantic
CPPFLAGS =

C_OBJS = $(patsubst %.c,%.o,$(wildcard src/*.c))

all: ${PROG}

${PROG}: ${C_OBJS}
	${CC} -o $@ $^

%.o: %.c
	${CC} ${CFLAGS} ${CPPFLAGS} -c -MD -o $@ $<

clean:
	rm -f ${C_OBJS} ${C_OBJS:.o=.d}
	rm -f ${PROG}

.PHONY = all clean
