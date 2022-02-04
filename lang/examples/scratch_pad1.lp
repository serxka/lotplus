module .main;
import std.io;

global_var priv: str = "my global string";

enum Buffering {
	NoBuffering,
	LineBuffering,
	FullBuffering;
}

struct IoStream {
	fd: i32,
	buffering priv: Buffering;
}

struct Streams {
	selected: usize,
	streams: [8; IoStream];
}

struct LinkedList: align(4) {
	value: i32,
	next: @LinkedList;
}

main(void): void {
	for(i: = 0; i < 5; i += 1) {
		io.println("number: %d", i);
	}

	while (true) {
		io.println("y");
	}

	do {
		foo();
	} while(false);

	if (x) {
		y;
	}

	if (x) {
		a;
	} else {
		b;
	}

	if (x) {
		a;
	} else if (y) {
		b;
	} else {
		c;
	}

	defer free(bar);
	
	switch (foo) {
		case posix.EBADF -> ret IoErr.BadDescriptor;
		case 2 -> bar();
		case 420/4 -> if (x) {
			io.println("gaming");
		}
		default -> panic("unknown error");
	}
}
