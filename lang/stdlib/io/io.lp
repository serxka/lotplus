module std.io;

import posix;
import intrinsic.varargs;

enum IoErr {
	NoError,
	BadDescriptor;

	from_errno(errno: i32): self {
		switch (errno) {
			case posix.EBADF ->
				ret IoErr.BadDescriptor;
			default ->
				panic("unknown errno value");
		}
	}
};

stdin const: IoStream = { .fd = 0 };
stdout const: IoStream = { .fd = 1 };
stderr const: IoStream = { .fd = 2 };

struct IoStream {
	fd priv: i32;

	read(@self, buf: @u8, amount: usize): <usize, IoErr> {
		assert(buf != null);
		n: = posix.read(self.fd, buf, amount);
		if (n < 0)
			ret null, IoErr.from_errno(posix.errno);
		ret n, null;
	}

	write(@self, buf: @u8, amount: usize): <usize, IoErr> {
		assert(buf != null);
		n: = posix.write(self.fd, buf, amount);
		if (n < 0)
			ret null, IoErr.from_errno(posix.errno);
		ret n, null;
	}

	readline(@self): str {

	}
}

svprint(io const: @IoStream, format const: str, args: varargs.va_list): <usize, IoErr> {

}

print(format const: str, ...): void {
	args: = varargs.va_start(format);
	svprint(@<stdout, format, args);
	varargs.va_end(args);
}

println(format const: str, ...): void {
	args: = varargs.va_start(format);
	svprint(@<stdout, format, args);
	_:, err: = stdout.write(cast(@u8)@<'\n', 1);
	if (err)
		panic("io error in println");
	varargs.va_end(args);
}
