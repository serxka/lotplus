module std.string;

enum ParseErr {
	Success,
	NoInput,
	InvalidCharacter,
	OutOfRange;
}

strtin(T: type) comptime: @(input: str): T {
	switch (T) {
		case {i8,i16,i32,i64,isize,u8,u16,u32,u64,usize} -> {}
	}
}
