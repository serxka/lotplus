module std;
import intrinsic;

enum Exit {
	SUCCESS,
	FAILURE;
}

exit(code: Exit) noret: void {
	libc._exit(bit(u8)code);
}

r_state      priv: u64       = 0x4d595df4d0f33173;
r_multiplier priv const: u64 = 0x5851f42d4c957f2d;
r_increment  priv: u64       = 0x14057b7ef767814f;

rand(void): u32 {
	x: = r_state;
	count: u32 = cast(u32)(x >> 59);

	r_state = x * r_multiplier + r_increment;
	x ^= x >> 18;
	ret intrinsic.rotr32(cast(u32)(x >> 27), count);
}

rngseed(seed: u64): void {
	r_state = seed + r_increment;
	_: = rand();
}
