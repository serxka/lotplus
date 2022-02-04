import std;
use std.{io, string};

main(void): u8 {
	# Seed and get our random number
	std.rngseed(std.time());
	num: = cast(i32)(std.rand() / 101);
	io.println("Guess a number between 1 and 100!");
	loop {
		io.print("Your number: ");
		guess:, err: = read_number();
		if (err) {
			io.println("Bad number");
			continue;
		}

		if (guess < num) {
			io.println("Guess too small");
		} else if (guess > num) {
			io.println("Guess too large");
		} else {
			io.println("You guessed right! The number was %d", num);
			break;
		}
	}
}

read_number(void): <i32, string.ParseErr> {
	# Read a line from stdin, allocate this line into a buffer
	line:, _: = io.stdin.readline();
	# defer freeing the line
	defer free(line);
	# Call std.parse for a i32 on the input
	ret string.strtin(i32)(line);
}
