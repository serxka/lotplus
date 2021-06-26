module .main;

main(void): void {
	number: u32;
	++var;
	var++;
	3 * (5 + 4);
	{
		var[3 * (5 + 4)];
		1 * 2 / 3;
		3 / 1 * 2;
		"hello!"[3];
	}
	asdasd = 3;
	item->child();
	add(4, 5);
	std.process.exit(1);
}

add(a: uint, b: uint): uint {
	ret a + b;
}
