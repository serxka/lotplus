mod lexer;
mod parser;
mod sema;

fn main() {
	let mut files = std::env::args()
		.skip(1)
		.filter_map(|f| std::fs::read_to_string(f).ok());
	while let Some(src) = files.next() {
		let tokens = lexer::tokenise(&src)
			.filter(|c| !c.is_whitespace())
			.collect::<Vec<_>>();
		let tree = parser::parse(tokens);
		println!("{:#?}", tree);
	}
}
