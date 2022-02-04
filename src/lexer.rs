use std::str::Chars;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Token {
	And,
	Arrow,
	BitAnd,
	BitAndSet,
	BitNot,
	BitOr,
	BitOrSet,
	BitXor,
	BitXorSet,
	BlockComment { comment: String, doc: bool },
	CharacterLit(char),
	CloseBrace,
	CloseBracket,
	CloseParen,
	Colon,
	Comma,
	Deref,
	Dot,
	Equal,
	Equate,
	Gt,
	GtEqual,
	Identifier(String),
	IntegerLit { value: u128, suffix: IntegerSuffix },
	LineComment { comment: String, doc: bool },
	Lt,
	LtEqual,
	Minus,
	MinusSet,
	Not,
	NotEqual,
	Null,
	OpenBrace,
	OpenBracket,
	OpenParen,
	Or,
	Percent,
	Plus,
	PlusSet,
	Pointer,
	Ref,
	SemiColon,
	ShiftLeft,
	ShiftRight,
	Slash,
	SlashSet,
	Star,
	StarSet,
	StringLit(String),
	Vararg,
	Whitespace,
	Align,
	Alignof,
	Bit,
	Break,
	Case,
	Cast,
	Comptime,
	Const,
	Continue,
	Default,
	Defer,
	Do,
	Else,
	Enum,
	False,
	For,
	If,
	Import,
	Loop,
	Module,
	Noret,
	Offsetof,
	Operator,
	Priv,
	Ret,
	SelfType,
	Sizeof,
	Struct,
	Switch,
	True,
	Typeof,
	Union,
	Use,
	Volatile,
	While,
	Unknown,
}

impl Token {
	pub fn is_whitespace(&self) -> bool {
		match self {
			Token::Whitespace => true,
			Token::LineComment { .. } => true,
			Token::BlockComment { .. } => true,
			_ => false,
		}
	}

	pub fn is_ident(&self) -> bool {
		match self {
			Token::Identifier(_) => true,
			_ => false,
		}
	}

	pub fn is_equivalent(&self, b: &Self) -> bool {
		std::mem::discriminant(self) == std::mem::discriminant(b)
	}

	pub const fn any_character() -> Self {
		Token::CharacterLit(EOF_CHAR)
	}

	pub const fn any_identifier() -> Self {
		Token::Identifier(String::new())
	}

	pub const fn any_integer() -> Self {
		Token::IntegerLit {
			value: 0,
			suffix: IntegerSuffix::None,
		}
	}

	pub const fn any_string() -> Self {
		Token::StringLit(String::new())
	}
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum IntegerSuffix {
	None,
	U8,
	U16,
	U32,
	U64,
	Usize,
	I8,
	I16,
	I32,
	I64,
	Isize,
}

impl IntegerSuffix {
	pub fn from_ident(ident: &str) -> IntegerSuffix {
		match ident {
			"" => IntegerSuffix::None,
			"u8" => IntegerSuffix::U8,
			"u16" => IntegerSuffix::U16,
			"u32" => IntegerSuffix::U32,
			"u64" => IntegerSuffix::U64,
			"usize" => IntegerSuffix::Usize,
			"i8" => IntegerSuffix::I8,
			"i16" => IntegerSuffix::I16,
			"i32" => IntegerSuffix::I32,
			"i64" => IntegerSuffix::I64,
			"isize" => IntegerSuffix::Isize,
			_ => panic!("invalid integer suffix: {:?}", ident),
		}
	}
}

pub struct Lexer<'a> {
	input: Chars<'a>,
	start_len: usize,
	pub total_consumed: usize,
	original: Chars<'a>,
}

pub const EOF_CHAR: char = '\0';

impl<'a> Lexer<'a> {
	pub fn new(src: &str) -> Lexer {
		Lexer {
			start_len: src.len(),
			input: src.chars(),
			total_consumed: 0,
			original: src.chars(),
		}
	}

	pub fn peek_one(&self) -> char {
		self.input.clone().nth(0).unwrap_or(EOF_CHAR)
	}

	pub fn peek_str(&self, amount: usize) -> &str {
		&self.input.clone().as_str()[0..amount]
	}

	pub fn is_eof(&self) -> bool {
		self.input.clone().next().is_none()
	}

	pub fn bump(&mut self) -> Option<char> {
		self.input.next()
	}

	pub fn len_consumed(&self) -> usize {
		self.start_len - self.input.as_str().len()
	}

	pub fn reset_consumed(&mut self) {
		self.start_len = self.input.as_str().len();
	}

	pub fn eat_while(&mut self, mut pred: impl FnMut(char) -> bool) -> String {
		let len_consumed_s = self.len_consumed();
		let start = self.total_consumed + len_consumed_s;
		while pred(self.peek_one()) && !self.is_eof() {
			self.bump();
		}
		self.original.as_str()
			[start..start + self.len_consumed() - len_consumed_s]
			.into()
	}

	pub fn read_token(&mut self) -> Token {
		let first = self.bump().unwrap();
		let token = match first {
			c if is_whitespace(c) => self.whitespace(),
			'#' => match self.peek_one() {
				'[' => self.block_comment(),
				_ => self.line_comment(),
			},
			c @ '0'..='9' => {
				let value = self.eat_number(c);
				let suffix = IntegerSuffix::from_ident(&self.eat_identifier());
				Token::IntegerLit { value, suffix }
			}
			'\'' => self.character(),
			'"' => self.quoted_string(),
			c if is_id_start(c) => self.ident_or_keyword(c),
			'!' => match self.peek_one() {
				'=' => {
					self.bump();
					Token::NotEqual
				}
				_ => Token::Not,
			},
			'%' => Token::Percent,
			'&' => match self.peek_one() {
				'&' => {
					self.bump();
					Token::And
				}
				'=' => {
					self.bump();
					Token::BitAndSet
				}
				_ => Token::BitAnd,
			},
			'(' => Token::OpenParen,
			')' => Token::CloseParen,
			'*' => match self.peek_one() {
				'=' => {
					self.bump();
					Token::StarSet
				}
				_ => Token::Star,
			},
			'+' => match self.peek_one() {
				'=' => {
					self.bump();
					Token::PlusSet
				}
				_ => Token::Plus,
			},
			'-' => match self.peek_one() {
				'>' => {
					self.bump();
					Token::Arrow
				}
				'=' => {
					self.bump();
					Token::MinusSet
				}
				_ => Token::Minus,
			},
			'.' => {
				// If we have two more dots, it a vararg argument
				if self.peek_str(2) == ".." {
					self.bump();
					self.bump();
					Token::Vararg
				} else {
					Token::Dot
				}
			}
			'/' => match self.peek_one() {
				'=' => {
					self.bump();
					Token::SlashSet
				}
				_ => Token::Slash,
			},
			':' => Token::Colon,
			';' => Token::SemiColon,
			'<' => match self.peek_one() {
				'=' => {
					self.bump();
					Token::LtEqual
				}
				'<' => {
					self.bump();
					Token::ShiftLeft
				}
				_ => Token::Lt,
			},
			'=' => match self.peek_one() {
				'=' => {
					self.bump();
					Token::Equate
				}
				_ => Token::Equal,
			},
			'>' => match self.peek_one() {
				'=' => {
					self.bump();
					Token::GtEqual
				}
				'>' => {
					self.bump();
					Token::ShiftRight
				}
				_ => Token::Gt,
			},
			'@' => match self.peek_one() {
				'^' => {
					self.bump();
					Token::Deref
				}
				'<' => {
					self.bump();
					Token::Ref
				}
				_ => Token::Pointer,
			},
			'[' => Token::OpenBracket,
			']' => Token::CloseBracket,
			',' => Token::Comma,
			'^' => match self.peek_one() {
				'=' => {
					self.bump();
					Token::BitXorSet
				}
				_ => Token::BitXor,
			},
			'{' => Token::OpenBrace,
			'|' => match self.peek_one() {
				'|' => {
					self.bump();
					Token::Or
				}
				'=' => {
					self.bump();
					Token::BitOrSet
				}
				_ => Token::BitOr,
			},
			'}' => Token::CloseBrace,
			'~' => Token::BitNot,
			_ => Token::Unknown,
		};

		token
	}

	fn whitespace(&mut self) -> Token {
		self.eat_while(is_whitespace);
		Token::Whitespace
	}

	fn block_comment(&mut self) -> Token {
		self.bump();
		// Check to see if the comment is a doc comment (#! comment)
		let doc = match self.peek_one() {
			'!' => {
				self.bump();
				true
			}
			_ => false,
		};
		let len_consumed_s = self.len_consumed();
		let start = self.total_consumed + len_consumed_s;

		let mut depth = 1;
		while let Some(c) = self.bump() {
			match c {
				'#' if self.peek_one() == '[' => {
					self.bump();
					depth += 1;
				}
				']' if self.peek_one() == '#' => {
					self.bump();
					depth -= 1;
					if depth == 0 {
						break;
					}
				}
				_ => {}
			}
		}

		let comment = self.original.as_str()
			[start..start + self.len_consumed() - len_consumed_s]
			.into();
		Token::BlockComment { comment, doc }
	}

	fn line_comment(&mut self) -> Token {
		// Check to see if the comment is a doc comment (#! comment)
		let doc = match self.peek_one() {
			'!' => {
				self.bump();
				true
			}
			_ => false,
		};
		// Read the whole comment
		let comment = self.eat_while(|c| c != '\n');

		Token::LineComment { comment, doc }
	}

	fn character(&mut self) -> Token {
		let c = match self.bump().unwrap() {
			'\\' => self.escaped_char(),
			c => c,
		};
		assert_eq!(Some('\''), self.bump());

		Token::CharacterLit(c)
	}

	fn escaped_char(&mut self) -> char {
		match self.bump().unwrap_or(EOF_CHAR) {
			'n' => '\n',
			'r' => '\r',
			't' => '\t',
			'"' => '"',
			'\'' => '\'',
			'\\' => '\\',
			c => panic!("unknown escape character {}", c),
		}
	}

	fn quoted_string(&mut self) -> Token {
		let mut string = String::new();
		while let Some(c) = self.bump() {
			match c {
				'"' => break,
				'\\' => string.push(self.escaped_char()),
				c => string.push(c),
			}
		}

		Token::StringLit(string)
	}

	fn eat_number(&mut self, first_digit: char) -> u128 {
		if first_digit == '0' {
			match self.peek_one() {
				'o' => {
					self.bump();
					u128::from_str_radix(&self.eat_oct_digits(), 8).unwrap()
				}
				'x' => {
					self.bump();
					u128::from_str_radix(&self.eat_hex_digits(), 16).unwrap()
				}
				'0'..='9' | '_' => {
					u128::from_str_radix(&self.eat_dec_digits(), 10).unwrap()
				}
				_ => 0,
			}
		} else {
			let mut i = self.eat_dec_digits();
			i.insert(0, first_digit);
			u128::from_str_radix(&i, 10).unwrap()
		}
	}

	fn eat_oct_digits(&mut self) -> String {
		let mut i = self.eat_while(|c| matches!(c, '0'..='7' | '_'));
		i.retain(|c| c != '_');
		i
	}

	fn eat_hex_digits(&mut self) -> String {
		let mut i = self.eat_while(
			|c| matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F' | '_'),
		);
		i.retain(|c| c != '_');
		i
	}

	fn eat_dec_digits(&mut self) -> String {
		let mut i = self.eat_while(|c| matches!(c, '0'..='9' | '_'));
		i.retain(|c| c != '_');
		i
	}

	fn eat_identifier(&mut self) -> String {
		if !is_id_start(self.peek_one()) {
			return String::new();
		}
		self.eat_while(is_id_continue)
	}

	fn ident_or_keyword(&mut self, first_char: char) -> Token {
		let mut ident = self.eat_while(is_id_continue);
		ident.insert(0, first_char);

		use Token::*;
		match ident.as_str() {
			"align" => Align,
			"alignof" => Alignof,
			"bit" => Bit,
			"break" => Break,
			"case" => Case,
			"cast" => Cast,
			"comptime" => Comptime,
			"const" => Const,
			"continue" => Continue,
			"default" => Default,
			"defer" => Defer,
			"do" => Do,
			"else" => Else,
			"enum" => Enum,
			"false" => False,
			"for" => For,
			"if" => If,
			"import" => Import,
			"loop" => Loop,
			"module" => Module,
			"noret" => Noret,
			"null" => Null,
			"offsetof" => Offsetof,
			"operator" => Operator,
			"priv" => Priv,
			"ret" => Ret,
			"self" => SelfType,
			"sizeof" => Sizeof,
			"struct" => Struct,
			"switch" => Switch,
			"true" => True,
			"typeof" => Typeof,
			"union" => Union,
			"use" => Use,
			"volatile" => Volatile,
			"while" => While,
			_ => Identifier(ident),
		}
	}
}

pub fn is_whitespace(c: char) -> bool {
	matches!(
		c,
		'\u{09}'  // horizontal tab
		| '\u{0A}' // line feed
		| '\u{0B}' // vertical tab
		| '\u{0C}' // form fed
		| '\u{0D}' // carriage return
		| '\u{20}' // space
	)
}

pub fn is_id_start(c: char) -> bool {
	matches!(
		c,
		'a'..='z'
		| 'A'..='Z'
		| '_'
	)
}

pub fn is_id_continue(c: char) -> bool {
	matches!(
		c,
		'a'..='z'
		| 'A'..='Z'
		| '0'..='9'
		| '_'
	)
}

pub fn tokenise(src: &str) -> impl Iterator<Item = Token> + '_ {
	let mut lexer = Lexer::new(src);
	std::iter::from_fn(move || {
		if lexer.is_eof() {
			None
		} else {
			lexer.reset_consumed();
			let token = lexer.read_token();
			lexer.total_consumed += lexer.len_consumed();
			Some(token)
		}
	})
}
