use crate::lexer::{IntegerSuffix, Token};
use crate::sema::{BinOp, PointerType, Qualifier, UnaryOp};

#[derive(Debug)]
pub enum AstNode {
	/// The top level node of the syntax tree, contains a list of definitions
	Unit(Vec<AstNode>),
	/// Should contain a path without the trailing group
	ModuleDecl(Box<AstNode>),
	/// Should contain a path, can also contain a group at the end
	ImportDecl(Box<AstNode>),
	UseDecl(Box<AstNode>),
	Path {
		/// Whether the path is absolute or not (starts with a `.`)
		absolute: bool,
		/// The base of the path: `.parser.char.spaces`
		base: Vec<AstNode>,
		/// If the path ends with a `.{...}` the values within will be
		/// contained here, otherwise it is empty.
		group: Vec<AstNode>,
	},
	Identifier(String),
	Integer(u128, IntegerSuffix),
	String(String),
	Character(char),
	Boolean(bool),
	Null,
	SelfValue,
	StructLiteral(Vec<(Option<AstNode>, AstNode)>),
	Type {
		pointer: PointerType,
		base_ty: Box<AstNode>,
		array_size: Option<Box<AstNode>>,
	},
	Qualifiers(Vec<Qualifier>),
	Function {
		identifier: Box<AstNode>,
		parameters: Vec<(AstNode, AstNode, AstNode)>,
		qualifiers: Box<AstNode>,
		return_types: Vec<AstNode>,
		body: Box<AstNode>,
	},
	MultipleRetVariable {
		vars: Vec<AstNode>,
		initial: Box<AstNode>,
	},
	Variable {
		identifier: Box<AstNode>,
		qualifiers: Box<AstNode>,
		ty: Option<Box<AstNode>>,
		initial: Option<Box<AstNode>>,
	},
	Arguments(Vec<AstNode>),
	Block(Vec<AstNode>),
	BinOp(BinOp, Box<AstNode>, Box<AstNode>),
	UnaryOp(UnaryOp, Box<AstNode>),
	Return(Vec<AstNode>),
	Break,
	Continue,
	If {
		precond: Option<Box<AstNode>>,
		cond: Box<AstNode>,
		body: Box<AstNode>,
		else_body: Option<Box<AstNode>>,
	},
	For {
		cond: Box<(Option<AstNode>, AstNode, Option<AstNode>)>,
		body: Box<AstNode>,
	},
	DoWhile {
		body: Box<AstNode>,
		cond: Box<AstNode>,
	},
	While {
		cond: Box<AstNode>,
		body: Box<AstNode>,
	},
	Loop(Box<AstNode>),
	Defer(Box<AstNode>),
	Switch {
		value: Box<AstNode>,
		cases: Vec<AstNode>,
	},
	Case {
		value: Box<AstNode>,
		body: Box<AstNode>,
	},
	Default(Box<AstNode>),
	VarArgs,
	SelfType,
	Struct {
		identifier: Box<AstNode>,
		qualifiers: Box<AstNode>,
		members: Vec<(AstNode, AstNode, AstNode)>,
		body: Option<Box<AstNode>>,
	},
	Union {
		identifier: Box<AstNode>,
		qualifiers: Box<AstNode>,
		members: Vec<(AstNode, AstNode, AstNode)>,
		body: Option<Box<AstNode>>,
	},
	Enum {
		identifier: Box<AstNode>,
		qualifiers: Box<AstNode>,
		elements: Vec<(AstNode, Option<AstNode>)>,
		body: Option<Box<AstNode>>,
	},
	Nil,
}

impl AstNode {
	pub fn ident(token: Token) -> AstNode {
		match token {
			Token::Identifier(i) => AstNode::Identifier(i),
			_ => {
				panic!("expected a token kind of identifier, found {:?}", token)
			}
		}
	}

	pub fn unaryop(op: Token, lhs: AstNode) -> AstNode {
		let op = match op {
			Token::Plus => UnaryOp::Positive,
			Token::Minus => UnaryOp::Negative,
			Token::Not => UnaryOp::Not,
			Token::BitNot => UnaryOp::Negate,
			Token::Sizeof => UnaryOp::Sizeof,
			Token::Alignof => UnaryOp::Alignof,
			Token::Typeof => UnaryOp::Typeof,
			Token::Offsetof => UnaryOp::Offsetof,
			Token::Deref => UnaryOp::Dereference,
			Token::Ref => UnaryOp::Reference,
			_ => panic!("invalid unary operator token: {:?}", op),
		};
		AstNode::UnaryOp(op, Box::new(lhs))
	}

	pub fn binop(op: Token, lhs: AstNode, rhs: AstNode) -> AstNode {
		let op = match op {
			Token::Plus => BinOp::Add,
			Token::Minus => BinOp::Subtract,
			Token::Star => BinOp::Multiply,
			Token::Slash => BinOp::Divide,
			Token::Percent => BinOp::Remainder,
			Token::BitXor => BinOp::BitwiseXor,
			Token::BitAnd => BinOp::BitwiseAnd,
			Token::BitOr => BinOp::BitwiseOr,
			Token::And => BinOp::And,
			Token::Or => BinOp::Or,
			Token::ShiftLeft => BinOp::ShiftLeft,
			Token::ShiftRight => BinOp::ShiftRight,
			Token::Lt => BinOp::LessThan,
			Token::Gt => BinOp::GreaterThan,
			Token::OpenParen => BinOp::FuncCall,
			Token::OpenBracket => BinOp::Index,
			Token::Dot => BinOp::Member,
			Token::Cast => BinOp::Cast,
			Token::Bit => BinOp::BitCast,
			Token::Equal => BinOp::Set,
			Token::PlusSet => BinOp::SetAdd,
			Token::MinusSet => BinOp::SetSubtract,
			Token::StarSet => BinOp::SetMultiply,
			Token::SlashSet => BinOp::SetDivide,
			Token::BitAndSet => BinOp::SetAnd,
			Token::BitXorSet => BinOp::SetXor,
			Token::BitOrSet => BinOp::SetOr,
			Token::Equate => BinOp::Equal,
			Token::NotEqual => BinOp::NotEqual,
			_ => panic!("invalid binary operator token: {:?}", op),
		};
		AstNode::BinOp(op, Box::new(lhs), Box::new(rhs))
	}

	pub fn empty_qualifiers() -> AstNode {
		AstNode::Qualifiers(Vec::new())
	}

	pub fn cons_pointer_type(
		x: PointerType,
		list: Box<AstNode>,
	) -> Box<AstNode> {
		let (mut pointer, base_ty, array_size) = match *list {
			AstNode::Type {
				pointer,
				base_ty,
				array_size,
			} => (pointer, base_ty, array_size),
			_ => unreachable!(),
		};
		if pointer == PointerType::None {
			pointer = x;
			if array_size.is_some() {
				Box::new(AstNode::Type {
					pointer,
					base_ty: Box::new(AstNode::Type {
						pointer: PointerType::None,
						base_ty,
						array_size,
					}),
					array_size: None,
				})
			} else {
				Box::new(AstNode::Type {
					pointer,
					base_ty,
					array_size,
				})
			}
		} else {
			Box::new(AstNode::Type {
				pointer: x,
				base_ty: Box::new(AstNode::Type {
					pointer,
					base_ty,
					array_size,
				}),
				array_size: None,
			})
		}
	}

	pub fn set_var_initial(&mut self, init: Option<Box<AstNode>>) {
		match self {
			AstNode::Variable {
				ref mut initial, ..
			} => *initial = init,
			_ => panic!("you fool"),
		}
	}
}

impl Qualifier {
	pub fn from_token(token: Token) -> Self {
		match token {
			Token::Priv => Self::Private,
			Token::Const => Self::Constant,
			Token::Volatile => Self::Volatile,
			Token::Comptime => Self::CompileTime,
			Token::Noret => Self::NoReturn,
			Token::Align => panic!("cannot convert align directly"),
			_ => panic!("token is not a qualifier"),
		}
	}
}

pub struct Parser {
	pub tokens: Vec<Token>,
	pub index: usize,
}

impl Parser {
	pub fn new(tokens: Vec<Token>) -> Self {
		Self {
			tokens: tokens,
			index: 0,
		}
	}

	fn bump(&mut self) -> Option<Token> {
		let t = self.tokens.get(self.index);
		self.index += 1;
		t.cloned()
	}

	fn dump(&mut self, n: usize) {
		self.index += n;
	}

	fn peek_one(&self) -> Option<&Token> {
		self.tokens.get(self.index)
	}

	fn peek_two(&self) -> Option<&Token> {
		self.tokens.get(self.index + 1)
	}

	fn find(&mut self, kind: Token) -> bool {
		match self.peek_one() {
			Some(t) => {
				if std::mem::discriminant(t) == std::mem::discriminant(&kind) {
					self.bump();
					true
				} else {
					false
				}
			}
			None => false,
		}
	}

	fn peek_find(&self, kind: Token) -> bool {
		match self.peek_one() {
			Some(t) => {
				if std::mem::discriminant(t) == std::mem::discriminant(&kind) {
					true
				} else {
					false
				}
			}
			_ => false,
		}
	}

	#[allow(dead_code)]
	fn find_two(&mut self, k1: Token, k2: Token) -> bool {
		match (self.peek_one(), self.peek_two()) {
			(Some(t1), Some(t2)) => {
				if (std::mem::discriminant(t1) == std::mem::discriminant(&k1))
					&& (std::mem::discriminant(t2)
						== std::mem::discriminant(&k2))
				{
					self.bump();
					self.bump();
					true
				} else {
					false
				}
			}
			_ => false,
		}
	}

	fn expect(&mut self, kind: Token) -> Token {
		match self.bump() {
			Some(t) => {
				if std::mem::discriminant(&t) == std::mem::discriminant(&kind) {
					t
				} else {
					panic!("expected a: {:?}, found {:?}", kind, t);
				}
			}
			None => panic!("expected a: {:?}, found EOF", kind),
		}
	}

	pub fn parse_unit(mut self) -> AstNode {
		let mut nodes = Vec::new();
		loop {
			// Try and parse a module declaration
			let child = self.parse_module_decl();
			if child.is_some() {
				nodes.push(child.unwrap());
				continue;
			}
			// Try and parse an import declaration
			let child = self.parse_import_decl();
			if child.is_some() {
				nodes.push(child.unwrap());
				continue;
			}
			// Try and parse a use declaration
			let child = self.parse_use_decl();
			if child.is_some() {
				nodes.push(child.unwrap());
				continue;
			}
			// Try and parse a structure definition
			let child = self.parse_structure(false);
			if child.is_some() {
				nodes.push(child.unwrap());
				continue;
			}
			// Try and parse a function definition
			let child = self.parse_function();
			if child.is_some() {
				nodes.push(child.unwrap());
				continue;
			}
			// Try and parse a global variable definition
			let child = self.parse_variable();
			if child.is_some() {
				nodes.push(child.unwrap());
				continue;
			}
			// Try to parse spare semicolons
			if self.find(Token::SemiColon) {
				continue;
			}
			// We could not parse anything, this must be the end of the file
			if self.peek_one().is_some() {
				panic!("expected end of file, found: {:?}", self.bump());
			}
			break;
		}

		AstNode::Unit(nodes)
	}

	fn parse_symbol_path(&mut self, grouping: bool) -> AstNode {
		// Is it an absolute path?
		let absolute = self.find(Token::Dot);
		let mut base = Vec::new();
		let mut group = Vec::new();
		// There needs to be at least one identifier
		base.push(AstNode::ident(self.expect(Token::any_identifier())));
		while self.find(Token::Dot) {
			match self.bump() {
				// Continue to
				Some(t) if t.is_ident() => base.push(AstNode::ident(t)),
				Some(t) if t.is_equivalent(&Token::OpenBrace) => {
					if !grouping {
						panic!("grouping was not expected in this context");
					}
					group.push(AstNode::ident(
						self.expect(Token::any_identifier()),
					));
					while self.find(Token::Comma) {
						match self.peek_one() {
							Some(t) if t.is_ident() => {
								group.push(AstNode::ident(self.bump().unwrap()))
							}
							t => panic!("unexpected token in path: {:?}", t),
						}
					}
					self.expect(Token::CloseBrace);
				}
				_ => break,
			}
		}

		AstNode::Path {
			absolute,
			base,
			group,
		}
	}

	fn parse_module_decl(&mut self) -> Option<AstNode> {
		if self.find(Token::Module) {
			let path = self.parse_symbol_path(false);
			self.expect(Token::SemiColon);
			Some(AstNode::ModuleDecl(Box::new(path)))
		} else {
			None
		}
	}

	fn parse_import_decl(&mut self) -> Option<AstNode> {
		if self.find(Token::Import) {
			let path = self.parse_symbol_path(false);
			self.expect(Token::SemiColon);
			Some(AstNode::ImportDecl(Box::new(path)))
		} else {
			None
		}
	}

	fn parse_use_decl(&mut self) -> Option<AstNode> {
		if self.find(Token::Use) {
			let path = self.parse_symbol_path(true);
			self.expect(Token::SemiColon);
			Some(AstNode::UseDecl(Box::new(path)))
		} else {
			None
		}
	}

	fn parse_members(&mut self) -> Vec<(AstNode, AstNode, AstNode)> {
		fn member1(p: &mut Parser) -> Option<(AstNode, AstNode, AstNode)> {
			if p.peek_find(Token::any_identifier()) {
				let var = AstNode::ident(p.bump().unwrap());
				let qualifiers = p.parse_qualifiers();
				p.expect(Token::Colon);
				let ty = p.parse_type();
				Some((var, qualifiers, ty))
			} else {
				None
			}
		}
		let mut members = Vec::new();
		match member1(self) {
			None => return members,
			Some(x) => members.push(x),
		}
		while self.find(Token::Comma) {
			members.push(member1(self).unwrap());
		}
		members
	}

	fn parse_elements(&mut self) -> Vec<(AstNode, Option<AstNode>)> {
		fn element1(p: &mut Parser) -> Option<(AstNode, Option<AstNode>)> {
			if p.peek_find(Token::any_identifier()) {
				let var = AstNode::ident(p.bump().unwrap());
				if p.find(Token::Equal) {
					let val = p.parse_expr().unwrap();
					Some((var, Some(val)))
				} else {
					Some((var, None))
				}
			} else {
				None
			}
		}
		let mut elements = Vec::new();
		match element1(self) {
			None => return elements,
			Some(x) => elements.push(x),
		}
		while self.find(Token::Comma) {
			elements.push(element1(self).unwrap());
		}
		elements
	}

	fn parse_structure(&mut self, anonymous: bool) -> Option<AstNode> {
		fn parse_body(p: &mut Parser) -> AstNode {
			let mut nodes = Vec::new();
			loop {
				let child = p.parse_use_decl();
				if child.is_some() {
					nodes.push(child.unwrap());
					continue;
				}
				let child = p.parse_function();
				if child.is_some() {
					nodes.push(child.unwrap());
					continue;
				}
				let child = p.parse_variable();
				if child.is_some() {
					nodes.push(child.unwrap());
					continue;
				}
				break;
			}
			AstNode::Block(nodes)
		}
		fn parse_struct(
			p: &mut Parser,
			anonymous: bool,
		) -> (
			Box<AstNode>,
			Box<AstNode>,
			Vec<(AstNode, AstNode, AstNode)>,
			Option<Box<AstNode>>,
		) {
			let ident = if anonymous {
				AstNode::Nil
			} else {
				AstNode::ident(p.expect(Token::any_identifier()))
			};
			let qualifiers = if p.find(Token::Colon) {
				p.parse_qualifiers()
			} else {
				AstNode::empty_qualifiers()
			};
			p.expect(Token::OpenBrace);
			let members = p.parse_members();
			p.expect(Token::SemiColon);
			let body = if !p.find(Token::CloseBrace) {
				if anonymous {
					panic!("an anonymous struct/union cannot have a body");
				}
				let b = Some(Box::new(parse_body(p)));
				p.bump();
				b
			} else {
				None
			};

			(Box::new(ident), Box::new(qualifiers), members, body)
		}

		if self.find(Token::Struct) {
			let s = parse_struct(self, anonymous);
			Some(AstNode::Struct {
				identifier: s.0,
				qualifiers: s.1,
				members: s.2,
				body: s.3,
			})
		} else if self.find(Token::Union) {
			let u = parse_struct(self, anonymous);
			Some(AstNode::Union {
				identifier: u.0,
				qualifiers: u.1,
				members: u.2,
				body: u.3,
			})
		} else if self.find(Token::Enum) {
			let ident = if anonymous {
				AstNode::Nil
			} else {
				AstNode::ident(self.expect(Token::any_identifier()))
			};
			let qualifiers = if self.find(Token::Colon) {
				self.parse_qualifiers()
			} else {
				AstNode::empty_qualifiers()
			};
			self.expect(Token::OpenBrace);
			let elements = self.parse_elements();
			self.expect(Token::SemiColon);
			let body = if !self.find(Token::CloseBrace) {
				if anonymous {
					panic!("an anonymous enum cannot have a body");
				}
				let b = Some(Box::new(parse_body(self)));
				self.bump();
				b
			} else {
				None
			};

			Some(AstNode::Enum {
				identifier: Box::new(ident),
				qualifiers: Box::new(qualifiers),
				elements,
				body,
			})
		} else {
			None
		}
	}

	fn parse_function(&mut self) -> Option<AstNode> {
		fn parse_return_type(p: &mut Parser) -> Vec<AstNode> {
			if p.find(Token::Lt) {
				let mut rets = Vec::new();
				rets.push(p.parse_type());
				while p.find(Token::Comma) {
					rets.push(p.parse_type());
				}
				p.expect(Token::Gt);
				rets
			} else {
				vec![p.parse_type()]
			}
		}

		if self.peek_find(Token::any_identifier()) {
			if self.peek_two().is_some()
				&& !self.peek_two().unwrap().is_equivalent(&Token::OpenParen)
			{
				return None;
			}
			let identifier = self.bump().unwrap();
			self.expect(Token::OpenParen);
			let parameters = self.parse_parameters();
			self.expect(Token::CloseParen);
			let qualifiers = self.parse_qualifiers();
			self.expect(Token::Colon);
			let return_types = parse_return_type(self);
			let body = self.parse_compound_statement();

			Some(AstNode::Function {
				identifier: Box::new(AstNode::ident(identifier)),
				parameters,
				qualifiers: Box::new(qualifiers),
				return_types,
				body: Box::new(body),
			})
		} else {
			None
		}
	}

	fn parse_parameters(&mut self) -> Vec<(AstNode, AstNode, AstNode)> {
		fn parameter1(p: &mut Parser) -> Option<(AstNode, AstNode, AstNode)> {
			if p.peek_find(Token::any_identifier()) {
				let var = AstNode::ident(p.bump().unwrap());
				match var {
					AstNode::Identifier(i) if i == "void" => return None,
					_ => {}
				}
				let qualifiers = p.parse_qualifiers();
				p.expect(Token::Colon);
				let ty = p.parse_type();
				Some((var, qualifiers, ty))
			} else if p.find(Token::Vararg) {
				Some((AstNode::Nil, AstNode::Nil, AstNode::VarArgs))
			} else if p.peek_find(Token::Pointer)
				|| p.peek_find(Token::SelfType)
			{
				Some((AstNode::Nil, AstNode::Nil, p.parse_type()))
			} else {
				None
			}
		}

		let mut paramters = Vec::new();
		match parameter1(self) {
			None => return paramters,
			Some(x) => paramters.push(x),
		}
		while self.find(Token::Comma) {
			paramters.push(parameter1(self).unwrap());
		}

		paramters
	}

	fn parse_type(&mut self) -> AstNode {
		fn pointer1(p: &mut Parser) -> PointerType {
			if p.find(Token::Pointer) {
				if p.find(Token::Const) {
					PointerType::ConstPointer
				} else {
					PointerType::MutPointer
				}
			} else {
				PointerType::None
			}
		}
		fn build_type(
			ps: Vec<PointerType>,
			base: AstNode,
			array_size: Option<Box<AstNode>>,
		) -> Box<AstNode> {
			let b = Box::new(AstNode::Type {
				pointer: PointerType::None,
				base_ty: Box::new(base),
				array_size,
			});
			ps.iter()
				.rfold(b, |b, &pt| AstNode::cons_pointer_type(pt, b))
		}
		// Parse and store as many pointers as we can
		let mut pointers = Vec::new();
		loop {
			let p = pointer1(self);
			// Parse rest of the type
			if p == PointerType::None {
				break;
			}
			pointers.push(p);
		}
		// Parse either a base type or an array
		if self.find(Token::OpenBracket) {
			let size = Box::new(self.parse_expr().unwrap());
			self.expect(Token::SemiColon);
			let base = self.parse_type();
			self.expect(Token::CloseBracket);
			*build_type(pointers, base, Some(size))
		} else if self.peek_find(Token::any_identifier()) {
			let base = self.parse_symbol_path(false);
			*build_type(pointers, base, None)
		} else if self.find(Token::SelfType) {
			*build_type(pointers, AstNode::SelfType, None)
		} else if [Token::Struct, Token::Union, Token::Enum]
			.into_iter()
			.any(|q| self.peek_find(q))
		{
			let struc = self.parse_structure(true).unwrap();
			*build_type(pointers, struc, None)
		} else {
			panic!("unexpected token in type: {:?}", self.bump());
		}
	}

	fn parse_statement(&mut self) -> Option<AstNode> {
		// Try to parse a use declaration
		let node = self.parse_use_decl();
		if node.is_some() {
			return node;
		}
		// Try to parse a return statement
		let node = self.parse_return();
		if node.is_some() {
			return node;
		}
		// Try to parse a control structure
		let node = self.parse_control();
		if node.is_some() {
			return node;
		}
		// Try to parse a variable declaration
		let node = self.parse_variable();
		if node.is_some() {
			return node;
		}
		// Try to parse a compound statement block
		if self.peek_find(Token::OpenBrace) {
			return Some(self.parse_compound_statement());
		}
		// Try to parse an expression
		let node = self.parse_expr();
		if node.is_some() {
			self.expect(Token::SemiColon);
			return node;
		}
		// Try to parse spare semicolons
		if self.find(Token::SemiColon) {
			return self.parse_statement();
		}

		// Otherwise we have an empty statement
		None
	}

	fn parse_compound_statement(&mut self) -> AstNode {
		self.expect(Token::OpenBrace);
		let mut block = Vec::new();
		loop {
			let stmt = self.parse_statement();
			match stmt {
				Some(x) => block.push(x),
				None => break,
			}
		}
		self.expect(Token::CloseBrace);
		AstNode::Block(block)
	}

	fn parse_return(&mut self) -> Option<AstNode> {
		if self.find(Token::Ret) {
			let mut exprs = Vec::new();
			loop {
				match self.parse_expr() {
					Some(e) => exprs.push(e),
					None => break,
				}
				if !self.find(Token::Comma) {
					break;
				}
			}
			self.expect(Token::SemiColon);
			Some(AstNode::Return(exprs))
		} else {
			None
		}
	}

	fn parse_control(&mut self) -> Option<AstNode> {
		fn parse_if(p: &mut Parser) -> AstNode {
			p.expect(Token::OpenParen);
			let (precond, cond) = {
				let mut precond = None;
				let cond;

				// Try to parse a variable decl and condition
				let c1 = p.parse_variable();
				if c1.is_some() {
					precond = Some(Box::new(c1.unwrap()));
					cond = p.parse_expr().unwrap();
				// If we can't just parse the condition
				} else {
					cond = p.parse_expr().unwrap();
				}
				(precond, Box::new(cond))
			};
			p.expect(Token::CloseParen);
			let block = p.parse_statement().unwrap();
			let else_body = if p.peek_find(Token::Else) {
				let mut block = Vec::new();
				if p.peek_two().is_some()
					&& p.peek_two().unwrap().is_equivalent(&Token::If)
				{
					p.dump(2);
					block.push(parse_if(p));
				} else {
					p.bump();
					block.push(p.parse_statement().unwrap());
				}
				Some(Box::new(AstNode::Block(block)))
			} else {
				None
			};

			AstNode::If {
				precond,
				cond,
				body: Box::new(block),
				else_body,
			}
		}

		fn parse_case(p: &mut Parser) -> Option<AstNode> {
			match p.peek_one() {
				Some(Token::Case) => {
					p.bump();
					let expr = p.parse_expr().unwrap();
					p.expect(Token::Arrow);
					let body = p.parse_statement().unwrap();
					Some(AstNode::Case {
						value: Box::new(expr),
						body: Box::new(body),
					})
				}
				Some(Token::Default) => {
					p.bump();
					p.expect(Token::Arrow);
					let body = p.parse_statement().unwrap();
					Some(AstNode::Default(Box::new(body)))
				}
				_ => None,
			}
		}

		match self.peek_one() {
			Some(Token::If) => {
				self.bump();
				Some(parse_if(self))
			}
			Some(Token::For) => {
				self.bump();
				self.expect(Token::OpenParen);
				let mut s1;
				s1 = self.parse_variable();
				if s1.is_none() {
					s1 = self.parse_expr();
					if s1.is_some() {
						self.expect(Token::SemiColon);
					}
				}
				let s2 = self.parse_expr().unwrap_or(AstNode::Boolean(true));
				self.expect(Token::SemiColon);
				let s3 = self.parse_expr();
				self.expect(Token::CloseParen);
				let body = self.parse_statement().unwrap();
				Some(AstNode::For {
					cond: Box::new((s1, s2, s3)),
					body: Box::new(body),
				})
			}
			Some(Token::Do) => {
				self.bump();
				let body = self.parse_statement().unwrap();
				self.expect(Token::While);
				self.expect(Token::OpenParen);
				let cond = self.parse_expr().unwrap();
				self.expect(Token::CloseParen);
				self.expect(Token::SemiColon);

				Some(AstNode::DoWhile {
					body: Box::new(body),
					cond: Box::new(cond),
				})
			}
			Some(Token::While) => {
				self.bump();
				self.expect(Token::OpenParen);
				let cond = self.parse_expr().unwrap();
				self.expect(Token::CloseParen);
				let body = self.parse_statement().unwrap();

				Some(AstNode::While {
					cond: Box::new(cond),
					body: Box::new(body),
				})
			}
			Some(Token::Loop) => {
				self.bump();
				Some(AstNode::Loop(Box::new(self.parse_statement().unwrap())))
			}
			Some(Token::Defer) => {
				self.bump();
				Some(AstNode::Defer(Box::new(self.parse_statement().unwrap())))
			}
			Some(Token::Switch) => {
				self.bump();
				self.expect(Token::OpenParen);
				let value = self.parse_expr().unwrap();
				self.expect(Token::CloseParen);
				self.expect(Token::OpenBrace);
				let mut cases = Vec::new();
				loop {
					match parse_case(self) {
						Some(x) => cases.push(x),
						None => break,
					}
				}
				self.expect(Token::CloseBrace);
				Some(AstNode::Switch {
					value: Box::new(value),
					cases,
				})
			}
			Some(Token::Break) => {
				self.bump();
				Some(AstNode::Break)
			}
			Some(Token::Continue) => {
				self.bump();
				Some(AstNode::Continue)
			}
			_ => None,
		}
	}

	fn parse_qualifiers(&mut self) -> AstNode {
		fn qualifier1(p: &mut Parser) -> Option<Qualifier> {
			if [
				Token::Priv,
				Token::Const,
				Token::Volatile,
				Token::Comptime,
				Token::Noret,
			]
			.into_iter()
			.any(|q| p.peek_find(q))
			{
				Some(Qualifier::from_token(p.bump().unwrap()))
			} else if p.find(Token::Align) {
				p.expect(Token::OpenParen);
				let q = match p.parse_expr() {
					Some(e) => Qualifier::Align(Box::new(e)),
					None => panic!(
						"cannot have an align statement without an align value"
					),
				};
				p.expect(Token::CloseParen);
				Some(q)
			} else {
				None
			}
		}
		let mut qualifiers = Vec::new();
		loop {
			let q = qualifier1(self);
			match q {
				Some(q) => qualifiers.push(q),
				None => break,
			}
		}
		AstNode::Qualifiers(qualifiers)
	}

	fn parse_variable(&mut self) -> Option<AstNode> {
		fn parse_definition(p: &mut Parser) -> Option<AstNode> {
			if p.peek_find(Token::any_identifier()) {
				// Check to see if this is actually a variable and not an
				// expression
				let peek_two = p.peek_two();
				if peek_two.is_some() {
					let peek_two = &peek_two.unwrap();
					// Check if a possible qualifier or a colon
					if ![
						Token::Priv,
						Token::Const,
						Token::Volatile,
						Token::Comptime,
						Token::Align,
						Token::Noret,
					]
					.into_iter()
					.any(|q| peek_two.is_equivalent(&q))
						&& !peek_two.is_equivalent(&Token::Colon)
					{
						return None;
					}
				}
				let var = AstNode::ident(p.expect(Token::any_identifier()));
				let qualifiers = p.parse_qualifiers();
				p.expect(Token::Colon);
				let ty = if !p.peek_find(Token::Equal)
					&& !p.peek_find(Token::Comma)
				{
					Some(Box::new(p.parse_type()))
				} else {
					None
				};
				Some(AstNode::Variable {
					identifier: Box::new(var),
					qualifiers: Box::new(qualifiers),
					ty,
					initial: None,
				})
			} else {
				None
			}
		}
		let mut vars = Vec::new();
		vars.push(parse_definition(self)?);
		while self.find(Token::Comma) {
			vars.push(parse_definition(self).unwrap());
		}
		if vars.len() == 1 {
			let initial = if self.find(Token::Equal) {
				let expr = self.parse_expr();
				match expr {
					Some(expr) => Some(Box::new(expr)),
					None => panic!(
						"expected expression following after `=' in variable \
						 declaration, found token: {:?}",
						self.bump()
					),
				}
			} else {
				None
			};
			self.expect(Token::SemiColon);
			vars[0].set_var_initial(initial);
			Some(vars.pop().unwrap())
		} else {
			self.expect(Token::Equal);
			let initial = match self.parse_expr() {
				Some(initial) => Box::new(initial),
				None => panic!(
					"expected expression following after `=' in variable \
					 declaration, found token: {:?}",
					self.bump()
				),
			};
			self.expect(Token::SemiColon);
			Some(AstNode::MultipleRetVariable { vars, initial })
		}
	}

	fn parse_arguments(&mut self) -> AstNode {
		let mut args = Vec::new();
		match self.parse_expr() {
			Some(e) => args.push(e),
			None => return AstNode::Arguments(args),
		}
		while self.find(Token::Comma) {
			match self.parse_expr() {
				Some(e) => args.push(e),
				None => panic!(
					"expected expression for argument but found: {:?}",
					self.bump()
				),
			}
		}
		AstNode::Arguments(args)
	}

	// StructLiteral(Vec<(Option<AstNode>, AstNode)>),

	fn parse_struct_lit(&mut self) -> AstNode {
		fn parse_key(p: &mut Parser) -> AstNode {
			if p.peek_find(Token::any_identifier()) {
				AstNode::ident(p.bump().unwrap())
			} else if p.peek_find(Token::any_integer()) {
				match p.expect(Token::any_integer()) {
					Token::IntegerLit { value, suffix } => {
						AstNode::Integer(value, suffix)
					}
					_ => unreachable!(),
				}
			} else if p.peek_find(Token::any_character()) {
				match p.expect(Token::any_character()) {
					Token::CharacterLit(s) => AstNode::Character(s),
					_ => unreachable!(),
				}
			} else {
				panic!("invalid token for struct literal key: {:?}", p.bump());
			}
		}
		fn item1(p: &mut Parser) -> (Option<AstNode>, AstNode) {
			let key = if p.find(Token::Dot) {
				let k = Some(parse_key(p));
				p.expect(Token::Equal);
				k
			} else {
				None
			};
			let value = p.parse_expr().unwrap();
			(key, value)
		}
		// Special case for empty struct literal
		if self.find(Token::CloseBrace) {
			return AstNode::StructLiteral(Vec::new());
		}
		let mut items = Vec::new();
		items.push(item1(self));
		while self.find(Token::Comma) {
			items.push(item1(self));
		}
		self.expect(Token::CloseBrace);
		AstNode::StructLiteral(items)
	}

	fn expression_base(&mut self) -> Option<AstNode> {
		// Try to parse an identifier or a path
		if self.peek_find(Token::any_identifier()) || self.peek_find(Token::Dot)
		{
			Some(self.parse_symbol_path(false))
		// Try to parse an integer literal
		} else if self.peek_find(Token::any_integer()) {
			match self.expect(Token::any_integer()) {
				Token::IntegerLit { value, suffix } => {
					Some(AstNode::Integer(value, suffix))
				}
				_ => unreachable!(),
			}
		// Try to parse a string
		} else if self.peek_find(Token::any_string()) {
			match self.expect(Token::any_string()) {
				Token::StringLit(s) => Some(AstNode::String(s)),
				_ => unreachable!(),
			}
		// Try to parse a character
		} else if self.peek_find(Token::any_character()) {
			match self.expect(Token::any_character()) {
				Token::CharacterLit(s) => Some(AstNode::Character(s)),
				_ => unreachable!(),
			}
		// Try to parse a true boolean
		} else if self.find(Token::True) {
			Some(AstNode::Boolean(true))
		// Try to parse a false boolean
		} else if self.find(Token::False) {
			Some(AstNode::Boolean(false))
		// Try to parse a null value
		} else if self.find(Token::Null) {
			Some(AstNode::Null)
		// Try to parse a reference to self
		} else if self.find(Token::SelfType) {
			Some(AstNode::SelfValue)
		// Try to parse a structure literal
		} else if self.find(Token::OpenBrace) {
			Some(self.parse_struct_lit())
		} else {
			None
		}
	}

	fn expr_bp(&mut self, min_bp: u8) -> Option<AstNode> {
		// Check to see that the next token isn't an EOF
		let peeked = self.peek_one()?.clone();
		// Try to parse a prefix operator
		let mut lhs = if prefix_binding_power(&peeked).is_some() {
			let (_, r_bp) = prefix_binding_power(&peeked).unwrap();
			self.bump();
			if peeked.is_equivalent(&Token::Cast)
				|| peeked.is_equivalent(&Token::Bit)
			{
				// parse type of cast
				self.expect(Token::OpenParen);
				let lhs = self.parse_type();
				self.expect(Token::CloseParen);
				// parse value of cast
				let rhs = self.expr_bp(r_bp).unwrap();
				AstNode::binop(peeked, lhs, rhs)
			} else {
				let rhs = match self.expr_bp(r_bp) {
					Some(x) => x,
					None => panic!(
						"expected expression after prefix unary op: {:?}",
						peeked
					),
				};
				AstNode::unaryop(peeked, rhs)
			}

		// Try to pass a bracketed expression
		} else if peeked.is_equivalent(&Token::OpenParen) {
			self.bump();
			let expr = match self.expr_bp(0) {
				Some(x) => x,
				None => panic!("cannot have empty expression \"()\""),
			};
			self.expect(Token::CloseParen);
			expr
		// Otherwise try to pass an expression base
		} else {
			self.expression_base()?
		};

		loop {
			// Get an operator, or if EOF, error
			let op = match self.peek_one() {
				Some(op) => op,
				None => panic!("unexpected EOF when parsing expression"),
			}
			.clone();

			if let Some((l_bp, _)) = postfix_binding_power(&op) {
				// If our next lowest bp is lower then our current, break out
				if l_bp < min_bp {
					break;
				}
				// Here we bump past the operator itself
				self.bump();

				// Check to see if we have a function
				if op.is_equivalent(&Token::OpenParen) {
					let rhs = self.parse_arguments();
					self.expect(Token::CloseParen);
					lhs = AstNode::binop(op, lhs, rhs);
				// Check to see if we have a index
				} else if op.is_equivalent(&Token::OpenBracket) {
					let rhs = match self.expr_bp(0) {
						Some(rhs) => rhs,
						None => panic!(
							"expected right hand side to operator: {:?}",
							op
						),
					};
					self.expect(Token::CloseBracket);
					lhs = AstNode::binop(op, lhs, rhs);
				// Otherwise we have a normal postfix operator
				} else {
					lhs = AstNode::unaryop(op, lhs);
				}
				continue;
			}

			if let Some((l_bp, r_bp)) = infix_binding_power(&op) {
				// If our next lowest bp is lower then our current, break out
				if l_bp < min_bp {
					break;
				}
				// Here we bump past the operator itself
				self.bump();
				// Parse the rhs of the expression
				let rhs = match self.expr_bp(r_bp) {
					Some(rhs) => rhs,
					None => {
						panic!("expected right hand side to operator: {:?}", op)
					}
				};

				lhs = AstNode::binop(op, lhs, rhs);
				continue;
			}
			break;
		}

		Some(lhs)
	}

	fn parse_expr(&mut self) -> Option<AstNode> {
		self.expr_bp(0)
	}
}

fn prefix_binding_power(op: &Token) -> Option<((), u8)> {
	use Token::*;
	match op {
		Plus | Minus | BitNot | Not | Ref | Deref | Cast | Bit | Sizeof
		| Alignof | Typeof | Offsetof => Some(((), 27)),
		_ => None,
	}
}

fn infix_binding_power(op: &Token) -> Option<(u8, u8)> {
	use Token::*;
	match op {
		Equal | BitAndSet | BitOrSet | BitXorSet | MinusSet | PlusSet
		| SlashSet | StarSet => Some((5, 6)),
		Or => Some((7, 8)),
		And => Some((9, 10)),
		BitOr => Some((11, 12)),
		BitXor => Some((13, 14)),
		BitAnd => Some((15, 16)),
		Equate | NotEqual => Some((17, 18)),
		Lt | Gt | LtEqual | GtEqual => Some((19, 20)),
		ShiftLeft | ShiftRight => Some((21, 22)),
		Plus | Minus => Some((23, 24)),
		Star | Slash => Some((25, 26)),
		Dot => Some((29, 30)),
		_ => None,
	}
}

fn postfix_binding_power(op: &Token) -> Option<(u8, ())> {
	use Token::*;
	match op {
		OpenParen | OpenBracket => Some((29, ())),
		_ => None,
	}
}

pub fn parse(tokens: Vec<Token>) -> AstNode {
	let parser = Parser::new(tokens);
	parser.parse_unit()
}
