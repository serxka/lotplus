use crate::ast::*;
use crate::lexer::Token;

macro_rules! error {
	($this:expr, $($arg:tt)*) => {{
		let fmt = format!($($arg)*);
		$this.error(&fmt);
	}}
}

macro_rules! error_fatal {
	($this:expr, $($arg:tt)*) => {{
		let fmt = format!($($arg)*);
		$this.error_fatal(&fmt);
	}}
}

macro_rules! unwrap {
	($this:expr, $func:expr, $($arg:tt)*) => {{
		match $func {
			Some(x) => x,
			None => {
				error_fatal!($this, $($arg)*);
				unreachable!()
			}
		}
	}}
}

pub struct Parser {
	tokens: Vec<Token>,
	index: usize,
	errors: usize,
}

impl Parser {
	pub fn new(tokens: Vec<Token>) -> Self {
		Self {
			tokens: tokens,
			index: 0,
			errors: 0,
		}
	}

	fn bump(&mut self) -> Option<Token> {
		let t = self.tokens.get(self.index);
		self.index += 1;
		t.cloned()
	}

	fn take(&mut self) -> Token {
		let t = self.tokens.get(self.index);
		self.index += 1;
		t.unwrap().clone()
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

	fn error(&mut self, msg: &str) {
		self.errors += 1;
		eprintln!("{}", msg);
		if self.errors > 10 {
			std::process::exit(1);
		}
	}

	fn error_fatal(&mut self, msg: &str) {
		eprintln!("FATAL: {}", msg);
		std::process::exit(1);
	}

	fn sep_by<F: FnMut(&mut Self)>(&mut self, t: Token, mut f: F) {
		while self.find(t.clone()) {
			f(self);
		}
	}

	fn end_by<F: FnMut(&mut Self)>(&mut self, t: Token, mut f: F) {
		while self.find(t.clone()) {
			f(self);
		}
		self.find(t);
	}

	pub fn parse_unit(mut self) -> Unit {
		let mut module = None;
		let mut imports = Vec::new();
		let mut statements = Vec::new();
		loop {
			// Try and parse a module declaration
			let m = self.parse_module_decl();
			if m.is_some() {
				// We have already parsed a module declaration, we can only have
				// one per file
				if module.is_some() {
					error!(
						self,
						"We have already parsed a module declaration ({:?}), \
						 tried to re-declare module as: {:?}",
						module.unwrap(),
						m
					);
				}
				module = m;
				continue;
			}
			// Try and parse an import declaration
			let i = self.parse_import_decl();
			if i.is_some() {
				imports.push(i.unwrap());
				continue;
			}
			// Try and parse a statement, however limit to only valid global
			// statements
			let s = self.parse_statement(true);
			if s.is_some() {
				statements.push(s.unwrap());
				continue;
			}
			// We could not parse anything, if the file was valid the next token
			// must be an EOF
			if self.peek_one().is_some() {
				error_fatal!(
					self,
					"Expected EOF however found tokens (such as: {:?})",
					self.bump().unwrap()
				);
			}

			// We have successfully parsed everything
			break;
		}
		let module = match module {
			Some(x) => x,
			None => {
				error!(
					self,
					"No module declaration was found, defaulting to \".main\""
				);
				Path::main_root()
			}
		};

		Unit {
			module,
			imports,
			statements,
		}
	}

	fn parse_symbol_path(&mut self, grouping: bool) -> Path {
		// Is it an absolute path?
		let absolute = self.find(Token::Dot);
		let mut base = Vec::new();
		let mut group = Vec::new();
		// There needs to be at least one identifier in the path
		let ident = self.expect(Token::any_ident());
		base.push(ident.into());
		// While we find another section of the path parse it
		self.sep_by(Token::Dot, |p| {
			match p.take() {
				// Continue to parse another segment
				t if t.is_ident() => base.push(t.into()),
				// Alternatively parse the group of the path
				t if t.is_equivalent(&Token::OpenBrace) => {
					if !grouping {
						error!(
							p,
							"Path grouping was not expected in this context"
						);
					}
					let ident = p.expect(Token::any_ident());
					group.push(ident.into());
					// Continue to parse the group elements
					p.end_by(Token::Comma, |p| match p.bump() {
						Some(t) if t.is_ident() => group.push(t.into()),
						t => error!(p, "Unexpected token in path: {:?}", t),
					});
					// We should find a close brace at the end of the group
					p.expect(Token::CloseBrace);
				}
				t => error!(p, "Unexpected token in path: {:?}", t),
			}
		});

		Path {
			absolute,
			base,
			group,
		}
	}

	fn parse_module_decl(&mut self) -> Option<Path> {
		if self.find(Token::Module) {
			let path = self.parse_symbol_path(false);
			self.expect(Token::SemiColon);
			Some(path)
		} else {
			None
		}
	}

	fn parse_import_decl(&mut self) -> Option<Path> {
		if self.find(Token::Import) {
			let path = self.parse_symbol_path(false);
			self.expect(Token::SemiColon);
			Some(path)
		} else {
			None
		}
	}

	fn parse_use_decl(&mut self) -> Option<Stmt> {
		if self.find(Token::Use) {
			let path = self.parse_symbol_path(true);
			self.expect(Token::SemiColon);
			Some(Stmt::UseDecl(path))
		} else {
			None
		}
	}

	fn parse_fields(&mut self) -> Vec<(Identifier, Qualifiers, Type)> {
		// Parse a single field of the struct
		fn field1(p: &mut Parser) -> Option<(Identifier, Qualifiers, Type)> {
			if p.peek_find(Token::any_ident()) {
				// Parse field name
				let field = p.take().into();
				// Any qualifiers
				let qualifiers = p.parse_qualifiers();
				// Must be followed by a token to separate type
				p.expect(Token::Colon);
				// Parse the type of the field
				let ty = p.parse_type();
				Some((field, qualifiers, ty))
			} else {
				None
			}
		}
		// Try and parse all the fields
		let mut fields = Vec::new();
		// We either have zero fields, or one or more
		match field1(self) {
			// If we have none, just return an empty array
			None => return fields,
			// Otherwise add it to our list of fields
			Some(x) => fields.push(x),
		}
		// Keep trying to parse fields while we find commas
		self.sep_by(Token::Comma, |p| {
			fields.push(field1(p).unwrap());
		});

		fields
	}

	fn parse_elements(&mut self) -> Vec<(Identifier, Option<Box<Expr>>)> {
		// Parse a single field of the enum
		fn element1(p: &mut Parser) -> Option<(Identifier, Option<Box<Expr>>)> {
			if p.peek_find(Token::any_ident()) {
				// Parse enum variant
				let variant = p.take().into();
				// See if it has a set value
				let value = if p.find(Token::Equal) {
					Some(unwrap!(
						p,
						p.parse_expr(),
						"Expected expression after `=` in enum variant"
					))
				} else {
					None
				};
				Some((variant, value))
			} else {
				None
			}
		}
		// Try and parse all the elements
		let mut elements = Vec::new();
		// We must have at least one variant in our enum
		elements.push(unwrap!(
			self,
			element1(self),
			"Enum must contain at least one variant"
		));
		// Keep trying to parse variants while we find commas
		self.sep_by(Token::Comma, |p| {
			elements.push(element1(p).unwrap());
		});

		elements
	}

	fn parse_structure(&mut self, anonymous: bool) -> Option<Structure> {
		fn parse_body(p: &mut Parser) -> Box<Stmt> {
			let mut stmts = Vec::new();
			loop {
				let s = p.parse_use_decl();
				if s.is_some() {
					stmts.push(s.unwrap());
					continue;
				}
				let s = p.parse_function();
				if s.is_some() {
					stmts.push(s.map(Stmt::Function).unwrap());
					continue;
				}
				let s = p.parse_variable();
				if s.is_some() {
					stmts.push(s.unwrap());
					continue;
				}
				break;
			}
			Box::new(Stmt::Block(stmts))
		}
		fn parse_struc<T, F: FnMut(&mut Parser) -> Vec<T>>(
			p: &mut Parser,
			mut parse_fields: F,
			anonymous: bool,
		) -> (Identifier, Qualifiers, Vec<T>, Option<Box<Stmt>>) {
			// Try and parse an identifier if necessary
			let ident = if anonymous {
				Identifier::nil()
			} else {
				p.expect(Token::any_ident()).into()
			};
			// Try and parse qualifiers if we have a colon to mark them
			let qualifiers = if p.find(Token::Colon) {
				p.parse_qualifiers()
			} else {
				Qualifiers::nil()
			};
			// The start of the structure block
			p.expect(Token::OpenBrace);
			// Fields within the structure
			let members = parse_fields(p);
			// We should have a semicolon to mark the end of fields
			p.expect(Token::SemiColon);
			// If we don't find a closing brace, we probably have a body to our
			// structure
			let body = if !p.peek_find(Token::CloseBrace) {
				if anonymous {
					error!(p, "an anonymous structure cannot have a body");
				}
				Some(parse_body(p))
			} else {
				None
			};
			// We should have a closing brace for our structure
			p.expect(Token::CloseBrace);

			(ident, qualifiers, members, body)
		}

		if self.find(Token::Struct) {
			let s = parse_struc(self, Self::parse_fields, anonymous);
			Some(Structure::Struct(Struct {
				ident: s.0,
				qualifiers: s.1,
				fields: s.2,
				body: s.3,
			}))
		} else if self.find(Token::Union) {
			let u = parse_struc(self, Self::parse_fields, anonymous);
			Some(Structure::Union(Union {
				ident: u.0,
				qualifiers: u.1,
				fields: u.2,
				body: u.3,
			}))
		} else if self.find(Token::Enum) {
			let e = parse_struc(self, Self::parse_elements, anonymous);
			Some(Structure::Enum(Enum {
				ident: e.0,
				qualifiers: e.1,
				elements: e.2,
				body: e.3,
			}))
		} else {
			None
		}
	}

	fn parse_function(&mut self) -> Option<Function> {
		fn parse_return_type(p: &mut Parser) -> Vec<Type> {
			let mut rets = Vec::new();
			// If we have multiple return values
			if p.find(Token::Lt) {
				// Get our first return type
				rets.push(p.parse_type());
				// Keep looking for more separated by commas
				p.sep_by(Token::Comma, |p| {
					rets.push(p.parse_type());
				});
				// Our return types closing bracket
				p.expect(Token::Gt);
			} else {
				// Parse a single type otherwise
				rets.push(p.parse_type())
			}
			rets
		}

		if self.peek_find(Token::any_ident()) {
			// Check to see that we do have an identifier followed by a `(`, if
			// we don't this isn't a function
			if self.peek_two().is_some()
				&& !self.peek_two().unwrap().is_equivalent(&Token::OpenParen)
			{
				return None;
			}
			// Take our function identifier
			let ident = self.take().into();
			// Parse our parameters
			self.expect(Token::OpenParen);
			let parameters = self.parse_parameters();
			self.expect(Token::CloseParen);
			// Parse any qualifiers and our type colon
			let qualifiers = self.parse_qualifiers();
			self.expect(Token::Colon);
			// Parse return type
			let return_types = parse_return_type(self);
			// Parse function body
			let body = self.parse_compound_statement();

			Some(Function {
				ident,
				parameters,
				qualifiers,
				return_types,
				body: Box::new(body),
			})
		} else {
			None
		}
	}

	fn parse_parameters(&mut self) -> Vec<(Identifier, Qualifiers, Type)> {
		fn parameter1(
			p: &mut Parser,
		) -> Option<(Identifier, Qualifiers, Type)> {
			if p.peek_find(Token::any_ident()) {
				// Parse an identifier for our parameter
				let x = p.take();
				println!("asd {:?}", x);
				let ident = match x {
					Token::Identifier(i) if i == "void" => return None,
					i => i.into(),
				};
				// Parse any qualifiers
				let qual = p.parse_qualifiers();
				// Expect a colon before our type
				p.expect(Token::Colon);
				// Parse our type
				let ty = p.parse_type();
				Some((ident, qual, ty))
			// If we don't see an identifier we might have a varargs
			} else if p.find(Token::Vararg) {
				Some((Identifier::nil(), Qualifiers::nil(), Type::varargs()))
			// Otherwise we might have reference to self
			} else if p.peek_find(Token::Pointer)
				|| p.peek_find(Token::SelfType)
			{
				Some((Identifier::nil(), Qualifiers::nil(), p.parse_type()))
			// If none of those work we don't have a parameter to parse
			} else {
				None
			}
		}

		let mut paramters = Vec::new();
		match parameter1(self) {
			None => return paramters,
			Some(x) => paramters.push(x),
		}
		self.sep_by(Token::Comma, |p| {
			paramters.push(parameter1(p).unwrap());
		});

		paramters
	}

	fn parse_type(&mut self) -> Type {
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
			base: Type,
			array_size: Option<Box<Expr>>,
		) -> Type {
			let b = Type {
				pointer: PointerType::None,
				base_ty: BaseType::Other(Box::new(base)),
				array_size,
			};
			ps.iter().rfold(b, |b, &pt| Type::cons_pointer_type(pt, b))
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

		// Try to parse an array
		if self.find(Token::OpenBracket) {
			// Parse the array size
			let size = self.parse_expr().unwrap();
			// Our type separator
			self.expect(Token::SemiColon);
			// Parse the array element type
			let base = self.parse_type();
			// The bracket should have a closing end
			self.expect(Token::CloseBracket);
			build_type(pointers, base, Some(size))
		// Try to parse an identifier as a base type
		} else if self.peek_find(Token::any_ident()) {
			let base = self.parse_symbol_path(false);
			let ty = Type::from_base(BaseType::Ty(base));
			build_type(pointers, ty, None)
		// Try to parse a self type
		} else if self.find(Token::SelfType) {
			let ty = Type::from_base(BaseType::SelfType);
			build_type(pointers, ty, None)
		// Try to parse an anonymous structure
		} else if [Token::Struct, Token::Union, Token::Enum]
			.into_iter()
			.any(|q| self.peek_find(q))
		{
			let struc = self.parse_structure(true).unwrap();
			let ty = Type::from_base(BaseType::AnonStructure(struc));
			build_type(pointers, ty, None)
		// We couldn't parse a type
		} else {
			panic!("unexpected token in type: {:?}", self.bump());
		}
	}

	fn parse_statement(&mut self, global_ctx: bool) -> Option<Stmt> {
		// Try to parse a use declaration
		let node = self.parse_use_decl();
		if node.is_some() {
			return node;
		}
		// Try and parse a structure definition
		let node = self.parse_structure(false);
		if node.is_some() {
			return Some(Stmt::Structure(node.unwrap()));
		}
		// Try to parse a variable declaration
		let node = self.parse_variable();
		if node.is_some() {
			return node;
		}
		if global_ctx {
			// Try to parse a function declaration
			let node = self.parse_function();
			if node.is_some() {
				return node.map(Stmt::Function);
			}
		}
		// Try to parse spare semicolons
		if self.find(Token::SemiColon) {
			return self.parse_statement(global_ctx);
		}

		if !global_ctx {
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
			// Try to parse a compound statement block
			if self.peek_find(Token::OpenBrace) {
				return Some(self.parse_compound_statement());
			}
			// Try to parse an expression
			let node = self.parse_expr();
			if node.is_some() {
				self.expect(Token::SemiColon);
				return Some(Stmt::Expression(node.unwrap()));
			}
		}

		// Otherwise we have an empty statement
		None
	}

	fn parse_compound_statement(&mut self) -> Stmt {
		self.expect(Token::OpenBrace);
		let mut block = Vec::new();
		loop {
			let stmt = self.parse_statement(false);
			match stmt {
				Some(x) => block.push(x),
				None => break,
			}
		}
		self.expect(Token::CloseBrace);
		Stmt::Block(block)
	}

	fn parse_return(&mut self) -> Option<Stmt> {
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
			Some(Stmt::Return(exprs))
		} else {
			None
		}
	}

	fn parse_control(&mut self) -> Option<Stmt> {
		fn parse_if(p: &mut Parser) -> Stmt {
			p.expect(Token::OpenParen);
			let precond = p.parse_variable().map(Box::new);
			let cond = Some(p.parse_expr().unwrap_or(Expr::false_val()));
			p.expect(Token::CloseParen);
			let true_body = Box::new(p.parse_statement(false).unwrap());
			let else_body = if p.peek_find(Token::Else) {
				let mut block = Vec::new();
				if p.peek_two().is_some()
					&& p.peek_two().unwrap().is_equivalent(&Token::If)
				{
					p.dump(2);
					block.push(parse_if(p));
				} else {
					p.bump();
					block.push(p.parse_statement(false).unwrap());
				}
				Some(Box::new(Stmt::Block(block)))
			} else {
				None
			};

			Stmt::If {
				precond,
				cond,
				true_body,
				else_body,
			}
		}

		fn parse_case(p: &mut Parser) -> Option<(bool, Case)> {
			match p.peek_one() {
				Some(Token::Case) => {
					p.bump();
					let expr = p.parse_expr().unwrap();
					p.expect(Token::Arrow);
					let body = p.parse_statement(false).unwrap();
					Some((
						false,
						Case {
							value: expr,
							body: Box::new(body),
						},
					))
				}
				Some(Token::Default) => {
					p.bump();
					p.expect(Token::Arrow);
					let body = p.parse_statement(false).unwrap();
					Some((
						true,
						Case {
							value: Expr::true_val(),
							body: Box::new(body),
						},
					))
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
				let initial_var = self.parse_variable().map(Stmt::into_var).map(Box::new);
				let mut initial = None;
				if initial_var.is_none() {
					initial = self.parse_expr();
					if initial.is_some() {
						self.expect(Token::SemiColon);
					}
				}
				let check = self.parse_expr().unwrap_or(Expr::true_val());
				self.expect(Token::SemiColon);
				let update = self.parse_expr();
				self.expect(Token::CloseParen);
				let body = Box::new(self.parse_statement(false).unwrap());
				Some(Stmt::For {
					initial, initial_var,
					check,
					update,
					body,
				})
			}
			Some(Token::Do) => {
				self.bump();
				let body = self.parse_statement(false).unwrap();
				self.expect(Token::While);
				self.expect(Token::OpenParen);
				let cond = self.parse_expr().unwrap();
				self.expect(Token::CloseParen);
				self.expect(Token::SemiColon);

				Some(Stmt::DoWhile {
					body: Box::new(body),
					cond,
				})
			}
			Some(Token::While) => {
				self.bump();
				self.expect(Token::OpenParen);
				let cond = self.parse_expr().unwrap();
				self.expect(Token::CloseParen);
				let body = self.parse_statement(false).unwrap();

				Some(Stmt::While {
					cond: cond,
					body: Box::new(body),
				})
			}
			Some(Token::Loop) => {
				self.bump();
				Some(Stmt::Loop(Box::new(self.parse_statement(false).unwrap())))
			}
			Some(Token::Defer) => {
				self.bump();
				Some(Stmt::Defer(Box::new(self.parse_statement(false).unwrap())))
			}
			Some(Token::Switch) => {
				self.bump();
				self.expect(Token::OpenParen);
				let cond = self.parse_expr().unwrap();
				self.expect(Token::CloseParen);
				self.expect(Token::OpenBrace);
				let mut cases = Vec::new();
				let mut default = None;
				loop {
					match parse_case(self) {
						Some((false, x)) => cases.push(x),
						Some((true, x)) => default = Some(x),
						None => break,
					}
				}
				self.expect(Token::CloseBrace);
				Some(Stmt::Switch {
					cond,
					cases,
					default,
				})
			}
			Some(Token::Break) => {
				self.bump();
				Some(Stmt::Break)
			}
			Some(Token::Continue) => {
				self.bump();
				Some(Stmt::Continue)
			}
			_ => None,
		}
	}

	fn parse_qualifiers(&mut self) -> Qualifiers {
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
				Some(Qualifier::from_token(p.take()))
			} else if p.find(Token::Align) {
				p.expect(Token::OpenParen);
				let q = match p.parse_expr() {
					Some(e) => Qualifier::Align(e),
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
		qualifiers.into()
	}

	fn parse_variable(&mut self) -> Option<Stmt> {
		fn parse_definition(p: &mut Parser) -> Option<Variable> {
			if p.peek_find(Token::any_ident()) {
				// Check to see if this is actually a variable and not an
				// expression
				if p.peek_two().is_some() {
					let peek_two = &p.peek_two().unwrap();
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
				// Parse the variable name
				let ident = p.take().into();
				// Parse an qualifiers
				let qualifiers = p.parse_qualifiers();
				// Parse the required type colon
				p.expect(Token::Colon);
				// If there is no `=` or `,` we have neither an expression or
				// another variable, we must try to parse a type
				let ty = if !p.peek_find(Token::Equal)
					&& !p.peek_find(Token::Comma)
				{
					Some(p.parse_type())
				} else {
					None
				};
				// Return our variable for now, don't set an initial value yet
				Some(Variable {
					ident,
					qualifiers,
					ty,
					initial_val: None,
				})
			} else {
				None
			}
		}
		// Try to parse as many variable declarations as we can
		let mut vars = Vec::new();
		vars.push(parse_definition(self)?);
		self.sep_by(Token::Comma, |p| {
			vars.push(parse_definition(p).unwrap());
		});
		// If we only found 1 variable, it's just a normal variable
		if vars.len() == 1 {
			// See and parse an initial value if there is one
			let initial = if self.find(Token::Equal) {
				let expr = self.parse_expr();
				match expr {
					Some(expr) => Some(expr),
					None => panic!(
						"expected expression following after `=' in variable \
						 declaration, found token: {:?}",
						self.bump()
					),
				}
			} else {
				None
			};
			// Variable must be followed by a semicolon
			self.expect(Token::SemiColon);
			vars[0].initial_val = initial;
			Some(Stmt::Var(vars.pop().unwrap()))
		// Otherwise if we find multiple this is a multiple return value
		} else {
			// We must have initial value for a multiple return value
			self.expect(Token::Equal);
			let initial = match self.parse_expr() {
				Some(initial) => initial,
				None => panic!(
					"expected expression following after `=' in variable \
					 declaration, found token: {:?}",
					self.bump()
				),
			};
			// Variable must be followed by a semicolon
			self.expect(Token::SemiColon);
			Some(Stmt::RetVar(MultiRetVariable {
				bindings: vars,
				initial,
			}))
		}
	}

	fn parse_arguments(&mut self) -> Box<Expr> {
		let mut args = Vec::new();
		match self.parse_expr() {
			Some(e) => args.push(e),
			None => return Box::new(Expr::Arguments(args)),
		}
		self.sep_by(Token::Comma, |p| match p.parse_expr() {
			Some(e) => args.push(e),
			None => panic!(
				"expected expression for argument but found: {:?}",
				p.bump()
			),
		});
		Box::new(Expr::Arguments(args))
	}

	fn parse_struct_lit(&mut self) -> Box<Expr> {
		// Try to parse the `.x` part of a struct lit key
		fn parse_key(p: &mut Parser) -> Box<Expr> {
			// If we index with an identifier
			let expr = if p.peek_find(Token::any_ident()) {
				Expr::Ident(p.take().into())
			// If we index with an integer
			} else if p.peek_find(Token::any_int()) {
				match p.take() {
					Token::IntegerLit { value, suffix } => {
						Expr::IntLit(value, suffix)
					}
					_ => unreachable!(),
				}
			// If we index with a character
			} else if p.peek_find(Token::any_char()) {
				match p.take() {
					Token::CharacterLit(c) => Expr::CharLit(c),
					_ => unreachable!(),
				}
			} else {
				panic!("invalid token for struct literal key: {:?}", p.bump());
			};
			Box::new(expr)
		}
		// Try to parse a single item of the struct
		fn item1(p: &mut Parser) -> (Option<Box<Expr>>, Box<Expr>) {
			// If there is a `.` we must have a key
			let key = if p.find(Token::Dot) {
				let k = Some(parse_key(p));
				// After the key there must be a `=`
				p.expect(Token::Equal);
				k
			} else {
				None
			};
			// Parse our value
			let value = p.parse_expr().unwrap();
			(key, value)
		}
		// Special case for empty struct literal
		if self.find(Token::CloseBrace) {
			return Box::new(Expr::StructLit(Vec::new()));
		}
		// Parse as many items as we can
		let mut items = Vec::new();
		items.push(item1(self));
		self.sep_by(Token::Comma, |p| {
			items.push(item1(p));
		});

		self.expect(Token::CloseBrace);
		Box::new(Expr::StructLit(items))
	}

	fn expression_base(&mut self) -> Option<Box<Expr>> {
		// Try to parse an identifier or a path
		let expr = if self.peek_find(Token::any_ident()) {
			Expr::Ident(self.take().into())
		// Try to parse an integer literal
		} else if self.peek_find(Token::any_int()) {
			match self.take() {
				Token::IntegerLit { value, suffix } => {
					Expr::IntLit(value, suffix)
				}
				_ => unreachable!(),
			}
		// Try to parse a string
		} else if self.peek_find(Token::any_str()) {
			match self.take() {
				Token::StringLit(s) => Expr::StrLit(s),
				_ => unreachable!(),
			}
		// Try to parse a character
		} else if self.peek_find(Token::any_char()) {
			match self.take() {
				Token::CharacterLit(s) => Expr::CharLit(s),
				_ => unreachable!(),
			}
		// Try to parse a true boolean
		} else if self.find(Token::True) {
			Expr::BoolLit(true)
		// Try to parse a false boolean
		} else if self.find(Token::False) {
			Expr::BoolLit(false)
		// Try to parse a null value
		} else if self.find(Token::Null) {
			Expr::NullVal
		// Try to parse a reference to self
		} else if self.find(Token::SelfType) {
			Expr::SelfVal
		// Try to parse a structure literal
		} else if self.find(Token::OpenBrace) {
			return Some(self.parse_struct_lit());
		} else {
			return None;
		};
		Some(Box::new(expr))
	}

	fn expr_bp(&mut self, min_bp: u8) -> Option<Box<Expr>> {
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
				Expr::cast(peeked, lhs, rhs)
			} else {
				let rhs = match self.expr_bp(r_bp) {
					Some(x) => x,
					None => panic!(
						"expected expression after prefix unary op: {:?}",
						peeked
					),
				};
				Expr::unaryop(peeked, rhs)
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
					lhs = Expr::binop(op, lhs, rhs);
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
					lhs = Expr::binop(op, lhs, rhs);
				// Otherwise we have a normal postfix operator
				} else {
					lhs = Expr::unaryop(op, lhs);
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

				lhs = Expr::binop(op, lhs, rhs);
				continue;
			}
			break;
		}

		Some(lhs)
	}

	fn parse_expr(&mut self) -> Option<Box<Expr>> {
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

pub fn parse(tokens: Vec<Token>) -> Unit {
	let parser = Parser::new(tokens);
	parser.parse_unit()
}
