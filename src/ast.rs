use crate::lexer::{IntegerSuffix, Token};

#[derive(Debug)]
pub struct Identifier(String);

#[derive(Debug)]
pub struct Qualifiers(Vec<Qualifier>);

#[derive(Debug)]
pub struct Path {
	pub absolute: bool,
	pub base: Vec<Identifier>,
	pub group: Vec<Identifier>,
}

#[derive(Debug)]
pub struct Unit {
	pub module: Path,
	pub imports: Vec<Path>,
	pub statements: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
	UseDecl(Path),
	Function(Function),
	Var(Variable),
	RetVar(MultiRetVariable),
	Structure(Structure),
	If {
		precond: Option<Box<Stmt>>,
		cond: Option<Box<Expr>>,
		true_body: Box<Stmt>,
		else_body: Option<Box<Stmt>>,
	},
	For {
		initial: Option<Box<Expr>>,
		initial_var: Option<Box<Variable>>,
		check: Box<Expr>,
		update: Option<Box<Expr>>,
		body: Box<Stmt>,
	},
	DoWhile {
		body: Box<Stmt>,
		cond: Box<Expr>,
	},
	While {
		cond: Box<Expr>,
		body: Box<Stmt>,
	},
	Switch {
		cond: Box<Expr>,
		cases: Vec<Case>,
		default: Option<Case>,
	},
	Loop(Box<Stmt>),
	Defer(Box<Stmt>),
	Return(Vec<Box<Expr>>),
	Break,
	Continue,
	Expression(Box<Expr>),
	Block(Vec<Stmt>),
}

#[derive(Debug)]
pub struct Function {
	pub ident: Identifier,
	pub parameters: Vec<(Identifier, Qualifiers, Type)>,
	pub qualifiers: Qualifiers,
	pub return_types: Vec<Type>,
	pub body: Box<Stmt>,
}

#[derive(Debug)]
pub struct Variable {
	pub ident: Identifier,
	pub qualifiers: Qualifiers,
	pub ty: Option<Type>,
	pub initial_val: Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct MultiRetVariable {
	pub bindings: Vec<Variable>,
	pub initial: Box<Expr>,
}

#[derive(Debug)]
pub enum Expr {
	Ident(Identifier),
	IntLit(u128, IntegerSuffix),
	StrLit(String),
	CharLit(char),
	BoolLit(bool),
	SelfVal,
	NullVal,
	StructLit(Vec<(Option<Box<Expr>>, Box<Expr>)>),
	Arguments(Vec<Box<Expr>>),
	BinOp(BinOp, Box<Expr>, Box<Expr>),
	Cast(BinOp, Type, Box<Expr>),
	UnaryOp(UnaryOp, Box<Expr>),
}

#[derive(Debug)]
pub enum BaseType {
	Ty(Path),
	Other(Box<Type>),
	VarArg,
	SelfType,
	AnonStructure(Structure),
}

#[derive(Debug)]
pub struct Type {
	pub pointer: PointerType,
	pub base_ty: BaseType,
	pub array_size: Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct Case {
	pub value: Box<Expr>,
	pub body: Box<Stmt>,
}

#[derive(Debug)]
pub enum Structure {
	Struct(Struct),
	Union(Union),
	Enum(Enum),
}

#[derive(Debug)]
pub struct Struct {
	pub ident: Identifier,
	pub qualifiers: Qualifiers,
	pub fields: Vec<(Identifier, Qualifiers, Type)>,
	pub body: Option<Box<Stmt>>,
}

#[derive(Debug)]
pub struct Union {
	pub ident: Identifier,
	pub qualifiers: Qualifiers,
	pub fields: Vec<(Identifier, Qualifiers, Type)>,
	pub body: Option<Box<Stmt>>,
}

#[derive(Debug)]
pub struct Enum {
	pub ident: Identifier,
	pub qualifiers: Qualifiers,
	pub elements: Vec<(Identifier, Option<Box<Expr>>)>,
	pub body: Option<Box<Stmt>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PointerType {
	None,
	MutPointer,
	ConstPointer,
}

#[derive(Debug)]
pub enum Qualifier {
	Private,
	Constant,
	Volatile,
	CompileTime,
	Align(Box<Expr>),
	NoReturn,
}

#[derive(Debug)]
pub enum BinOp {
	Add,
	Subtract,
	Multiply,
	Divide,
	Remainder,
	BitwiseXor,
	BitwiseAnd,
	BitwiseOr,
	And,
	Or,
	ShiftLeft,
	ShiftRight,
	LessThan,
	GreaterThan,
	Index,
	FuncCall,
	Member,
	Cast,
	BitCast,
	Set,
	SetAdd,
	SetSubtract,
	SetMultiply,
	SetDivide,
	SetAnd,
	SetXor,
	SetOr,
	Equal,
	NotEqual,
}

#[derive(Debug)]
pub enum UnaryOp {
	Positive,
	Negative,
	Not,
	Negate,
	Sizeof,
	Alignof,
	Typeof,
	Offsetof,
	Dereference,
	Reference,
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

impl Qualifiers {
	pub const fn nil() -> Qualifiers {
		Qualifiers(Vec::new())
	}
}

impl std::convert::From<Vec<Qualifier>> for Qualifiers {
	fn from(v: Vec<Qualifier>) -> Qualifiers {
		Qualifiers(v)
	}
}

impl Identifier {
	pub fn from(ident: String) -> Identifier {
		Identifier(ident)
	}

	pub const fn nil() -> Identifier {
		Identifier(String::new())
	}

	pub fn as_str(&self) -> &str {
		&self.0
	}
}

impl std::convert::From<Token> for Identifier {
	fn from(t: Token) -> Identifier {
		match t {
			Token::Identifier(i) => Identifier::from(i),
			_ => panic!(
				"cannot convert a token which is not `Token::Identifier` into \
				 an Identifier"
			),
		}
	}
}

impl Path {
	pub fn main_root() -> Path {
		Path {
			absolute: true,
			base: vec![Identifier::from("main".into())],
			group: Vec::new(),
		}
	}
}

impl Stmt {
	pub fn into_var(self) -> Variable {
		match self {
			Stmt::Var(var) => var,
			_ => panic!("statement is not a variable (was: {:?})", self),
		}
	}
}

impl Expr {
	pub fn binop(op: Token, lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr> {
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
		Box::new(Expr::BinOp(op, lhs, rhs))
	}

	pub fn unaryop(op: Token, lhs: Box<Expr>) -> Box<Expr> {
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
		Box::new(Expr::UnaryOp(op, lhs))
	}

	pub fn cast(op: Token, ty: Type, rhs: Box<Expr>) -> Box<Expr> {
		let op = match op {
			Token::Cast => BinOp::Cast,
			Token::Bit => BinOp::BitCast,
			_ => panic!("invalid cast operator token: {:?}", op),
		};
		Box::new(Expr::Cast(op, ty, rhs))
	}

	pub fn true_val() -> Box<Expr> {
		Box::new(Expr::BoolLit(true))
	}
	
	pub fn false_val() -> Box<Expr> {
		Box::new(Expr::BoolLit(false))
	}
}

impl Type {
	pub const fn varargs() -> Type {
		Type {
			pointer: PointerType::None,
			base_ty: BaseType::VarArg,
			array_size: None,
		}
	}

	pub fn from_base(base: BaseType) -> Self {
		Self {
			pointer: PointerType::None,
			base_ty: base,
			array_size: None,
		}
	}

	pub fn cons_pointer_type(x: PointerType, mut list: Type) -> Type {
		// If our current pointer is already none, update it
		if list.pointer == PointerType::None {
			list.pointer = x;
			// If our type is of an array, create new node rather than updating
			if list.array_size.is_some() {
				Type {
					pointer: list.pointer,
					base_ty: BaseType::Other(Box::new(Type {
						pointer: PointerType::None,
						base_ty: list.base_ty,
						array_size: list.array_size,
					})),
					array_size: None,
				}
			// Otherwise just return after updating pointer
			} else {
				list
			}
		// Otherwise nest another type
		} else {
			Type {
				pointer: x,
				base_ty: BaseType::Other(Box::new(list)),
				array_size: None,
			}
		}
	}
}
