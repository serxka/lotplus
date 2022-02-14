use std::rc::Rc;
use std::collections::HashMap;

use crate::lexer::{IntegerSuffix, Token};

pub trait Map {
	fn map_unit(&mut self, u: Unit) -> Unit;
	fn map_path(&mut self, p: Path) -> Path;
	fn map_stmt(&mut self, s: Stmt) -> Stmt;
	fn map_expr(&mut self, e: Expr) -> Expr;
	fn map_type(&mut self, t: Type) -> Type;
	fn map_struc(&mut self, s: Structure) -> Structure;
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Identifier {
	name: String,
	ty: Option<Rc<Type>>
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Qualifiers(Vec<Qualifier>);

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Path {
	pub absolute: bool,
	pub base: Vec<Identifier>,
	pub group: Vec<Identifier>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Unit {
	pub module: Path,
	pub imports: Vec<Path>,
	pub statements: Vec<Stmt>,
	pub env: Rc<Env>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Stmt {
	UseDecl(Path),
	Function(Function),
	Var(Variable),
	RetVar(MultiRetVariable),
	Structure(Structure),
	If {
		precond: Option<Box<Stmt>>,
		cond: Option<Rc<Expr>>,
		true_body: Box<Stmt>,
		else_body: Option<Box<Stmt>>,
	},
	For {
		initial: Option<Rc<Expr>>,
		initial_var: Option<Box<Variable>>,
		check: Rc<Expr>,
		update: Option<Rc<Expr>>,
		body: Box<Stmt>,
	},
	DoWhile {
		body: Box<Stmt>,
		cond: Rc<Expr>,
	},
	While {
		cond: Rc<Expr>,
		body: Box<Stmt>,
	},
	Switch {
		cond: Rc<Expr>,
		cases: Vec<Case>,
		default: Option<Case>,
	},
	Loop(Box<Stmt>),
	Defer(Box<Stmt>),
	Return(Vec<Rc<Expr>>),
	Break,
	Continue,
	Expression(Rc<Expr>),
	Block(Vec<Stmt>),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Function {
	pub ident: Identifier,
	pub parameters: Vec<(Identifier, Qualifiers, Type)>,
	pub qualifiers: Qualifiers,
	pub return_types: Vec<Type>,
	pub body: Box<Stmt>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Variable {
	pub ident: Identifier,
	pub qualifiers: Qualifiers,
	pub ty: Option<Type>,
	pub initial_val: Option<Rc<Expr>>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct MultiRetVariable {
	pub bindings: Vec<Variable>,
	pub initial: Rc<Expr>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Expr {
	Ident(Identifier),
	IntLit(u128, IntegerSuffix),
	StrLit(String),
	CharLit(char),
	BoolLit(bool),
	SelfVal,
	NullVal,
	StructLit(Vec<(Option<Rc<Expr>>, Rc<Expr>)>),
	Arguments(Vec<Rc<Expr>>),
	BinOp(BinOp, Rc<Expr>, Rc<Expr>),
	Cast(BinOp, Type, Rc<Expr>),
	UnaryOp(UnaryOp, Rc<Expr>),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum BaseType {
	Ty(Path),
	Other(Box<Type>),
	VarArg,
	SelfType,
	AnonStructure(Structure),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Type {
	pub pointer: PointerType,
	pub base_ty: BaseType,
	pub array_size: Option<Rc<Expr>>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Case {
	pub value: Rc<Expr>,
	pub body: Box<Stmt>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Structure {
	Struct(Struct),
	Union(Union),
	Enum(Enum),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Struct {
	pub ident: Identifier,
	pub qualifiers: Qualifiers,
	pub fields: Vec<(Identifier, Qualifiers, Type)>,
	pub body: Option<Box<Stmt>>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Union {
	pub ident: Identifier,
	pub qualifiers: Qualifiers,
	pub fields: Vec<(Identifier, Qualifiers, Type)>,
	pub body: Option<Box<Stmt>>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Enum {
	pub ident: Identifier,
	pub qualifiers: Qualifiers,
	pub elements: Vec<(Identifier, Option<Rc<Expr>>)>,
	pub body: Option<Box<Stmt>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PointerType {
	None,
	MutPointer,
	ConstPointer,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Qualifier {
	Private,
	Constant,
	Volatile,
	CompileTime,
	Align(Rc<Expr>),
	NoReturn,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
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

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
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
		Identifier { name: ident, ty: None }
	}

	pub const fn nil() -> Identifier {
		Identifier { name: String::new(), ty: None }
	}

	pub fn as_str(&self) -> &str {
		&self.name
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
	
	pub fn new() -> Path {
		Path {
			absolute: false,
			base: Vec::new(),
			group: Vec::new(),
		}
	}
	
	pub fn push(&mut self, path: &str) {
		self.base.push(Identifier::from(path.into()))
	}
	
	pub fn push_group(&mut self, path: &str) {
		self.group.push(Identifier::from(path.into()))
	}
}

#[macro_export]
macro_rules! path {
    () => (
        $crate::__rust_force_expr!($crate::ast::Path::new())
    );
    ($($x:expr),+ $(,)?) => (
        {
        	let mut path = $crate::ast::Path::new();
        	$(
        		path.push($x);
        	),+
        	path
        }
    );
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
	pub fn binop(op: Token, lhs: Rc<Expr>, rhs: Rc<Expr>) -> Rc<Expr> {
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
		Rc::new(Expr::BinOp(op, lhs, rhs))
	}

	pub fn unaryop(op: Token, lhs: Rc<Expr>) -> Rc<Expr> {
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
		Rc::new(Expr::UnaryOp(op, lhs))
	}

	pub fn cast(op: Token, ty: Type, rhs: Rc<Expr>) -> Rc<Expr> {
		let op = match op {
			Token::Cast => BinOp::Cast,
			Token::Bit => BinOp::BitCast,
			_ => panic!("invalid cast operator token: {:?}", op),
		};
		Rc::new(Expr::Cast(op, ty, rhs))
	}

	pub fn true_val() -> Rc<Expr> {
		Rc::new(Expr::BoolLit(true))
	}
	
	pub fn false_val() -> Rc<Expr> {
		Rc::new(Expr::BoolLit(false))
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

#[derive(Debug, Eq, PartialEq)]
pub struct Env {
	table: HashMap<String, (Identifier, Rc<Expr>)>,
	types: HashMap<Path, Type>,
	prev: Option<Rc<Env>>,
}

impl Env {
	pub fn new() -> Self {
		Env {
			table: HashMap::new(),
			types: HashMap::new(),
			prev: None,
		}
	}

	pub fn parent(prev: Rc<Env>) -> Self {
		Env {
			table: HashMap::new(),
			types: HashMap::new(),
			prev: Some(prev),
		}
	}

	pub fn get(&self, id: &str) -> Option<&(Identifier, Rc<Expr>)> {
		let v = self.table.get(id);
		if v.is_none() && self.prev.is_some() {
			self.prev.as_ref().unwrap().get(id)
		} else {
			v
		}
	}

	pub fn set(&mut self, id: Identifier, expr: Rc<Expr>) {
		self.table.insert(id.as_str().to_owned(), (id, expr));
	}
}
