use crate::parser::AstNode;

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
	Align(Box<AstNode>),
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
