use std::collections::HashMap;

use crate::ast::Identifier;

pub struct Env {
	table: HashMap<String, Identifier>
}

struct ScopeEnviroment;

struct AssociateTypes;
