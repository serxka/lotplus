use std::rc::Rc;

use crate::ast::*;

pub struct ScopeEnviroment {
	current_env: Rc<Env>
}
impl Map for ScopeEnviroment {
	fn map_unit(&mut self, mut u: Unit) -> Unit {
		for _path in u.imports.iter() {
			// TODO: Improve scope resolution code
		}
		u.statements = u.statements.into_iter().map(|stmt| {
			self.map_stmt(stmt)
		}).collect();
		u
	}

	fn map_stmt(&mut self, _s: Stmt) -> Stmt {
		unimplemented!()
	}

	fn map_expr(&mut self, _e: Expr) -> Expr {
		unimplemented!()
	}

	fn map_struc(&mut self, _s: Structure) -> Structure {
		unimplemented!()
	}
	
	fn map_path(&mut self, _p: Path) -> Path {
		panic!();
	}
	
	fn map_type(&mut self, _: Type) -> Type {
		panic!();
	}
}
impl ScopeEnviroment {
	pub fn scope_unit(u: Unit) -> Unit {
		let mut scope = ScopeEnviroment {
			current_env: u.env.clone()
		};
		
		scope.map_unit(u)
	}
}

// struct AssociateTypes;
