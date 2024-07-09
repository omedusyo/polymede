use crate::parser::{
    identifier::{Variable, ConstructorName},
    base::{Program, TypeDeclaration, Type}
};

pub type Result<A> = std::result::Result<A, Error>;

use std::collections::HashSet;

#[derive(Debug)]
pub enum Error {
    TypeConstructorDoesntExist { constructor_name: ConstructorName },
    TypeConstructorIsApplliedToWrongNumberOfArguments { expected: usize, received: usize },
    UndefinedTypeVaraible { variable: Variable },
}

fn check_type_declaration(program: &Program, decl: &TypeDeclaration) -> Result<()> {
    todo!()
}

struct TypeEnvironment {
    variables: HashSet<Variable>,
}

impl TypeEnvironment {
    pub fn exists(&self, variable: &Variable) -> bool {
        self.variables.contains(variable)
    }
}

fn check_type(program: &Program, type_env: &TypeEnvironment, type_: &Type) -> Result<()> {
    use Type::*;
    match type_{
        VariableUse(type_var) => {
            if type_env.exists(&type_var) {
                Ok(())
            } else {
                Err(Error::UndefinedTypeVaraible { variable: type_var.clone() })
            }
        },
        TypeApplication(type_constructor_name, types) => {
            // TODO: Check arity and check each type inside with the same env
            match program.get_type_declaration(type_constructor_name) {
                Some(decl) => {
                    if decl.arity() == types.len() {
                        for type_ in types {
                            check_type(program, type_env, type_)?;
                        }
                        Ok(())
                    } else {
                        Err(Error::TypeConstructorIsApplliedToWrongNumberOfArguments { expected: decl.arity(), received: types.len() })
                    }
                },
                None => Err(Error::TypeConstructorDoesntExist { constructor_name: type_constructor_name.clone() }),
            }
        },
        Arrow(function_type) => {
            for type_ in &function_type.input_types {
                check_type(program, type_env, type_)?
            }
            check_type(program, type_env, &function_type.output_type)
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn example() {
    }
}
