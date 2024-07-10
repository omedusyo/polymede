use crate::parser::{
    identifier::{Variable, ConstructorName},
    base::{Program, TypeDeclaration, Type, FunctionType, ConstructorDeclaration, FunctionDeclaration, LetDeclaration, Term},
};

use std::collections::HashSet;

pub type Result<A> = std::result::Result<A, Error>;

#[derive(Debug)]
pub enum Error {
    TypeConstructorDoesntExist { constructor_name: ConstructorName },
    TypeConstructorIsApplliedToWrongNumberOfArguments { expected: usize, received: usize },
    UndefinedTypeVaraible { variable: Variable },
    NegativeOccuranceOfRecursiveTypeVariableInInductiveDeclaration { variable: Variable }
}

// ===Type Formation===
pub fn check_type_formation(program: &Program) -> Result<()> {
    for decl in program.type_declarations.values() {
        check_type_declaration(program, decl)?
    }

    for decl in program.function_declarations.values() {
        check_types_in_function_declaration(program, decl)?
    }

    for decl in program.let_declarations.values() {
        check_types_in_let_declaration(program, decl)?
    }

    Ok(())
}

fn check_type_declaration(program: &Program, decl: &TypeDeclaration) -> Result<()> {
    match decl {
        TypeDeclaration::Enum(decl) => {
            let type_env: TypeEnvironment = TypeEnvironment::new(&decl.type_parameters);
            for constructor_decl in &decl.constructors {
                check_constructor_declaration(program, &type_env, constructor_decl, None)?
            }
            Ok(())
        },
        TypeDeclaration::Ind(decl) => {
            let mut type_env: TypeEnvironment = TypeEnvironment::new(&decl.type_parameters);
            type_env.add(&decl.recursive_type_var);
            for constructor_decl in &decl.constructors {
                check_constructor_declaration(program, &type_env, constructor_decl, Some(&decl.recursive_type_var))?
            }
            Ok(())
        }
    }
}

fn check_constructor_declaration(program: &Program, type_env: &TypeEnvironment, decl: &ConstructorDeclaration, rec_var: Option<&Variable>) -> Result<()> {
    for type_ in &decl.parameters {
        check_type(program, type_env, type_)?;
        match rec_var {
            Some(rec_var) => check_positive_occurance(rec_var, type_)?,
            _ => {},
        }
    }
    Ok(())
}

fn check_types_in_function_declaration(program: &Program, decl: &FunctionDeclaration) -> Result<()> {
    let type_env: TypeEnvironment = TypeEnvironment::new(&decl.type_parameters);
    check_function_type(program, &type_env, &decl.function.type_)
}

fn check_types_in_let_declaration(program: &Program, decl: &LetDeclaration) -> Result<()> {
    let type_env: TypeEnvironment = TypeEnvironment::new(&decl.type_parameters);
    check_type(program, &type_env, &decl.body.type_)
}

struct TypeEnvironment {
    variables: HashSet<Variable>,
}

impl TypeEnvironment {
    pub fn new(vars: &[Variable]) -> Self {
        let mut set = HashSet::new();
        for var in vars {
            set.insert(var.clone());
        }
        Self { variables: set }
    }

    pub fn add(&mut self, var: &Variable) {
        self.variables.insert(var.clone());
    }

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
            check_function_type(program, type_env, function_type)
        }
    }
}

fn check_function_type(program: &Program, type_env: &TypeEnvironment, function_type: &FunctionType) -> Result<()> {
    for type_ in &function_type.input_types {
        check_type(program, type_env, type_)?
    }
    check_type(program, type_env, &function_type.output_type)
}

#[derive(Debug, Copy, Clone)]
enum Polarity {
    Positive,
    Negative
}

impl Polarity {
    pub fn flip(self) -> Self {
        use Polarity::*;
        match self {
            Positive => Negative,
            Negative => Positive,
        }
    }
}

// Foo(Bar(x), a)      positive occurance of x
// Fn(x -> a)          negative occurance of x
// Fn(Fn(x -> a) -> b) positive occurance of x
// Fn(x, a -> x)          has both positive and negative occurance of x
fn check_positive_occurance(type_var0: &Variable, type_: &Type) -> Result<()> {

    fn check(type_var0: &Variable, type_: &Type, polarity: Polarity) -> Result<()> {
        use Type::*;
        match type_{
            VariableUse(type_var) => {
                if type_var == type_var0 {
                    match polarity {
                        Polarity::Positive => Ok(()),
                        Polarity::Negative => Err(Error::NegativeOccuranceOfRecursiveTypeVariableInInductiveDeclaration { variable: type_var.clone() }),
                    }
                } else {
                    Ok(())
                }
            },
            TypeApplication(_, types) => {
                for type_ in types {
                    check(type_var0, type_, polarity)?
                }
                Ok(())
            },
            Arrow(function_type) => {
                for type_ in &function_type.input_types {
                    check(type_var0, type_, polarity.flip())?
                }
                check(type_var0, &function_type.output_type, polarity)
            }
        }
    }

    check(type_var0, type_, Polarity::Positive)
}

// ===Term Formation===
fn type_check(program: &Program, term: &Term, type_: &Type) -> Result<()> {
    todo!()
}

#[cfg(test)]
mod tests {
    #[test]
    fn example() {
    }
}
