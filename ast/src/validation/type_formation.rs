use crate::base::{Program, TypeDeclaration, Type, FunctionType, ConstructorDeclaration, FunctionDeclaration, LetDeclaration};
use crate::identifier::Variable;
use crate::validation:: base::{Result, Error, ErrorWithLocation};
use std::collections::HashSet;

pub fn check_program(program: &Program) -> core::result::Result<(), Vec<ErrorWithLocation>> {
    let mut errors = vec![];
    for decl in program.type_declarations.values() {
        match check_type_declaration(program, decl) {
            Ok(_) => {},
            Err(e) => errors.push(ErrorWithLocation::TypeDeclaration(decl.name().clone(), e)),
        }
    }

    for decl in program.function_declarations.values() {
        match check_types_in_function_declaration(program, decl) {
            Ok(_) => {},
            Err(e) => errors.push(ErrorWithLocation::TypeDeclaration(decl.name().clone(), e)),
        }
    }

    for decl in program.let_declarations.values() {
        match check_types_in_let_declaration(program, decl) {
            Ok(_) => {},
            Err(e) => errors.push(ErrorWithLocation::TypeDeclaration(decl.name().clone(), e)),
        }
    }

    Ok(())
}

fn check_type_declaration(program: &Program, decl: &TypeDeclaration) -> Result<()> {
    use TypeDeclaration::*;
    match decl {
        Enum(decl) => {
            let type_env: TypeScope = TypeScope::new(&decl.type_parameters);
            for constructor_decl in decl.constructors.values() {
                check_constructor_declaration(program, &type_env, constructor_decl, None)?
            }
            Ok(())
        },
        Ind(decl) => {
            let mut type_env: TypeScope = TypeScope::new(&decl.type_parameters);
            type_env.add(&decl.recursive_type_var);
            for constructor_decl in decl.constructors.values() {
                check_constructor_declaration(program, &type_env, constructor_decl, Some(&decl.recursive_type_var))?
            }
            Ok(())
        }
    }
}

fn check_constructor_declaration(program: &Program, type_env: &TypeScope, decl: &ConstructorDeclaration, rec_var: Option<&Variable>) -> Result<()> {
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
    let type_env: TypeScope = TypeScope::new(&decl.type_parameters);
    check_function_type(program, &type_env, &decl.function.type_)
}

fn check_types_in_let_declaration(program: &Program, decl: &LetDeclaration) -> Result<()> {
    let type_env: TypeScope = TypeScope::new(&decl.type_parameters);
    check_type(program, &type_env, &decl.body.type_)
}

pub struct TypeScope {
    variables: HashSet<Variable>,
}

impl TypeScope {
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

pub fn check_type(program: &Program, type_env: &TypeScope, type_: &Type) -> Result<()> {
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
                None => Err(Error::TypeConstructorDoesntExist { type_name: type_constructor_name.clone() }),
            }
        },
        Arrow(function_type) => {
            check_function_type(program, type_env, function_type)
        }
    }
}

fn check_function_type(program: &Program, type_env: &TypeScope, function_type: &FunctionType) -> Result<()> {
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
