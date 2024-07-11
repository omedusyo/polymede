use crate::parser::{
    identifier::{Variable, ConstructorName},
    base::{Program, TypeDeclaration, Type, FunctionType, ConstructorDeclaration, FunctionDeclaration, LetDeclaration, Term, type_apply},
};

use std::collections::HashSet;
use std::collections::HashMap;

pub type Result<A> = std::result::Result<A, Error>;

#[derive(Debug)]
pub enum Error {
    TypeConstructorDoesntExist { type_name: Variable },
    TypeConstructorIsApplliedToWrongNumberOfArguments { expected: usize, received: usize },
    UndefinedTypeVaraible { variable: Variable },
    NegativeOccuranceOfRecursiveTypeVariableInInductiveDeclaration { variable: Variable },

    VariableOutOfScope { variable: Variable },
    VariableDoesntHaveExpectedType { expected_type: Type, received_type: Type },

    TypeAnnotationDoesntMatchExpectedType { expected_type: Type, type_annotation: Type },
    TermIsConstructorButExpectedTypeIsNot { expected_type: Type },
    TermIsLambdaButExpectedTypeIsNotArrowType { expected_type: Type },
    ConstructorDoesntBelongToExpectedTypeDeclaration { constructor_name: ConstructorName, type_name: Variable },
    ConstructorIsApplliedToWrongNumberOfArguments { expected: usize, received: usize },
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

struct TypeScope {
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

fn check_type(program: &Program, type_env: &TypeScope, type_: &Type) -> Result<()> {
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

// ===Term Formation===
struct Environment<'a> {
    program: &'a Program,
    bindings: HashMap<Variable, Type>,
}

impl Environment<'_> {
    fn get_type(&self, var: &Variable) -> Option<&Type> {
        self.bindings.get(var)
    }

    fn get_type_declaration(&self, type_constructor_name: &ConstructorName) -> Option<&TypeDeclaration> {
        self.program.get_type_declaration(type_constructor_name)
    }
}

fn eq_type(type0: &Type, type1: &Type) -> bool {
    // TODO: Is this really enough?
    type0 == type1
}

fn type_infer(env: &Environment, term: &Term) -> Result<Type> {
    use Term::*;
    match term {
        TypedTerm(typed_term) => {
            let type_ = &typed_term.type_;
            let term = &typed_term.term;
            type_check(env, term, type_)?;
            Ok(type_.clone())
        },
        VariableUse(var) => {
            match env.get_type(var) {
                Some(type_) => Ok(type_.clone()),
                None => Err(Error::VariableOutOfScope { variable: var.clone() }),
            }
        },
        ConstructorUse(constructor_name, args) => {
            // TODO:
            //   if the constructor has no type parameters, then we can infer the type.
            todo!()
        },
        Lambda(function) => {
            todo!()
        },
        FunctionApplication(fn_name, args) => {
            // type infer args, then feed them into fn_name
            todo!()
        },
        Match(arg, branches) => {
            todo!()
        },
        Fold(arg, branches) => {
            todo!()
        },
        LambdaApplication(fn_term, args) => {
            todo!()
        },
        Let(bindings, body) => {
            todo!()
        },
    }
}

fn type_check(env: &Environment, term: &Term, expected_type: &Type) -> Result<()> {
    use Term::*;
    match term {
        TypedTerm(typed_term) => {
            // TODO: You need to check that the type of the type annotation is well-formed.
            let type_annotation = &typed_term.type_;
            if eq_type(type_annotation, expected_type) {
                let term = &typed_term.term;
                type_check(env, term, expected_type)
            } else {
                Err(Error::TypeAnnotationDoesntMatchExpectedType { expected_type: expected_type.clone(), type_annotation: type_annotation.clone() })
            }
        },
        VariableUse(var) => {
            match env.get_type(var) {
                Some(type_) => {
                    if eq_type(expected_type, type_) {
                        Ok(())
                    } else {
                        Err(Error::VariableDoesntHaveExpectedType { expected_type: expected_type.clone(), received_type: type_.clone() })
                    }
                },
                None => Err(Error::VariableOutOfScope { variable: var.clone() }),
            }
        },
        ConstructorUse(constructor_name, args) => {
            match expected_type {
                Type::TypeApplication(type_constructor_name, types) => {
                    // SAFETY: We assume that `expected_type` is a valid type.
                    let Some(type_decl) = env.get_type_declaration(type_constructor_name) else { unreachable!() };
                    match type_decl.type_apply_constructor(constructor_name, types) {
                        Some((constructor_decl, specialized_types)) => {
                            if constructor_decl.arity() != args.len() {
                                return Err(Error::ConstructorIsApplliedToWrongNumberOfArguments { expected: constructor_decl.arity(), received: args.len() })
                            }

                            for (arg, type_) in args.iter().zip(&specialized_types) { 
                                type_check(env, arg, &type_)?
                            }

                            Ok(())
                        },
                        None => Err(Error::ConstructorDoesntBelongToExpectedTypeDeclaration { constructor_name: constructor_name.clone(), type_name: type_decl.name().clone() })
                    }
                },
                _ => Err(Error::TermIsConstructorButExpectedTypeIsNot { expected_type: expected_type.clone() })
            }
        },
        Lambda(function) => {
            match expected_type {
                Type::Arrow(fn_type) => {
                    todo!()
                },
                _ => Err(Error::TermIsLambdaButExpectedTypeIsNotArrowType { expected_type: expected_type.clone() })
            }
        },
        FunctionApplication(fn_name, args) => {
            // Infer types of `args`
            // TODO: 1. lookup the function type (what if it is polymorphic? We need to know how to
            // instantiate these types...)
            //       2. for each argument type_check that it has correct type (given by the
            //          function)
            //       3. type_check that the output type has expected_type
            todo!()
        },
        Match(arg, branches) => {
            todo!()
        },
        Fold(arg, branches) => {
            todo!()
        },
        LambdaApplication(fn_term, args) => {
            todo!()
        },
        Let(bindings, body) => {
            todo!()
        },
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn example() {
    }
}
