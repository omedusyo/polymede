use crate::base::{
    ConstructorDeclaration, FunctionDeclaration, FunctionType, Program, RunDeclaration, Type,
    TypeDeclaration,
};
use crate::identifier::TypeVariable;
use crate::validation::base::{Error, ErrorInDeclaration, Result};
use std::collections::HashSet;

pub fn check_program(program: &Program) -> core::result::Result<(), Vec<ErrorInDeclaration>> {
    let mut errors = vec![];
    for decl in program.type_declarations.values() {
        match check_type_declaration(program, decl) {
            Ok(_) => {}
            Err(e) => errors.push(ErrorInDeclaration::Type(decl.name().clone(), e)),
        }
    }

    {
        let msg_type_decl = program.get_msg_type_declaration();
        match check_msg_type_declaration(msg_type_decl) {
            Ok(_) => {}
            Err(e) => errors.push(ErrorInDeclaration::Type(msg_type_decl.name().clone(), e)),
        }
    }

    for decl in program.function_declarations.values() {
        match check_types_in_function_declaration(program, decl) {
            Ok(_) => {}
            Err(e) => errors.push(ErrorInDeclaration::Function(decl.name().clone(), e)),
        }
    }

    for decl in &program.run_declaration {
        match check_types_in_run_declaration(program, decl) {
            Ok(_) => {}
            Err(e) => errors.push(ErrorInDeclaration::Run(e)),
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
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
        }
        Ind(decl) => {
            let mut type_env: TypeScope = TypeScope::new(&decl.type_parameters);
            type_env.add(&decl.recursive_type_var);
            for constructor_decl in decl.constructors.values() {
                check_constructor_declaration(
                    program,
                    &type_env,
                    constructor_decl,
                    Some(&decl.recursive_type_var),
                )?
            }
            Ok(())
        }
    }
}

fn check_msg_type_declaration(decl: &TypeDeclaration) -> Result<()> {
    let constructors = decl.constructors_in_source_ordering();

    use TypeDeclaration::*;
    let number_of_parameters = match decl {
        Enum(decl) => decl.type_parameters.len(),
        Ind(decl) => decl.type_parameters.len(),
    };
    if number_of_parameters > 0 {
        return Err(Error::MsgTypeCantHaveTypeParameters);
    }

    for constructor in constructors {
        for type_ in &constructor.parameters {
            if !type_.is_value_type() {
                return Err(Error::MsgTypeIsNotValueType {
                    received_type: type_.clone(),
                });
            }
        }
    }
    Ok(())
}

fn check_constructor_declaration(
    program: &Program,
    type_env: &TypeScope,
    decl: &ConstructorDeclaration,
    rec_var: Option<&TypeVariable>,
) -> Result<()> {
    for type_ in &decl.parameters {
        check_type(program, type_env, type_)?;
        if let Some(rec_var) = rec_var {
            check_positive_occurance(rec_var, type_)?
        }
    }
    Ok(())
}

fn check_types_in_function_declaration(
    program: &Program,
    decl: &FunctionDeclaration,
) -> Result<()> {
    match decl {
        FunctionDeclaration::User(decl) => {
            let type_env: TypeScope = TypeScope::new(&decl.type_parameters);
            check_function_type(program, &type_env, &decl.function.type_)
        }
        FunctionDeclaration::Foreign(decl) => {
            let type_env: TypeScope = TypeScope::new(&[]);
            check_function_type(program, &type_env, &decl.type_)
        }
    }
}

fn check_types_in_run_declaration(program: &Program, decl: &RunDeclaration) -> Result<()> {
    let type_env: TypeScope = TypeScope::new(&[]);
    let Type::Command(_) = &decl.body.type_ else {
        return Err(Error::RunExpressionDoesntHaveExpectedCommandType {
            received: decl.body.type_.clone(),
        });
    };
    check_type(program, &type_env, &decl.body.type_)
}

pub struct TypeScope {
    variables: HashSet<TypeVariable>,
}

impl TypeScope {
    pub fn new(vars: &[TypeVariable]) -> Self {
        let mut set = HashSet::new();
        for var in vars {
            set.insert(var.clone());
        }
        Self { variables: set }
    }

    pub fn add(&mut self, var: &TypeVariable) {
        self.variables.insert(var.clone());
    }

    pub fn exists(&self, variable: &TypeVariable) -> bool {
        self.variables.contains(variable)
    }
}

pub fn check_type(program: &Program, type_env: &TypeScope, type_: &Type) -> Result<()> {
    use Type::*;
    match type_ {
        VariableUse(type_var) => {
            if type_env.exists(type_var) {
                Ok(())
            } else {
                Err(Error::UndefinedTypeVariable {
                    variable: type_var.clone(),
                })
            }
        }
        TypeApplication(type_name, types) => match program.get_type_declaration(type_name) {
            Some(decl) => {
                if decl.arity() == types.len() {
                    for type_ in types {
                        check_type(program, type_env, type_)?;
                    }
                    Ok(())
                } else {
                    Err(Error::TypeConstructorIsApplliedToWrongNumberOfArguments {
                        expected: decl.arity(),
                        received: types.len(),
                    })
                }
            }
            None => Err(Error::TypeConstructorDoesntExist {
                type_name: type_name.clone(),
            }),
        },
        Arrow(function_type) => check_function_type(program, type_env, function_type),
        I32 => Ok(()),
        F32 => Ok(()),
        String => Ok(()),
        Command(type_) => check_type(program, type_env, type_),
    }
}

fn check_function_type(
    program: &Program,
    type_env: &TypeScope,
    function_type: &FunctionType,
) -> Result<()> {
    for type_ in &function_type.input_types {
        check_type(program, type_env, type_)?
    }
    check_type(program, type_env, &function_type.output_type)
}

#[derive(Debug, Copy, Clone)]
enum Polarity {
    Positive,
    Negative,
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
fn check_positive_occurance(type_var0: &TypeVariable, type_: &Type) -> Result<()> {
    fn check(type_var0: &TypeVariable, type_: &Type, polarity: Polarity) -> Result<()> {
        use Type::*;
        match type_ {
            VariableUse(type_var) => {
                if type_var == type_var0 {
                    match polarity {
                        Polarity::Positive => Ok(()),
                        Polarity::Negative => Err(
                            Error::NegativeOccuranceOfRecursiveTypeVariableInInductiveDeclaration {
                                variable: type_var.clone(),
                            },
                        ),
                    }
                } else {
                    Ok(())
                }
            }
            TypeApplication(_, types) => {
                for type_ in types {
                    check(type_var0, type_, polarity)?
                }
                Ok(())
            }
            Arrow(function_type) => {
                for type_ in &function_type.input_types {
                    check(type_var0, type_, polarity.flip())?
                }
                check(type_var0, &function_type.output_type, polarity)
            }
            I32 => Ok(()),
            F32 => Ok(()),
            String => Ok(()),
            Command(type_) => check(type_var0, type_, polarity),
        }
    }

    check(type_var0, type_, Polarity::Positive)
}
