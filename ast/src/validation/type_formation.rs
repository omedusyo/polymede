use crate::base::{
    ConstructorDefinition, FunctionDefinition, FunctionType, Program, RunDefinition, Type,
    TypeDefinition,
};
use crate::identifier::TypeVariable;
use crate::validation::base::{Error, ErrorInDefinition, Result};
use std::collections::HashSet;

pub fn check_program(program: &Program) -> core::result::Result<(), Vec<ErrorInDefinition>> {
    let mut errors = vec![];
    for def in program.type_definitions.values() {
        match check_type_definition(program, def) {
            Ok(_) => {}
            Err(e) => errors.push(ErrorInDefinition::Type(def.name().clone(), e)),
        }
    }

    {
        let msg_type_def = program.get_msg_type_definition();
        match check_msg_type_definition(msg_type_def) {
            Ok(_) => {}
            Err(e) => errors.push(ErrorInDefinition::Type(msg_type_def.name().clone(), e)),
        }
    }

    for def in program.function_definitions.values() {
        match check_types_in_function_definition(program, def) {
            Ok(_) => {}
            Err(e) => errors.push(ErrorInDefinition::Function(def.name().clone(), e)),
        }
    }

    for def in &program.run_definition {
        match check_types_in_run_definition(program, def) {
            Ok(_) => {}
            Err(e) => errors.push(ErrorInDefinition::Run(e)),
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn check_type_definition(program: &Program, def: &TypeDefinition) -> Result<()> {
    use TypeDefinition::*;
    match def {
        Enum(def) => {
            let type_env: TypeScope = TypeScope::new(&def.type_parameters);
            for constructor_def in def.constructors.values() {
                check_constructor_definition(program, &type_env, constructor_def, None)?
            }
            Ok(())
        }
        Ind(def) => {
            let mut type_env: TypeScope = TypeScope::new(&def.type_parameters);
            type_env.add(&def.recursive_type_var);
            for constructor_def in def.constructors.values() {
                check_constructor_definition(
                    program,
                    &type_env,
                    constructor_def,
                    Some(&def.recursive_type_var),
                )?
            }
            Ok(())
        }
    }
}

fn check_msg_type_definition(def: &TypeDefinition) -> Result<()> {
    let constructors = def.constructors_in_source_ordering();

    use TypeDefinition::*;
    let number_of_parameters = match def {
        Enum(def) => def.type_parameters.len(),
        Ind(def) => def.type_parameters.len(),
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

fn check_constructor_definition(
    program: &Program,
    type_env: &TypeScope,
    def: &ConstructorDefinition,
    rec_var: Option<&TypeVariable>,
) -> Result<()> {
    for type_ in &def.parameters {
        check_type(program, type_env, type_)?;
        if let Some(rec_var) = rec_var {
            check_positive_occurance(rec_var, type_)?
        }
    }
    Ok(())
}

fn check_types_in_function_definition(
    program: &Program,
    def: &FunctionDefinition,
) -> Result<()> {
    match def {
        FunctionDefinition::User(def) => {
            let type_env: TypeScope = TypeScope::new(&def.type_parameters);
            check_function_type(program, &type_env, &def.function.type_)
        }
        FunctionDefinition::Foreign(def) => {
            let type_env: TypeScope = TypeScope::new(&[]);
            check_function_type(program, &type_env, &def.type_)
        }
    }
}

fn check_types_in_run_definition(program: &Program, def: &RunDefinition) -> Result<()> {
    let type_env: TypeScope = TypeScope::new(&[]);
    let Type::Command(_) = &def.body.type_ else {
        return Err(Error::RunExpressionDoesntHaveExpectedCommandType {
            received: def.body.type_.clone(),
        });
    };
    check_type(program, &type_env, &def.body.type_)
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
        TypeApplication(type_name, types) => match program.get_type_definition(type_name) {
            Some(def) => {
                if def.arity() == types.len() {
                    for type_ in types {
                        check_type(program, type_env, type_)?;
                    }
                    Ok(())
                } else {
                    Err(Error::TypeConstructorIsApplliedToWrongNumberOfArguments {
                        expected: def.arity(),
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
                            Error::NegativeOccuranceOfRecursiveTypeVariableInInductiveDefinition {
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
