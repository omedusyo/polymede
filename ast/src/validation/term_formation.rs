use crate::parser::{
    identifier::{Variable, FunctionName, ConstructorName},
    base::{Program, TypeDeclaration, Type, FunctionType, ConstructorDeclaration, FunctionDeclaration, LetDeclaration, Term, Pattern, PatternBranch },
};
use crate::validation:: {
    base::{Result, Error, ErrorWithLocation},
    type_formation,
    type_formation::TypeScope,
};
use std::collections::HashMap;

struct Environment<'a> {
    program: &'a Program,
    type_env: &'a TypeScope,
    // TODO: Consider having &Type
    bindings_stack: Vec<HashMap<Variable, Type>>,
}

pub fn check_program(program: &Program) -> core::result::Result<(), ErrorWithLocation> {
    for decl in program.function_declarations.values() {
        match check_function_declaration(program, decl) {
            Ok(_) => {},
            Err(e) => return Err(ErrorWithLocation::FunctionDeclaration(decl.name.clone(), e))
        }
    }

    for decl in program.let_declarations.values() {
        match check_let_declaration(program, decl) {
            Ok(_) => {},
            Err(e) => return Err(ErrorWithLocation::LetDeclaration(decl.name.clone(), e))
        }
    }
    Ok(())
}

fn check_let_declaration(program: &Program, decl: &LetDeclaration) -> Result<()> {
    let type_env = TypeScope::new(&decl.type_parameters);
    let mut env = Environment::new(program, &type_env);
    type_check(&mut env, &decl.body.term, &decl.body.type_)
}

fn check_function_declaration(program: &Program, decl: &FunctionDeclaration) -> Result<()> {
    let type_env = TypeScope::new(&decl.type_parameters);
    let mut env = Environment::new(program, &type_env);
    let function_type = &decl.function.type_;
    let function_parameters = &decl.function.function.parameters;

    env.open_env();
    for (var, type_) in function_parameters.iter().zip(&function_type.input_types) {
        env.extend(var, type_);
    }

    type_check(&mut env, &decl.function.function.body, &function_type.output_type)?;
    Ok(())
}

impl <'env>Environment<'env> {
    fn new(program: &'env Program, type_env: &'env TypeScope) -> Self {
        Self { program, type_env, bindings_stack: vec![HashMap::new()] }
    }

    pub fn get_type(&self, var: &Variable) -> Option<&Type> {
        for bindings in &self.bindings_stack {
            match bindings.get(var) {
                Some(type_) => return Some(type_),
                None => {},
            }
        }
        None
    }

    pub fn check_type_formation(&self, type_: &Type) -> Result<()> {
        type_formation::check_type(&self.program, &self.type_env, type_)
    }

    pub fn get_type_declaration(&self, type_name: &Variable) -> Option<&TypeDeclaration> {
        self.program.get_type_declaration(type_name)
    }

    pub fn get_type_declaration_of_constructor(&self, constructor_name: &ConstructorName) -> Option<&TypeDeclaration> {
        self.program.get_type_declaration_of_constructor(constructor_name)
    }

    pub fn is_ind_type_declaration(&self, type_name: &Variable) -> bool {
        match self.program.get_type_declaration(type_name) {
            Some(TypeDeclaration::Ind(_)) => true,
            _ => false,
        }
    }

    pub fn get_function_declaration(&self, function_name: &FunctionName) -> Option<&FunctionDeclaration> {
        self.program.get_function_declaration(function_name)
    }

    pub fn open_env(&mut self) {
        self.bindings_stack.push(HashMap::new())
    }

    pub fn close_env(&mut self) {
        let _ = self.bindings_stack.pop();
    }

    fn extend(&mut self, var: &Variable, type_: &Type) {
        match self.bindings_stack.last_mut() {
            Some(bindings) => {
                let _ = bindings.insert(var.clone(), type_.clone());
            },
            None => unreachable!(),
        }
    }

    // Assumes arities match.
    pub fn extend_multiple(&mut self, vars: &[Variable], types: &[Type]) {
        for (var, type_) in vars.iter().zip(types) {
            self.extend(var, type_)
        }
    }
}

fn eq_type(type0: &Type, type1: &Type) -> bool {
    // TODO: Is this really enough?
    type0 == type1
}

// We are expecting the `term` to be of Arrow type.
fn type_infer_arrow_inputs(env: &mut Environment, term: &Term, expected_out_type: &Type) -> Result<Vec<Type>> {
    use Term::*;
    match term {
        TypedTerm(typed_term) => {
            // TODO: You need to check that the type of the type annotation is well-formed.
            //let type_ = &typed_term.type_;
            //let term = &typed_term.term;
            //
            //let Type::Arrow(fn_type) = type_ else { return Err(Error::TermDoesntHaveExpectedArrowType { received: type_.clone() }) };
            //type_check(env, term, type_)?;
            //
            //eq_type(fn_type.output_type)
            //
            //Ok(fn_type.input_types.clone())
            todo!()
        },
        VariableUse(var) => {
            match env.get_type(var) {
                Some(type_) => todo!(),
                None => Err(Error::VariableOutOfScope { variable: var.clone() }),
            }
        },
        Lambda(function) => {
            todo!()
        },
        FunctionApplication(fn_name, type_args, args) => {
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
        ConstructorUse(_, _) => {
            // TODO: Error: Can't possibly be of Arrow type
            todo!()
        }
    }
}

fn type_infer(env: &mut Environment, term: &Term) -> Result<Type> {
    use Term::*;
    match term {
        TypedTerm(typed_term) => {
            let type_ = &typed_term.type_;
            let term = &typed_term.term;
            env.check_type_formation(type_)?;
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
            // If constructor has 0 type-parameters, then we can infer its type, otherwise ask for type-annotation.
            let Some(type_decl) = env.get_type_declaration_of_constructor(constructor_name) else { return Err(Error::ConstructorDoesntExist { constructor_name: constructor_name.clone() })};
            if type_decl.arity() > 0 { return Err(Error::UnableToInferTypeOfConstructor) }
            let Some((constructor_decl, specialized_types)) = type_decl.type_apply_constructor(constructor_name, &vec![]) else { unreachable!() };
            if constructor_decl.arity() != args.len() { return Err(Error::ConstructorIsAppliedToWrongNumberOfArguments { expected: constructor_decl.arity(), received: args.len() }) }

            for (arg, type_) in args.iter().zip(&specialized_types) { 
                type_check(env, arg, &type_)?
            }

            Ok(Type::TypeApplication(constructor_name.clone(), vec![]))
        },
        Lambda(_function) => {
            Err(Error::UnableToInferTypeOfLambda)
        },
        FunctionApplication(function_name, type_args, args) => {
            let Some(fn_decl) = env.get_function_declaration(function_name) else { return Err(Error::FunctionDoesntExist { function_name: function_name.clone() }) };
            let fn_type = fn_decl.type_apply(type_args);
            if fn_type.input_types.len() != args.len() { return Err(Error::ApplyingWrongNumberOfArgumentsToFunction { expected: fn_type.input_types.len(), received: args.len() }); }
            for (arg, type_) in args.iter().zip(&fn_type.input_types) {
                type_check(env, arg, type_)?
            }
            Ok(fn_type.output_type)
        },
        Match(_arg, _branches) => {
            // TODO: We could infer arg, then infer branches and check that all of the branches
            // match to the same type.
            Err(Error::UnableToInferTypeOfMatch)
        },
        Fold(_arg, _branches) => {
            Err(Error::UnableToInferTypeOfFold)
        },
        LambdaApplication(fn_term, args) => {
            let fn_type = type_infer(env, fn_term)?;
            let Type::Arrow(fn_type) = fn_type else { return Err(Error::TermDoesntHaveExpectedArrowType { received: fn_type }) };
            if fn_type.input_types.len() != args.len() { return Err(Error::ApplyingWrongNumberOfArgumentsToLambda { expected: fn_type.input_types.len(), received: args.len() }); }

            for (arg, type_) in args.iter().zip(&fn_type.input_types) {
                type_check(env, arg, type_)?;
            }
            Ok(fn_type.output_type)
        },
        Let(bindings, body) => {
            env.open_env();
            for (var, term) in bindings {
                let type_ = type_infer(env, term)?;
                env.extend(var, &type_);
            }
            let type_ = type_infer(env, body)?;
            env.close_env();
            Ok(type_)
        },
    }
}

fn type_check(env: &mut Environment, term: &Term, expected_type: &Type) -> Result<()> {
    use Term::*;
    match term {
        TypedTerm(typed_term) => {
            let type_annotation = &typed_term.type_;
            env.check_type_formation(type_annotation)?;
            if eq_type(type_annotation, expected_type) {
                let term = &typed_term.term;
                type_check(env, term, expected_type)
            } else {
                Err(Error::TypeAnnotationDoesntMatchExpectedType { expected_type: expected_type.clone(), received_type: type_annotation.clone() })
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
            let Type::TypeApplication(type_constructor_name, types) = expected_type else { return Err(Error::TermIsConstructorButExpectedTypeIsNot { expected_type: expected_type.clone() }) };
            // SAFETY: We assume that `expected_type` is a valid type.
            let Some(type_decl) = env.get_type_declaration(type_constructor_name) else { unreachable!() };
            let Some((constructor_decl, specialized_types)) = type_decl.type_apply_constructor(constructor_name, types) else { return Err(Error::ConstructorDoesntBelongToExpectedTypeDeclaration { constructor_name: constructor_name.clone(), type_name: type_decl.name().clone() }) };
            if constructor_decl.arity() != args.len() { return Err(Error::ConstructorIsAppliedToWrongNumberOfArguments { expected: constructor_decl.arity(), received: args.len() }) }

            for (arg, type_) in args.iter().zip(&specialized_types) { 
                type_check(env, arg, &type_)?
            }

            Ok(())
        },
        Lambda(function) => {
            match expected_type {
                Type::Arrow(fn_type) => {
                    if fn_type.input_types.len() != function.parameters.len() {
                        return Err(Error::LambdaHasWrongNumberOfArguments { expected: fn_type.input_types.len(), received: function.parameters.len() })
                    }

                    env.open_env();
                    env.extend_multiple(&function.parameters, &fn_type.input_types);
                    type_check(env, &function.body, &fn_type.output_type)?;
                    env.close_env();

                    Ok(())
                },
                _ => Err(Error::TermIsLambdaButExpectedTypeIsNotArrowType { expected_type: expected_type.clone() })
            }
        },
        FunctionApplication(function_name, type_args, args) => {
            let Some(fn_decl) = env.get_function_declaration(function_name) else { return Err(Error::FunctionDoesntExist { function_name: function_name.clone() }) };
            let fn_type = fn_decl.type_apply(type_args);
            if fn_type.input_types.len() != args.len() { return Err(Error::ApplyingWrongNumberOfArgumentsToFunction { expected: fn_type.input_types.len(), received: args.len() }); }
            for (arg, type_) in args.iter().zip(&fn_type.input_types) {
                type_check(env, arg, type_)?
            }
            if eq_type(&fn_type.output_type, expected_type) {
                Ok(())
            } else {
                Err(Error::FunctionOutputTypeDoesntMatchExpectedType { expected_type: expected_type.clone(), received_type: fn_type.output_type })
            }
        },
        Match(arg, branches) => {
            let arg_type = type_infer(env, arg)?;
            let Type::TypeApplication(type_name, type_args) = arg_type else { return Err(Error::AttemptToMatchNonEnumerableType { received_type: arg_type }) };
            for branch in branches {
                check_pattern_branch(env, branch, &type_name, &type_args, &expected_type)?;
            }
            Ok(())
        },
        Fold(arg, branches) => {
            let arg_type = type_infer(env, arg)?;
            let arg_type_copy = arg_type.clone(); // TODO: Can I get rid of this clone?
            let Type::TypeApplication(type_name, type_args) = arg_type else { return Err(Error::AttemptToMatchNonEnumerableType { received_type: arg_type }) };
            if env.is_ind_type_declaration(&type_name) {
                for branch in branches {
                    check_pattern_branch(env, branch, &type_name, &type_args, &expected_type)?;
                }
                Ok(())
            } else {
                Err(Error::AttemptToFoldNonIndType { received_type: arg_type_copy })
            }
        },
        LambdaApplication(fn_term, args) => {
            // There are two ways to do this:
            // 1. infer fn_term then check args
            // 2. infer args then check fn_term
            // We take the 1. approach.

            // TODO: We could actually try to infer the type of `fn_term` with the expectation of
            // it being an Arrow type whose otuput type is `expecated_type`.
            // But this would require writing another specialized type inference function.
            let fn_type = type_infer(env, fn_term)?;
            let Type::Arrow(fn_type) = fn_type else { return Err(Error::TermDoesntHaveExpectedArrowType { received: fn_type.clone() }) };
            if fn_type.input_types.len() != args.len() {
                return Err(Error::ApplyingWrongNumberOfArgumentsToLambda { expected: fn_type.input_types.len(), received: args.len() });
            }

            for (arg, type_) in args.iter().zip(&fn_type.input_types) {
                type_check(env, arg, type_)?
            }

            Ok(())
        },
        Let(bindings, body) => {
            env.open_env();
            for (var, term) in bindings {
                let type_ = type_infer(env, term)?;
                env.extend(var, &type_);
            }
            type_check(env, body, expected_type)?;
            env.close_env();
            Ok(())
        },
    }
}

fn check_pattern_branch(env: &mut Environment, branch: &PatternBranch, type_name: &Variable, type_args: &[Type], expected_type: &Type) -> Result<()>  {
    env.open_env();
    // The creation/cloning of the type here is a bit unfortunate.
    check_and_extend_pattern(env, &branch.pattern, &Type::TypeApplication(type_name.clone(), type_args.to_vec()), expected_type)?;
    type_check(env, &branch.body, expected_type)?;
    env.close_env();
    Ok(())
}

// Note this may extend current environment with new bindings
// The expected_type is the type expected of the body of the pattern (this is important for folds)
fn check_and_extend_pattern(env: &mut Environment, pattern: &Pattern, expected_pattern_type: &Type, expected_type: &Type) -> Result<()> {
    match pattern {
        Pattern::Constructor(constructor_name, patterns) => {
            let Type::TypeApplication(type_name, type_args) = expected_pattern_type else { return Err(Error::AttemptToMatchNonEnumerableType { received_type: expected_pattern_type.clone() }) };
            let Some(type_decl) = env.get_type_declaration(&type_name) else { return Err(Error::TypeConstructorDoesntExist { type_name: type_name.clone() }) };

            use TypeDeclaration::*;
            let Some((_, specialized_types)) = (match type_decl {
                Enum(decl) => decl.type_apply_constructor(constructor_name, type_args),
                Ind(decl) => decl.type_apply_constructor(constructor_name, type_args, expected_type),
            }) else {
                return Err(Error::ConstructorDoesntBelongToExpectedTypeDeclaration { constructor_name: constructor_name.clone(), type_name: type_decl.name().clone() }) 
            };

            if specialized_types.len() != patterns.len() { return Err(Error::PatternHasWrongNumberOfArguments { expected: specialized_types.len(), received: patterns.len() }) }
            
            for (pattern, specialized_type) in patterns.iter().zip(&specialized_types) {
                check_and_extend_pattern(env, pattern, specialized_type, expected_type)?
            }

            Ok(())
        },
        Pattern::Variable(var) => {
            env.extend(var, expected_pattern_type);
            Ok(())
        },
        Pattern::Anything(_) => {
            Ok(())
        },
    }
}
