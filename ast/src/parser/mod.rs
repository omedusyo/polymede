pub mod base;
mod combinator;
pub mod identifier;
pub mod lex;
mod pattern;
mod program;
pub mod show;
mod special;
mod term;
mod types;

use crate::base::Program;
use crate::parser::base::Error;

pub fn parse_program(s: &str) -> Result<Program, Error> {
    base::parse_program(s)
}

#[cfg(test)]
mod tests {
    use crate::base::{RunDefinition, Term, Type, UserFunctionBinding};
    use crate::identifier::{interner, Identifier};
    use crate::parser::{
        base::{Error, PreEnumDefinition, PreIndDefinition, PreTypeDefinition, Result, State},
        program::{pre_program, run_definition, type_definition, user_function_definition},
        types::type_,
    };

    #[test]
    fn test_type_0() -> Result<()> {
        let mut interner = interner();
        let s = "Result(  Error,   x )";
        let mut state = State::new(s, &mut interner);

        let result = type_(&mut state);

        assert!(matches!(result, Ok(Type::TypeApplication(_, _))));
        let Ok(Type::TypeApplication(identifier, type_args)) = result else {
            unreachable!()
        };
        assert_eq!(type_args.len(), 2);

        assert_eq!(identifier.str(&mut interner), "Result");
        assert!(matches!(type_args[0], Type::TypeApplication(_, _)));
        assert!(matches!(type_args[1], Type::VariableUse(_)));

        let Type::TypeApplication(ref err_identifier, ref err_type_args) = type_args[0] else {
            unreachable!()
        };
        assert_eq!(err_type_args.len(), 0);
        assert_eq!(err_identifier.str(&mut interner), "Error");

        let Type::VariableUse(ref var_name) = type_args[1] else {
            unreachable!()
        };
        assert_eq!(var_name.str(&mut interner), "x");

        Ok(())
    }

    #[test]
    fn test_type_1() -> Result<()> {
        let mut interner = interner();
        let s = "Fn(A1, A2 -> A3)";
        let mut state = State::new(s, &mut interner);

        let result = type_(&mut state);

        assert!(matches!(result, Ok(Type::Arrow(_))));
        let Ok(Type::Arrow(fn_type)) = result else {
            unreachable!()
        };

        assert_eq!(fn_type.input_types.len(), 2);

        Ok(())
    }

    #[test]
    fn test_type_definition_0() -> Result<()> {
        let mut interner = interner();
        let s = "type Bool = enum { T | F }";
        let mut state = State::new(s, &mut interner);

        let result = type_definition(&mut state);

        assert!(matches!(result, Ok(PreTypeDefinition::Enum(_))));
        let Ok(PreTypeDefinition::Enum(PreEnumDefinition {
            name,
            type_parameters: _,
            constructors,
        })) = result
        else {
            unreachable!()
        };
        assert_eq!(name.str(&mut interner), "Bool");

        assert_eq!(constructors.len(), 2);
        assert_eq!(constructors[0].name.str(&mut interner), "T");
        assert_eq!(constructors[1].name.str(&mut interner), "F");

        Ok(())
    }

    #[test]
    fn test_type_definition_1() -> Result<()> {
        let mut interner = interner();
        let s = "type SomeType = enum { | Const | Unary(a) | Binary(Foo(x, Bar), y) | Ternary(a, b, c) }";
        let mut state = State::new(s, &mut interner);

        let result = type_definition(&mut state);

        assert!(matches!(result, Ok(PreTypeDefinition::Enum(_))));

        let Ok(PreTypeDefinition::Enum(PreEnumDefinition {
            name,
            type_parameters: _,
            constructors,
        })) = result
        else {
            unreachable!()
        };
        assert_eq!(name.str(&mut interner), "SomeType");

        assert_eq!(constructors.len(), 4);
        assert_eq!(constructors[0].name.str(&mut interner), "Const");
        assert_eq!(constructors[1].name.str(&mut interner), "Unary");
        assert_eq!(constructors[2].name.str(&mut interner), "Binary");
        assert_eq!(constructors[3].name.str(&mut interner), "Ternary");

        assert_eq!(constructors[0].parameters.len(), 0);
        assert_eq!(constructors[1].parameters.len(), 1);
        assert_eq!(constructors[2].parameters.len(), 2);
        assert_eq!(constructors[3].parameters.len(), 3);

        Ok(())
    }

    #[test]
    fn test_type_definition_2() -> Result<()> {
        let mut interner = interner();
        let s = "type Nat = ind { nat . Zero | Succ(nat) }";
        let mut state = State::new(s, &mut interner);

        let result = type_definition(&mut state);

        assert!(matches!(result, Ok(PreTypeDefinition::Ind(_))));
        let Ok(PreTypeDefinition::Ind(PreIndDefinition {
            name,
            type_parameters: _,
            recursive_type_var,
            constructors,
        })) = result
        else {
            unreachable!()
        };
        assert_eq!(name.str(&mut interner), "Nat");
        assert_eq!(recursive_type_var.str(&mut interner), "nat");

        assert_eq!(constructors.len(), 2);
        assert_eq!(constructors[0].name.str(&mut interner), "Zero");
        assert_eq!(constructors[1].name.str(&mut interner), "Succ");

        assert_eq!(constructors[0].parameters.len(), 0);
        assert_eq!(constructors[1].parameters.len(), 1);

        Ok(())
    }

    #[test]
    fn test_type_definition_3() -> Result<()> {
        let mut interner = interner();
        let s = "type List(a, ignored) = ind { list . Nil | Cons(a, list) }";
        let mut state = State::new(s, &mut interner);

        let result = type_definition(&mut state);

        assert!(matches!(result, Ok(PreTypeDefinition::Ind(_))));
        let Ok(PreTypeDefinition::Ind(PreIndDefinition {
            name,
            type_parameters,
            recursive_type_var,
            constructors,
        })) = result
        else {
            unreachable!()
        };
        assert_eq!(name.str(&mut interner), "List");

        assert_eq!(type_parameters.len(), 2);
        assert_eq!(type_parameters[0].str(&mut interner), "a");
        assert_eq!(type_parameters[1].str(&mut interner), "ignored");

        assert_eq!(recursive_type_var.str(&mut interner), "list");

        assert_eq!(constructors.len(), 2);
        assert_eq!(constructors[0].name.str(&mut interner), "Nil");
        assert_eq!(constructors[1].name.str(&mut interner), "Cons");

        assert_eq!(constructors[0].parameters.len(), 0);
        assert_eq!(constructors[1].parameters.len(), 2);

        Ok(())
    }

    #[test]
    fn test_type_definition_4() -> Result<()> {
        let mut interner = interner();
        let s = "type Foo(x, y, z, y, z) = enum { A | B }";
        let mut state = State::new(s, &mut interner);

        let result = type_definition(&mut state);

        assert!(matches!(result, Err(Error::DuplicateVariableNames { .. })));
        let Err(Error::DuplicateVariableNames { duplicates }) = result else {
            unreachable!()
        };
        assert_eq!(duplicates.len(), 2);
        assert_eq!(duplicates[0].str(&mut interner), "y");
        assert_eq!(duplicates[1].str(&mut interner), "z");

        Ok(())
    }

    #[test]
    fn test_function_definition_0() -> Result<()> {
        let mut interner = interner();
        let s = "fn square = # Nat -> Nat : { x . mul(x, x) }";
        let mut state = State::new(s, &mut interner);

        let result = user_function_definition(&mut state);
        assert!(matches!(result, Ok(_)));
        let UserFunctionBinding {
            name,
            type_parameters,
            function,
        } = result?;

        assert_eq!(name.str(&mut interner), "square");
        assert_eq!(type_parameters.len(), 0);

        let Type::TypeApplication(ref t0, _) = function.type_.input_types[0] else {
            unreachable!()
        };
        let Type::TypeApplication(ref t, _) = function.type_.output_type else {
            unreachable!()
        };
        assert_eq!(t0.str(&mut interner), "Nat");
        assert_eq!(t.str(&mut interner), "Nat");

        let Term::FunctionApplication(mul, _, args) = function.function.body else {
            unreachable!()
        };
        let Term::VariableUse(ref arg0) = args[0] else {
            unreachable!()
        };
        let Term::VariableUse(ref arg1) = args[1] else {
            unreachable!()
        };

        assert_eq!(mul.str(&mut interner), "mul");
        assert_eq!(arg0.str(&mut interner), "x");
        assert_eq!(arg1.str(&mut interner), "x");

        Ok(())
    }

    #[test]
    fn test_function_definition_1() -> Result<()> {
        let mut interner = interner();
        let s = "fn id = forall { a . # a -> a : { x . x } }";
        let mut state = State::new(s, &mut interner);

        let result = user_function_definition(&mut state);
        assert!(matches!(result, Ok(_)));
        let UserFunctionBinding {
            name,
            type_parameters,
            function,
        } = result?;

        assert_eq!(name.str(&mut interner), "id");
        assert_eq!(type_parameters.len(), 1);

        let Type::VariableUse(ref t0) = function.type_.input_types[0] else {
            unreachable!()
        };
        let Type::VariableUse(ref t) = function.type_.output_type else {
            unreachable!()
        };
        assert_eq!(t0.str(&mut interner), "a");
        assert_eq!(t.str(&mut interner), "a");

        let Term::VariableUse(x) = function.function.body else {
            unreachable!()
        };
        assert_eq!(x.str(&mut interner), "x");

        Ok(())
    }

    #[test]
    fn test_function_definition_2() -> Result<()> {
        let mut interner = interner();
        let s = "fn map = forall { a, b . # Fn(a -> b), List(a) -> List(b) : { f, xs . fold xs { Nil . Nil | Cons(x, state) . Cons(f(x), state) } } }";
        let mut state = State::new(s, &mut interner);

        let result = user_function_definition(&mut state);
        assert!(matches!(result, Ok(_)));
        let UserFunctionBinding {
            name,
            type_parameters,
            function: _,
        } = result?;

        assert_eq!(name.str(&mut interner), "map");
        assert_eq!(type_parameters.len(), 2);

        Ok(())
    }

    #[test]
    fn test_run_definition_0() -> Result<()> {
        let mut interner = interner();
        let s = "run # Nat : S(S(Zero))";
        let mut state = State::new(s, &mut interner);

        let result = run_definition(&mut state);
        assert!(matches!(result, Ok(_)));

        Ok(())
    }

    #[test]
    fn test_run_definition_1() -> Result<()> {
        let mut interner = interner();
        let s = "run # List(Nat) : Nil";
        let mut state = State::new(s, &mut interner);

        let result = run_definition(&mut state);
        assert!(matches!(result, Ok(_)));
        let RunDefinition { body: _ } = result?;

        Ok(())
    }

    #[test]
    fn test_local_let_0() -> Result<()> {
        let mut interner = interner();
        let s = "fn f = # Nat -> Nat : { x . let {, y = # Nat : add(x, One), z = # Nat : add(x, Two) . mul(y, z) } }";
        let mut state = State::new(s, &mut interner);

        let result = user_function_definition(&mut state);
        assert!(matches!(result, Ok(_)));

        Ok(())
    }

    #[test]
    fn test_lambda_0() -> Result<()> {
        let mut interner = interner();
        let s = "fn twice = forall { a, b . # Fn(a -> b), a -> b : { f, a . apply f to (apply f to (a)) }}";
        let mut state = State::new(s, &mut interner);

        let result = user_function_definition(&mut state);
        assert!(matches!(result, Ok(_)));

        Ok(())
    }

    #[test]
    fn test_program_0() -> Result<()> {
        let s = "\
        type Bool = enum {\n\
        | T\n\
        | F\n\
        }\n\
        \n\
        fn not = # Bool -> Bool : { b .\n\
            match b {\n\
            | T . F\n\
            | F . T\n\
            }\n\
        }\n\
        \n\
        run # Bool : T\n\
        ";
        let mut interner = interner();
        let mut state = State::new(s, &mut interner);

        let result = pre_program(&mut state);
        assert!(matches!(result, Ok(_)));

        Ok(())
    }

    #[test]
    fn test_program_1() -> Result<()> {
        let s = "\
        run # List(Nat) : Nil\n\
        ";
        let mut interner = interner();
        let mut state = State::new(s, &mut interner);

        let result = pre_program(&mut state);
        assert!(matches!(result, Ok(_)));

        Ok(())
    }
}
