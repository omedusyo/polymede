mod base;
mod combinator;
mod identifier;
mod pattern;
mod program;
mod special;
mod term;
mod types;
mod lex;

#[cfg(test)]
mod tests {
    use crate::parser::{
        base::{State, Result, Error, Term, Type, TypeDeclaration, FunctionDeclaration, LetDeclaration, EnumDeclaration, IndDeclaration},
        types::type_,
        program::{program, let_declaration, function_declaration, type_declaration},
    };

    #[test]
    fn test_type_0() -> Result<()> {
        let s = "Result(  Error,   x )";
        let mut state = State::new(s);

        let result = type_(&mut state);

        assert!(matches!(result, Ok(Type::TypeApplication(_, _))));
        let Ok(Type::TypeApplication(identifier, type_args)) = result else { unreachable!() };
        assert_eq!(type_args.len(), 2);

        assert_eq!(identifier.str(), "Result");
        assert!(matches!(type_args[0], Type::TypeApplication(_, _)));
        assert!(matches!(type_args[1], Type::VariableUse(_)));

        let Type::TypeApplication(ref err_identifier, ref err_type_args) = type_args[0] else { unreachable!() };
        assert_eq!(err_type_args.len(), 0);
        assert_eq!(err_identifier.str(), "Error");

        let Type::VariableUse(ref var_name) = type_args[1] else { unreachable!() };
        assert_eq!(var_name.str(), "x");

        Ok(())
    }

    #[test]
    fn test_type_1() -> Result<()> {
        let s = "Fn(A1, A2 -> A3)";
        let mut state = State::new(s);

        let result = type_(&mut state);

        assert!(matches!(result, Ok(Type::Arrow(_))));
        let Ok(Type::Arrow(fn_type)) = result else { unreachable!() };

        assert_eq!(fn_type.input_types.len(), 2);

        Ok(())
    }

    #[test]
    fn test_type_declaration_0() -> Result<()> {
        let s = "type Bool = enum { T | F }";
        let mut state = State::new(s);

        let result = type_declaration(&mut state);

        assert!(matches!(result, Ok(TypeDeclaration::Enum(_))));
        let Ok(TypeDeclaration::Enum(EnumDeclaration { name, type_parameters: _, constructors })) = result else { unreachable!() };
        assert_eq!(name.str(), "Bool");

        assert_eq!(constructors.len(), 2);
        assert_eq!(constructors[0].name.str(), "T");
        assert_eq!(constructors[1].name.str(), "F");

        Ok(())
    }

    #[test]
    fn test_type_declaration_1() -> Result<()> {
        let s = "type SomeType = enum { | Const | Unary(a) | Binary(Foo(x, Bar), y) | Ternary(a, b, c) }";
        let mut state = State::new(s);

        let result = type_declaration(&mut state);

        assert!(matches!(result, Ok(TypeDeclaration::Enum(_))));

        let Ok(TypeDeclaration::Enum(EnumDeclaration { name, type_parameters: _, constructors })) = result else { unreachable!() };
        assert_eq!(name.str(), "SomeType");

        assert_eq!(constructors.len(), 4);
        assert_eq!(constructors[0].name.str(), "Const");
        assert_eq!(constructors[1].name.str(), "Unary");
        assert_eq!(constructors[2].name.str(), "Binary");
        assert_eq!(constructors[3].name.str(), "Ternary");

        assert_eq!(constructors[0].parameters.len(), 0);
        assert_eq!(constructors[1].parameters.len(), 1);
        assert_eq!(constructors[2].parameters.len(), 2);
        assert_eq!(constructors[3].parameters.len(), 3);

        Ok(())
    }

    #[test]
    fn test_type_declaration_2() -> Result<()> {
        let s = "type Nat = ind { nat . Zero | Succ(nat) }";
        let mut state = State::new(s);

        let result = type_declaration(&mut state);

        assert!(matches!(result, Ok(TypeDeclaration::Ind(_))));
        let Ok(TypeDeclaration::Ind(IndDeclaration { name, type_parameters: _, recursive_type_var, constructors })) = result else { unreachable!() };
        assert_eq!(name.str(), "Nat");
        assert_eq!(recursive_type_var.str(), "nat");

        assert_eq!(constructors.len(), 2);
        assert_eq!(constructors[0].name.str(), "Zero");
        assert_eq!(constructors[1].name.str(), "Succ");

        assert_eq!(constructors[0].parameters.len(), 0);
        assert_eq!(constructors[1].parameters.len(), 1);

        Ok(())
    }

    #[test]
    fn test_type_declaration_3() -> Result<()> {
        let s = "type List(a, ignored) = ind { list . Nil | Cons(a, list) }";
        let mut state = State::new(s);

        let result = type_declaration(&mut state);

        assert!(matches!(result, Ok(TypeDeclaration::Ind(_))));
        let Ok(TypeDeclaration::Ind(IndDeclaration { name, type_parameters, recursive_type_var, constructors })) = result else { unreachable!() };
        assert_eq!(name.str(), "List");

        assert_eq!(type_parameters.len(), 2);
        assert_eq!(type_parameters[0].str(), "a");
        assert_eq!(type_parameters[1].str(), "ignored");

        assert_eq!(recursive_type_var.str(), "list");

        assert_eq!(constructors.len(), 2);
        assert_eq!(constructors[0].name.str(), "Nil");
        assert_eq!(constructors[1].name.str(), "Cons");

        assert_eq!(constructors[0].parameters.len(), 0);
        assert_eq!(constructors[1].parameters.len(), 2);

        Ok(())
    }

    #[test]
    fn test_type_declaration_4() -> Result<()> {
        let s = "type Foo(x, y, z, y, z) = enum { A | B }";
        let mut state = State::new(s);

        let result = type_declaration(&mut state);

        assert!(matches!(result, Err(Error::DuplicateVariableNames {..})));
        let Err(Error::DuplicateVariableNames { duplicates }) = result else { unreachable!() };
        assert_eq!(duplicates.len(), 2);
        assert_eq!(duplicates[0].str(), "y");
        assert_eq!(duplicates[1].str(), "z");

        Ok(())
    }

    #[test]
    fn test_function_declaration_0() -> Result<()> {
        let s = "fn square = # Nat -> Nat : { x . mul(x, x) }";
        let mut state = State::new(s);

        let result = function_declaration(&mut state);
        assert!(matches!(result, Ok(_)));
        let FunctionDeclaration { name, type_parameters, function } = result?;

        assert_eq!(name.str(), "square");
        assert_eq!(type_parameters.len(), 0);

        let Type::TypeApplication(ref t0, _) = function.type_.input_types[0] else { unreachable!() };
        let Type::TypeApplication(ref t, _) = function.type_.output_type else { unreachable!() };
        assert_eq!(t0.str(), "Nat");
        assert_eq!(t.str(), "Nat");

        let Term::FunctionApplication(mul, args) = function.body else { unreachable!() };
        let Term::VariableUse(ref arg0) = args[0] else { unreachable!() };
        let Term::VariableUse(ref arg1) = args[1] else { unreachable!() };


        assert_eq!(mul.str(), "mul");
        assert_eq!(arg0.str(), "x");
        assert_eq!(arg1.str(), "x");

        Ok(())
    }

    #[test]
    fn test_function_declaration_1() -> Result<()> {
        let s = "fn id = forall { a . # a -> a : { x . x } }";
        let mut state = State::new(s);

        let result = function_declaration(&mut state);
        assert!(matches!(result, Ok(_)));
        let FunctionDeclaration { name, type_parameters, function } = result?;

        assert_eq!(name.str(), "id");
        assert_eq!(type_parameters.len(), 1);

        let Type::VariableUse(ref t0) = function.type_.input_types[0] else { unreachable!() };
        let Type::VariableUse(ref t) = function.type_.output_type else { unreachable!() };
        assert_eq!(t0.str(), "a");
        assert_eq!(t.str(), "a");

        let Term::VariableUse(x) = function.body else { unreachable!() };
        assert_eq!(x.str(), "x");

        Ok(())
    }

    #[test]
    fn test_function_declaration_2() -> Result<()> {
        let s = "fn map = forall { a, b . # Fn(a -> b), List(a) -> List(b) : { f, xs . fold xs { Nil . Nil | Cons(x, state) . Cons(f(x), state) } } }";
        let mut state = State::new(s);

        let result = function_declaration(&mut state);
        assert!(matches!(result, Ok(_)));
        let FunctionDeclaration { name, type_parameters, function: _ } = result?;

        assert_eq!(name.str(), "map");
        assert_eq!(type_parameters.len(), 2);

        Ok(())
    }

    #[test]
    fn test_let_declaration_0() -> Result<()> {
        let s = "let two = # Nat : S(S(Zero))";
        let mut state = State::new(s);

        let result = let_declaration(&mut state);
        assert!(matches!(result, Ok(_)));
        let LetDeclaration { name, type_parameters, body: _ } = result?;

        assert_eq!(name.str(), "two");
        assert_eq!(type_parameters.len(), 0);


        Ok(())
    }

    #[test]
    fn test_let_declaration_1() -> Result<()> {
        let s = "let nil = forall { a . # List : Nil }";
        let mut state = State::new(s);

        let result = let_declaration(&mut state);
        assert!(matches!(result, Ok(_)));
        let LetDeclaration { name, type_parameters, body: _ } = result?;

        assert_eq!(name.str(), "nil");
        assert_eq!(type_parameters.len(), 1);

        Ok(())
    }

    #[test]
    fn test_local_let_0() -> Result<()> {
        let s = "fn f = # Nat -> Nat : { x . let {, y = add(x, One), z = add(x, Two) . mul(y, z) } }";
        let mut state = State::new(s);

        let result = function_declaration(&mut state);
        assert!(matches!(result, Ok(_)));

        Ok(())
    }

    #[test]
    fn test_lambda_0() -> Result<()> {
        let s = "fn twice = forall { a, b . # Fn(a -> b), a -> b : { f, a . apply f to (apply f to (a)) }}";
        let mut state = State::new(s);

        let result = function_declaration(&mut state);
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
        ";

        let mut state = State::new(s);

        let result = program(&mut state);
        assert!(matches!(result, Ok(_)));

        Ok(())
    }
}
