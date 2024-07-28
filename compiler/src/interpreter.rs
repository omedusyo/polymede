use crate::graph_memory_machine::{Term, Pattern, Variant, Program, Function, FunctionImport, FunctionOrImport, FunctionName, VarName};

type Result<A> = std::result::Result<A, RuntimeError>;

#[derive(Debug)]
enum RuntimeError {
    UndefinedVariable(VarName),
    UndefinedFunction(FunctionName),
    WrongNumberOfArguments { expected: usize, received: usize },
    ExpectedConstant,
    ExpectedByteArray,
    ExpectedTuple,
    ExpectedClosure,
    TupleIndexOutOfBounds,
    AttemptToPopEmptyVarEnv,
    EmptySequenceOfTerms,
    AttemptToPatternMatchOnByteArray,
    AttemptToPatternMatchOnClosure,
    NoMatchesFound,
    UnsupportedPrimitiveFunction { import: FunctionImport }
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Const(Variant),
    ByteArray(Vec<u8>),
    Tuple(Variant, Vec<Value>),
    Closure(Closure),
}

#[derive(Debug, Clone, PartialEq)]
struct Closure {
    fn_name: FunctionName,
    partial_arguments: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq)]
struct VarEnvironment(Vec<Value>);

type PrimitiveFunction = fn (Vec<Value>) -> Result<Value>;

#[derive(Debug)]
struct FunctionEnvironment {
    functions: Vec<FunctionOrPrimitive>,
}

#[derive(Debug)]
enum FunctionOrPrimitive {
    Function(Function),
    Primitive(PrimitiveFunction),
}

impl Value {
    fn matches(&self, pattern: &Pattern) -> Result<bool> {
        use Pattern::*;
        use Value::*;
        match (self, pattern) {
            (ByteArray(_), _) => Err(RuntimeError::AttemptToPatternMatchOnByteArray),
            (Closure(_), _) => Err(RuntimeError::AttemptToPatternMatchOnClosure),
            (Const(variant0), Variant(variant1)) => Ok(variant0 == variant1),
            (Tuple(variant0, _), Variant(variant1)) => Ok(variant0 == variant1),
            (_, Always) => Ok(true),
        }
    }
}

impl VarEnvironment {
    fn new() -> Self {
        Self(vec![])
    }

    fn lookup(&self, var_name: VarName) -> Result<Value> {
        match self.0.get(var_name) {
            Some(val) => Ok(val.clone()),
            None => Err(RuntimeError::UndefinedVariable(var_name)),
        }
    }

    fn push_binding(&mut self, value: Value) {
        self.0.push(value)
    }

    fn pop_binding(&mut self) -> Result<Value> {
        match self.0.pop() {
            Some(val) => Ok(val),
            None => Err(RuntimeError::AttemptToPopEmptyVarEnv),
        }
    }
}

impl FunctionEnvironment {
    fn apply(&self, fn_name: FunctionName, args: Vec<Value>) -> Result<Value> {
        let Some(function) = self.functions.get(fn_name) else { return Err(RuntimeError::UndefinedFunction(fn_name)) };
        let function = &self.functions[fn_name];
        match function {
            FunctionOrPrimitive::Function(function) => {
                if function.number_of_parameters == args.len() {
                    let mut env = VarEnvironment(args);
                    self.interpret(&function.body, &mut env)
                } else {
                    Err(RuntimeError::WrongNumberOfArguments { expected: function.number_of_parameters, received: args.len() })
                }
            },
            FunctionOrPrimitive::Primitive(function) => {
                function(args)
            },
        }
    }

    fn interpret_terms(&self, terms: &[Term], var_env: &mut VarEnvironment) -> Result<Vec<Value>> {
        let mut values: Vec<Value> = Vec::with_capacity(terms.len());
        for term in terms {
            let val = self.interpret(term, var_env)?;
            values.push(val);
        }
        Ok(values)
    }

    fn interpret(&self, term: &Term, var_env: &mut VarEnvironment) -> Result<Value> {
        use Term::*;
        match term {
            Const(variant) => Ok(Value::Const(*variant)),
            ByteArray(bytes) => Ok(Value::ByteArray(bytes.to_vec())),
            Tuple(variant, terms) => Ok(Value::Tuple(*variant, self.interpret_terms(terms, var_env)?)),
            ProjectComponent(term, i) => {
                let val = self.interpret(term, var_env)?;
                match val {
                    Value::Tuple(_, components) => match components.get(*i as usize) {
                        Some(val) => Ok(val.clone()),
                        None => Err(RuntimeError::TupleIndexOutOfBounds),
                    },
                    _ => Err(RuntimeError::ExpectedTuple)
                }
            },
            Call(fn_name, terms) => {
                let args = self.interpret_terms(terms, var_env)?;
                self.apply(*fn_name, args)
            },
            PartialApply(fn_name, terms) => {
                let partial_arguments = self.interpret_terms(terms, var_env)?;
                Ok(Value::Closure(Closure { fn_name: *fn_name, partial_arguments }))
            },
            CallClosure(closure_term, arg_terms) => {
                match self.interpret(closure_term, var_env)? {
                    Value::Closure(closure) => {
                        let mut args = vec![Value::Closure(closure.clone())]; // For recursion
                        for captured_val in closure.partial_arguments {
                            args.push(captured_val)
                        }
                        for arg_val in self.interpret_terms(arg_terms, var_env)? {
                            args.push(arg_val)
                        }
                        self.apply(closure.fn_name, args)
                    },
                    _ => Err(RuntimeError::ExpectedClosure),
                }
            },
            VarUse(var) => var_env.lookup(*var),
            Let(terms, body_term) => {
                for term in terms {
                    let val = self.interpret(term, var_env)?;
                    var_env.push_binding(val);
                }
                let body_val = self.interpret(body_term, var_env)?;
                var_env.pop_binding()?;
                Ok(body_val)
            },
            Match(term, branches) => {
                let val = self.interpret(term, var_env)?;
                for (pattern, body) in branches {
                    if val.matches(pattern)? { 
                        return self.interpret(body, var_env)
                    }
                }
                Err(RuntimeError::NoMatchesFound)
            },
            Seq(terms) => {
                if terms.len() == 0 {
                    return Err(RuntimeError::EmptySequenceOfTerms)
                }
                
                let mut val = self.interpret(&terms[0], var_env)?;
                for term in &terms[1..] {
                    val = self.interpret(term, var_env)?;
                }
                Ok(val)
            },
            CommandAndThen(_cmd_term, _continuation_term) => todo!(),
            Pure(_term) => todo!(),
        }
    }
}

fn run(program: Program) -> Result<Value> {
    let mut functions = vec![];
    for function in program.functions {
        match function {
            FunctionOrImport::Fn(function) => {
                functions.push(FunctionOrPrimitive::Function(function))
            },
            FunctionOrImport::Import(import) => {
                functions.push(FunctionOrPrimitive::Primitive(
                    if import.external_name == "i32_inc" {
                        increment
                    } else if import.external_name == "i32_dec" {
                        decrement
                    } else if import.external_name == "i32_add" {
                        addition
                    } else {
                        return Err(RuntimeError::UnsupportedPrimitiveFunction { import })
                    }
                ))
            }
        }
    }
    let fn_env = FunctionEnvironment {
        functions,
    };
    let mut env = VarEnvironment::new();

    fn_env.interpret(&program.main, &mut env)
}

// ===Primitive Functions===
fn increment(args: Vec<Value>) -> Result<Value> {
    if args.len() == 1 {
        match args[0] {
            Value::Const(x) => Ok(Value::Const(x + 1)),
            _ => Err(RuntimeError::ExpectedConstant),
        }
    } else {
        Err(RuntimeError::WrongNumberOfArguments { expected: 1, received: args.len() })
    }
}

fn decrement(args: Vec<Value>) -> Result<Value> {
    if args.len() == 1 {
        match args[0] {
            Value::Const(x) => Ok(Value::Const(x - 1)),
            _ => Err(RuntimeError::ExpectedConstant),
        }
    } else {
        Err(RuntimeError::WrongNumberOfArguments { expected: 1, received: args.len() })
    }
}

fn addition(args: Vec<Value>) -> Result<Value> {
    if args.len() == 2 {
        match args[0] {
            Value::Const(x) => match args[1] {
                Value::Const(y) => Ok(Value::Const(x + y)),
                _ => Err(RuntimeError::ExpectedConstant),
            },
            _ => Err(RuntimeError::ExpectedConstant),
        }
    } else {
        Err(RuntimeError::WrongNumberOfArguments { expected: 2, received: args.len() })
    }
}

mod tests {
    use super::*;
    use crate::graph_memory_machine::{FunctionImport, constant, tuple, project, var, call, call_closure, partial_apply, pattern_match};

    #[test]
    fn test_constant() -> Result<()> {
        // functions
        let inc = 0;
        let program = Program {
            functions: vec![
                FunctionOrImport::Import(FunctionImport {
                    number_of_parameters: 1,
                    external_name: "i32_inc".to_string(),
                }),
            ],
            // main: call(inc, vec![constant(666)]),
            main: call(inc, vec![call(inc, vec![constant(666)])]),
        };

        let val = run(program)?;

        assert!(val == Value::Const(668));
        Ok(())
    }

    #[test]
    fn test_singleton() -> Result<()> {
        // functions
        let singleton = 0;

        // constructors
        let nil = 0;
        let cons = 1;

        let program = Program {
            functions: vec![
                // fn singleton(x) {
                //   Tuple(Cons, [x, Const(Nil)])
                // }
                FunctionOrImport::Fn(Function {
                    number_of_parameters: 1,
                    body: {
                        let x = 0;
                        tuple(cons, vec![var(x), constant(nil)])
                    }
                }),
            ],
            main: call(singleton, vec![constant(666)]),
        };

        let val = run(program)?;

        use Value::*;
        assert!(val == Tuple(cons, vec![Const(666), Const(nil)]));
        Ok(())
    }

    #[test]
    fn test_inc_singleton() -> Result<()> {
        // functions
        let inc = 0;
        let singleton = 1;

        // constructors
        let nil = 0;
        let cons = 1;

        let program = Program {
            // Assume the first primitive function is `inc`.
            functions: vec![
                FunctionOrImport::Import(FunctionImport {
                    number_of_parameters: 1,
                    external_name: "i32_inc".to_string(),
                }),
                // fn singleton(x) {
                //   Tuple(Cons, [call inc(x), Const(Nil)])
                // }
                FunctionOrImport::Fn(Function {
                    number_of_parameters: 1,
                    body: {
                        let x = 0;
                        tuple(cons, vec![call(inc, vec![var(x)]), constant(nil)])
                    }
                }),
            ],
            main: call(singleton, vec![constant(666)]),
        };

        let val = run(program)?;

        use Value::*;
        assert!(val == Tuple(cons, vec![Const(667), Const(nil)]));
        Ok(())
    }

    #[test]
    fn test_pattern_matching() -> Result<()> {
        // functions
        let is_zero = 0;

        // constructors
        let t = 1;
        let f = 0;
        let program = Program {
            functions: vec![
                // fn is_zero(n) {
                //   match n {
                //   | 0 -> t
                //   | _ -> f
                //   }
                // }
                FunctionOrImport::Fn(Function {
                    number_of_parameters: 1,
                    body: {
                        let n = 0;
                        pattern_match(
                            var(n),
                            vec![
                                (Pattern::Variant(0), constant(t)),
                                (Pattern::Always, constant(f)),
                            ]
                        )
                    }
                }),
            ],
            main: call(is_zero, vec![constant(0)])
        };

        let val = run(program)?;
        assert!(val == Value::Const(t));
        
        Ok(())
    }

    #[test]
    fn test_rec() -> Result<()> {
        // functions
        let add = 0;
        let dec = 1;
        let sum = 2;

        // constructors
        let program = Program {
            // Assume the first primitive function is `inc`.
            functions: vec![
                FunctionOrImport::Import(FunctionImport {
                    number_of_parameters: 2,
                    external_name: "i32_add".to_string(),
                }),
                FunctionOrImport::Import(FunctionImport {
                    number_of_parameters: 1,
                    external_name: "i32_dec".to_string(),
                }),
                // fn sum(n) {
                //   match n {
                //   | 0 -> 0
                //   | _ -> n + sum(dec(n))
                //   }
                // }
                FunctionOrImport::Fn(Function {
                    number_of_parameters: 1,
                    body: {
                        let n = 0;
                        pattern_match(
                            var(n),
                            vec![
                                (Pattern::Variant(0), constant(0)),
                                (Pattern::Always, call(add, vec![var(n), call(sum, vec![call(dec, vec![var(n)])])])),
                            ]
                        )
                    }
                }),
            ],
            main: call(sum, vec![constant(5)])
        };

        let val = run(program)?;
        assert!(val == Value::Const(15));
        
        Ok(())
    }

    #[test]
    fn test_range() -> Result<()> {
        // functions
        let dec = 0;
        let range_loop = 1;
        let range = 2;

        // constructors
        let nil = 0;
        let cons = 1;

        let program = Program {
            functions: vec![
                FunctionOrImport::Import(FunctionImport {
                    number_of_parameters: 1,
                    external_name: "i32_dec".to_string(),
                }),
                // fn rangeLoop(n, xs) {
                //  match n {
                //  | Const(nil) -> xs
                //  | _ ->
                //      let m = call dec(n)
                //      call rangeLoop(m, cons(m, xs)))
                //  }
                // }
                FunctionOrImport::Fn(Function {
                    number_of_parameters: 2,
                    body: {
                        let n = 0;
                        let xs = 1;
                        pattern_match(var(n), vec![
                            (Pattern::Variant(nil), var(xs)),
                            (Pattern::Always,
                                // let_bind(
                                //     call(dec, vec![var(n)]), {
                                //         let m = 2;
                                //         call(range_loop, vec![var(m), tuple(cons, vec![var(m), var(xs)])])
                                //     }
                                // )
                                call(range_loop, vec![call(dec, vec![var(n)]), tuple(cons, vec![call(dec, vec![var(n)]), var(xs)])])
                            )
                        ])
                    },
                }),
                // fn range(n) {
                //   call rangeLoop(n, Const(Nil))
                // }
                FunctionOrImport::Fn(Function {
                    number_of_parameters: 1,
                    body: {
                        let n = 0;
                        call(range_loop, vec![var(n), constant(nil)])
                    },
                }),
            ],
            main: call(range, vec![constant(3)]),
        };

        let val = run(program)?;

        use Value::*;
        fn construct(x: Value, xs: Value) -> Value {
            Tuple(1, vec![x, xs])
        }

        // println!("VALUE: {:?}", val);
        // assert!(val == Tuple(cons, vec![Const(0), Tuple(cons, vec![Const(1), Const(nil)])]));
        assert!(val == construct(Const(0), construct(Const(1), construct(Const(2), Const(nil)))));
        Ok(())
    }

    #[test]
    fn test_closure() -> Result<()> {
        // functions
        let add = 0;
        let another_add = 1;

        let program = Program {
            functions: vec![
                FunctionOrImport::Import(FunctionImport {
                    number_of_parameters: 1,
                    external_name: "i32_add".to_string(),
                }),
                // fn another_add(self, x, y) {
                //     x + y
                // }
                FunctionOrImport::Fn(Function {
                    number_of_parameters: 3,
                    body: {
                        // let self_var = 0;
                        let x = 1;
                        let y = 2;
                        call(add, vec![var(x), var(y)])
                    }
                }),
            ],
            main: call_closure(partial_apply(another_add, vec![constant(5)]), vec![constant(6)]),
        };

        let val = run(program)?;

        assert!(val == Value::Const(11));
        Ok(())
    }
}
