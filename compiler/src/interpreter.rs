use crate::graph_memory_machine::{Term, Pattern, Variant, Program, Function, FunctionName, VarName};


#[derive(Debug, Clone, Eq, PartialEq)]
enum Value {
    Const(Variant),
    ByteArray(Vec<u8>),
    Tuple(Variant, Vec<Value>),
}

impl Value {
    fn matches(&self, pattern: &Pattern) -> Result<bool> {
        use Pattern::*;
        use Value::*;
        match (self, pattern) {
            (ByteArray(_), _) => Err(RuntimeError::AttemptToPatternMatchOnByteArray),
            (Const(variant0), Variant(variant1)) => Ok(variant0 == variant1),
            (Tuple(variant0, _), Variant(variant1)) => Ok(variant0 == variant1),
            (_, Always) => Ok(true),
        }
    }
}

#[derive(Debug)]
struct VarEnvironment(Vec<Value>);

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

type PrimitiveFunction = fn (Vec<Value>) -> Result<Value>;

#[derive(Debug)]
struct FunctionEnvironment {
    primitive_functions: Vec<PrimitiveFunction>,
    functions: Vec<Function>,
}

impl FunctionEnvironment {
    fn apply(&self, fn_name: FunctionName, args: Vec<Value>) -> Result<Value> {
        if fn_name < self.primitive_functions.len() {
            self.primitive_functions[fn_name](args)
        } else if fn_name - self.primitive_functions.len() < self.functions.len() {
            let function = &self.functions[fn_name - self.primitive_functions.len()];
            if function.number_of_parameters == args.len() {
                let mut env = VarEnvironment(args);
                self.interpret(&function.body, &mut env)
            } else {
                Err(RuntimeError::WrongNumberOfArguments { expected: function.number_of_parameters, received: args.len() })
            }
        } else {
            Err(RuntimeError::UndefinedFunction(fn_name))
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
        }
    }
}

#[derive(Debug)]
enum RuntimeError {
    UndefinedVariable(VarName),
    UndefinedFunction(FunctionName),
    WrongNumberOfArguments { expected: usize, received: usize },
    ExpectedConstant,
    ExpectedByteArray,
    ExpectedTuple,
    TupleIndexOutOfBounds,
    AttemptToPopEmptyVarEnv,
    EmptySequenceOfTerms,
    AttemptToPatternMatchOnByteArray,
    NoMatchesFound,
}

type Result<A> = std::result::Result<A, RuntimeError>;


fn run(program: Program, primitive_functions: Vec<PrimitiveFunction>) -> Result<Value> {
    let fn_env = FunctionEnvironment {
        primitive_functions,
        functions: program.functions,
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
    use crate::graph_memory_machine::{constant, tuple, project, var, call, pattern_match};

    #[test]
    fn test_constant() -> Result<()> {
        // functions
        let inc = 0;
        let program = Program {
            // Assume the first primitive function is `inc`.
            number_of_primitive_functions: 2,
            functions: vec![],
            // main: call(inc, vec![constant(666)]),
            main: call(inc, vec![call(inc, vec![constant(666)])]),
        };

        let primitive_functions: Vec<PrimitiveFunction> = vec![ increment ];

        let val = run(program, primitive_functions)?;

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
            // Assume the first primitive function is `inc`.
            number_of_primitive_functions: 0,
            functions: vec![
                // fn singleton(x) {
                //   Tuple(Cons, [x, Const(Nil)])
                // }
                Function {
                    number_of_parameters: 1,
                    body: {
                        let x = 0;
                        tuple(cons, vec![var(x), constant(nil)])
                    }
                }
            ],
            main: call(singleton, vec![constant(666)]),
        };

        let primitive_functions: Vec<PrimitiveFunction> = vec![];

        let val = run(program, primitive_functions)?;

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
            number_of_primitive_functions: 1,
            functions: vec![
                // fn singleton(x) {
                //   Tuple(Cons, [call inc(x), Const(Nil)])
                // }
                Function {
                    number_of_parameters: 1,
                    body: {
                        let x = 0;
                        tuple(cons, vec![call(inc, vec![var(x)]), constant(nil)])
                    }
                }
            ],
            main: call(singleton, vec![constant(666)]),
        };

        let primitive_functions: Vec<PrimitiveFunction> = vec![increment];

        let val = run(program, primitive_functions)?;

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
            // Assume the first primitive function is `inc`.
            number_of_primitive_functions: 2,
            functions: vec![
                // fn is_zero(n) {
                //   match n {
                //   | 0 -> t
                //   | _ -> f
                //   }
                // }
                Function {
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
                }
            ],
            main: call(is_zero, vec![constant(0)])
        };

        let primitive_functions: Vec<PrimitiveFunction> = vec![];

        let val = run(program, primitive_functions)?;
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
            number_of_primitive_functions: 2,
            functions: vec![
                // fn sum(n) {
                //   match n {
                //   | 0 -> 0
                //   | _ -> n + sum(dec(n))
                //   }
                // }
                Function {
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
                }
            ],
            main: call(sum, vec![constant(5)])
        };

        let primitive_functions: Vec<PrimitiveFunction> = vec![addition, decrement];

        let val = run(program, primitive_functions)?;
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
            // Assume the first primitive function is `dec`.
            number_of_primitive_functions: 1,
            functions: vec![
                // fn rangeLoop(n, xs) {
                //  match n {
                //  | Const(nil) -> xs
                //  | _ ->
                //      let m = call dec(n)
                //      call rangeLoop(m, cons(m, xs)))
                //  }
                // }
                Function {
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
                },
                // fn range(n) {
                //   call rangeLoop(n, Const(Nil))
                // }
                Function {
                    number_of_parameters: 1,
                    body: {
                        let n = 0;
                        call(range_loop, vec![var(n), constant(nil)])
                    },
                }
            ],
            main: call(range, vec![constant(3)]),
        };

        let primitive_functions: Vec<PrimitiveFunction> = vec![ decrement ];

        let val = run(program, primitive_functions)?;

        use Value::*;
        fn construct(x: Value, xs: Value) -> Value {
            Tuple(1, vec![x, xs])
        }

        // println!("VALUE: {:?}", val);
        // assert!(val == Tuple(cons, vec![Const(0), Tuple(cons, vec![Const(1), Const(nil)])]));
        assert!(val == construct(Const(0), construct(Const(1), construct(Const(2), Const(nil)))));
        Ok(())
    }
}
