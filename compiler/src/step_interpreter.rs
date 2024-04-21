// #[derive(Debug)]
// enum FocusedExpression {
//     Reducible(Term),
//     Seq { index: usize, terms: Vec<FocusedExpression> },
//     Done(Value),
// }

// #[derive(Debug)]
// enum Stack {
//     Empty,
//     WaitingForTupleComponents(Variant, Box<Stack>),
// }

// #[derive(Debug)]
// struct State {
//     var_env: VarEnvironment,
//     fn_env: FunctionEnvironment,
//     focused_expr: FocusedExpression,
//     stack: Stack,
// }

// impl State {
//     fn new(term: Term, fn_env: FunctionEnvironment) -> Self {
//         Self {
//             var_env: VarEnvironment::new(),
//             fn_env,
//             focused_expr: FocusedExpression::Reducible(term),
//             stack: Stack::Empty,
//         }
//     }

//     fn step(mut self) -> Self {
//         match self.focused_expr {
//             FocusedExpression::Reducible(term) => {
//                 use Term::*;
//                 match term {
//                     Const(variant) => {
//                         self.focused_expr = FocusedExpression::Done(Value::Const(variant));
//                         self
//                     },
//                     ByteArray(bytes) => {
//                         self.focused_expr = FocusedExpression::Done(Value::ByteArray(bytes));
//                         self
//                     },
//                     Tuple(variant, terms) => {
//                         self.stack = Stack::WaitingForTupleComponents(variant, Box::new(self.stack));

//                         let terms: Vec<FocusedExpression> = terms.into_iter().map(FocusedExpression::Reducible).collect();
//                         self.focused_expr = FocusedExpression::Seq { index: 0, terms };

//                         self
//                     },
//                     _ => {
//                         todo!()
//                     }
//                 }
//             },
//             FocusedExpression::Seq { index, terms } => {
//                 todo!()
//             },
//             FocusedExpression::Done(value) => {
//                 todo!()
//             },
//         }
//     }

//     fn interpret(mut self) -> Result<Value> {
//         match self.focused_expr {
//             FocusedExpression::Done(value) => match self.stack {
//                 Stack::Empty => Ok(value),
//                 _ => todo!(),
//             },
//             _ => {
//                 self = self.step();
//                 self.interpret()
//             }
//         }
//     }
// }

// fn run(program: Program, primitive_functions: Vec<PrimitiveFunction>) -> Result<Value> {
//     let fn_env = FunctionEnvironment {
//         primitive_functions,
//         functions: program.functions,
//     };
//     let state = State::new(program.main, fn_env);
//     state.interpret()
// }
