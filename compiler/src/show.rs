use crate::graph_memory_machine::{
    Function, FunctionImport, FunctionOrImport, Pattern, Program, Term,
};

pub enum PrettyString {
    String(String),
    Sequence(Vec<PrettyString>, Separator),
    Indent(Box<PrettyString>),
    Lines(Vec<PrettyString>, Separator),
    IgnoreNewLines(Box<PrettyString>),
}

pub enum Separator {
    None,
    Comma,
    Or,
}

impl Separator {
    fn str(&self) -> &str {
        match self {
            Separator::None => "",
            Separator::Comma => ", ",
            Separator::Or => " | ",
        }
    }
}

impl PrettyString {
    pub fn str(self) -> String {
        fn show(s: PrettyString, indentation: usize, ignore_new_lines: bool) -> String {
            let indent_by: usize = 2;

            use PrettyString::*;
            match s {
                String(s) => {
                    if ignore_new_lines {
                        s
                    } else {
                        let ws = " ".repeat(indentation);
                        format!("{}{}", ws, s)
                    }
                }
                Sequence(ss, sep) => ss
                    .into_iter()
                    .map(|s| show(s, indentation, ignore_new_lines))
                    .collect::<Vec<_>>()
                    .join(sep.str()),
                Indent(s) => show(*s, indentation + indent_by, ignore_new_lines),
                Lines(ss, sep) => {
                    if ignore_new_lines {
                        ss.into_iter()
                            .map(|s| show(s, indentation, ignore_new_lines))
                            .collect::<Vec<_>>()
                            .join(sep.str())
                    } else {
                        ss.into_iter()
                            .map(|s| show(s, indentation, ignore_new_lines))
                            .collect::<Vec<_>>()
                            .join(&format!("{}\n", sep.str()))
                    }
                }
                IgnoreNewLines(s) => show(*s, indentation, true),
            }
        }

        show(self, 0, false)
    }
}

fn str(s: &str) -> PrettyString {
    PrettyString::String(s.to_string())
}

fn string(s: String) -> PrettyString {
    PrettyString::String(s)
}

fn seq(ss: Vec<PrettyString>) -> PrettyString {
    PrettyString::Sequence(ss, Separator::None)
}

fn comma_seq(ss: Vec<PrettyString>) -> PrettyString {
    PrettyString::Sequence(ss, Separator::Comma)
}

fn indent(s: PrettyString) -> PrettyString {
    PrettyString::Indent(Box::new(s))
}

fn lines(ss: Vec<PrettyString>) -> PrettyString {
    PrettyString::Lines(ss, Separator::None)
}

fn or_lines(ss: Vec<PrettyString>) -> PrettyString {
    PrettyString::Lines(ss, Separator::Or)
}

fn ignore_indentation(s: PrettyString) -> PrettyString {
    PrettyString::IgnoreNewLines(Box::new(s))
}

pub fn show_program(program: &Program) -> PrettyString {
    let mut ss = vec![];
    for (i, function) in program.functions.iter().enumerate() {
        match function {
            FunctionOrImport::Fn(function) => ss.push(show_function(function, i)),
            FunctionOrImport::Import(import) => ss.push(show_import(import, i)),
        }
    }

    ss.push(show_term(&program.main, 0));
    lines(ss)
}

pub fn show_function(function: &Function, function_index: usize) -> PrettyString {
    let parameters: Vec<_> = (0..function.number_of_parameters)
        .map(|i| string(format!("{}", i)))
        .collect();
    lines(vec![
        seq(vec![
            str("fn "),
            string(format!("{}", function_index)),
            str("("),
            comma_seq(parameters),
            str(") {"),
        ]),
        indent(show_term(&function.body, function.number_of_parameters)),
        str("}"),
    ])
}

pub fn show_import(import: &FunctionImport, function_index: usize) -> PrettyString {
    let parameters: Vec<_> = (0..import.number_of_parameters)
        .map(|i| string(format!("{}", i)))
        .collect();
    seq(vec![
        str("fn "),
        string(format!("{}", function_index)),
        str("("),
        comma_seq(parameters),
        str(") { foreign_import(\""),
        string(import.external_name.clone()),
        str("\") }"),
    ])
}

pub fn show_term(term: &Term, next_parameter: usize) -> PrettyString {
    use Term::*;

    match term {
        Const(variant) => string(format!("{}", variant)),
        Float32(x) => string(format!("{}", x)),
        ByteArray(bytes) => string(format!(
            "[{}]",
            bytes
                .iter()
                .map(|byte| format!("{}", byte))
                .collect::<Vec<_>>()
                .join(", ")
        )),
        Tuple(variant, args) => {
            if args.is_empty() {
                string(format!("Cons(@{})", variant))
            } else {
                seq(vec![
                    string(format!("Cons(@{}; ", variant)),
                    ignore_indentation(comma_seq(
                        args.iter()
                            .map(|arg| show_term(arg, next_parameter))
                            .collect(),
                    )),
                    str(")"),
                ])
            }
        }
        ProjectComponent(term, index) => seq(vec![
            str("Pi("),
            ignore_indentation(show_term(term, next_parameter)),
            string(format!(", {})", index)),
        ]),
        Call(function_name, args) => seq(vec![
            string(format!("call {}(", function_name)),
            ignore_indentation(comma_seq(
                args.iter()
                    .map(|arg| show_term(arg, next_parameter))
                    .collect(),
            )),
            str(")"),
        ]),
        PartialApply(function_name, args) => seq(vec![
            string(format!("papply {}(", function_name)),
            ignore_indentation(comma_seq(
                args.iter()
                    .map(|arg| show_term(arg, next_parameter))
                    .collect(),
            )),
            str(")"),
        ]),
        CallClosure(term, args) => seq(vec![
            str("call-closure "),
            ignore_indentation(show_term(term, next_parameter)),
            str("("),
            ignore_indentation(comma_seq(
                args.iter()
                    .map(|arg| show_term(arg, next_parameter))
                    .collect(),
            )),
            str(")"),
        ]),
        VarUse(var) => string(format!("${}", var)),
        Let(args, body) => {
            let mut strs = vec![str("{")];
            let mut count = next_parameter;
            for arg in args {
                strs.push(seq(vec![
                    string(format!("let {} = ", count)),
                    ignore_indentation(show_term(arg, count)),
                    str("; "),
                ]));
                count += 1;
            }
            strs.push(show_term(body, count));
            strs.push(str("}"));
            lines(strs)
        }
        Match(arg, branches) => lines(vec![
            seq(vec![
                str("match "),
                show_term(arg, next_parameter),
                str(" {"),
            ]),
            or_lines({
                let mut branches_s = vec![];
                for (pattern, body) in branches {
                    let pattern_s = match pattern {
                        Pattern::Variant(variant) => string(format!("@{}", variant)),
                        Pattern::Always => str("_"),
                    };
                    branches_s.push(ignore_indentation(seq(vec![
                        pattern_s,
                        str(" . "),
                        show_term(body, next_parameter + 1),
                    ])))
                }
                branches_s
            }),
            str("}"),
        ]),
        Seq(terms) => {
            todo!()
        }
        CommandAndThen(cmd_term, continuation_term) => seq(vec![
            str("do "),
            ignore_indentation(show_term(cmd_term, next_parameter)),
            str(" then {"),
            ignore_indentation(show_term(&continuation_term.body, next_parameter + 1)),
            str("}"),
        ]),
        Pure(term) => seq(vec![
            str("pure("),
            ignore_indentation(show_term(term, next_parameter)),
            str(")"),
        ]),
        Receive => str("receive"),
    }
}
