use crate::binary_format::sections;
use crate::binary_format::sections::{TypeSection, FunctionSection, ExportSection, ImportSection, CodeSection};
use crate::base::{
    indices::{TypeIndex, LocalIndex, GlobalIndex, LabelIndex, FunctionIndex},
    types::{FunctionType, ValueType, BlockType, NumType},
    export::{Export, ExportDescription},
    import::{Import, ImportDescription},
};

pub struct Module {
    function_types: Vec<FunctionType>,
    // TODO: Bundling Function Imports and Functions into one place is convinient, but not exactly
    // performance friendly.
    functions: Vec<FunctionOrImport>,
    exports: Vec<Export>
}

enum FunctionOrImport {
    Fn(Function),
    Import(FunctionImport),
}

impl FunctionOrImport {
    fn type_index(&self) -> TypeIndex {
        match self {
            Self::Import(FunctionImport { type_index, .. }) => *type_index,
            Self::Fn(Function { type_index, .. }) => *type_index,
        }
    }
}

struct Function {
    type_index: TypeIndex,
    locals: Vec<ValueType>,
    body: Expression,
}

struct TypedFunction {
    type_: FunctionType,
    locals: Vec<ValueType>,
    body: Expression,
}

struct FunctionImport {
    module_name: String,
    name: String,
    type_index: TypeIndex,
}

struct TypedFunctionImport {
    module_name: String,
    name: String,
    type_: FunctionType,
}


enum Expression {
    Unreachable,
    Nop,

    // ===Control Instructions===
    Block { type_: BlockType, body: Box<Expression> },
    Loop { type_: BlockType, body: Box<Expression> },
    IfThen { type_: BlockType, then_body: Box<Expression> },
    IfThenElse { type_: BlockType, then_body: Box<Expression>, else_body: Box<Expression> },
    Br(LabelIndex),
    Call(FunctionIndex, Vec<Expression>),
    Return(Box<Expression>),

    // ===Variable Instructions====
    LocalGet(LocalIndex),
    LocalSet(LocalIndex, Box<Expression>),
    LocalTee(LocalIndex),
    GlobalGet(GlobalIndex),
    GlobalSet(GlobalIndex, Box<Expression>),

    // ===Numeric Instructions===
    Op0(Op0),
    Op1(Op1, Box<Expression>),
    Op2(Op2, Box<Expression>, Box<Expression>),
    Rel1(Rel1, Box<Expression>),
    Rel2(Rel2, Box<Expression>, Box<Expression>),
}

enum Op0 {
    Const(NumberLiteral),
}

enum NumberLiteral {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

enum Op1 {
    Int(Size, IntegerOp1),
    Float(Size, FloatOp1)
}

enum Op2 {
    Int(Size, IntegerOp2),
    Float(Size, FloatOp2),
}

enum IntegerOp1 {
    Clz,
    Ctz,
    Popcnt,
}

enum Size {
    X32,
    X64,
}

enum IntegerOp2 {
    Add,
    Sub,
    Mul,
    Div(Signedness),
    Rem(Signedness),
    And,
    Or,
    Xor,
    Shl,
    Shr(Signedness),
    Rotl,
    Rotr
}

enum FloatOp1 {
    Abs,
    Neg,
    Sqrt,
    Ceil,
    Floor,
    Trunc,
    Nearest,
}

enum FloatOp2 {
    Add,
    Sub,
    Mul,
    Div,
    Min,
    Max,
    Copysign,
}

enum Signedness {
    Signed,
    Unsigned,
}

enum Rel1 {
    Int(Size, IntRel1),
}

enum Rel2 {
    Int(Size, IntRel2),
    Float(Size, FloatRel2),
}

enum IntRel1 {
    Eqz,
}

enum IntRel2 {
    Eq,
    Ne,
    Lt(Signedness),
    Gt(Signedness),
    Le(Signedness),
    Ge(Signedness),
}

enum FloatRel2 {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}


impl Module {
    fn empty() -> Self {
        Self { function_types: vec![], functions: vec![], exports: vec![], }
    }

    fn add_function_type(&mut self, fn_type: FunctionType) -> TypeIndex {
        let type_index = TypeIndex(self.function_types.len() as u32);
        self.function_types.push(fn_type);
        type_index
    }

    fn add_function(&mut self, function: Function) -> FunctionIndex {
        let fn_index = FunctionIndex(self.functions.len() as u32);
        self.functions.push(FunctionOrImport::Fn(function));
        fn_index
    }

    fn add_typed_function(&mut self, typed_function: TypedFunction) -> FunctionIndex {
        let type_ = typed_function.type_;
        let type_index = self.add_function_type(type_);
        let fn_index = self.add_function(Function { type_index, locals: typed_function.locals, body: typed_function.body });
        fn_index
    }

    fn add_export(&mut self, export: Export) {
        self.exports.push(export);
    }

    fn add_function_import(&mut self, fn_import: FunctionImport) -> FunctionIndex {
        let fn_index = FunctionIndex(self.functions.len() as u32);
        self.functions.push(FunctionOrImport::Import(fn_import));
        fn_index
    }

    fn add_typed_function_import(&mut self, fn_import: TypedFunctionImport) -> FunctionIndex {
        let type_ = fn_import.type_;
        let type_index = self.add_function_type(type_);
        self.add_function_import(FunctionImport { module_name: fn_import.module_name, name: fn_import.name, type_index })
    }

    pub fn binary_format(self) -> sections::Module {
        let mut bin_module = sections::Module::empty();

        bin_module.type_section = Some(TypeSection { function_types: self.function_types });

        bin_module.function_section = {
            let function_types_map: Vec<TypeIndex> = self.functions.iter().filter_map(|fn_| match fn_ {
                FunctionOrImport::Fn(fn_) => Some(fn_.type_index),
                FunctionOrImport::Import(_) => None,
            }).collect();
            Some(FunctionSection { type_indices: function_types_map })
        };

        bin_module.import_section = {
            // TODO: Can this be done without cloning of strings?
            let imports: Vec<Import> = self.functions.iter().filter_map(|fn_| match fn_ {
                FunctionOrImport::Import(fn_import) => Some(Import { module_name: fn_import.module_name.clone(), name: fn_import.name.clone(), import_description: ImportDescription::FunctionTypeIndex(fn_import.type_index) }),
                FunctionOrImport::Fn(_) => None,
            }).collect();
            Some(ImportSection { imports })
        };

        bin_module.code_section = {
            let codes: Vec<sections::Code> = self.functions.into_iter().filter_map(|fn_| match fn_ {
                FunctionOrImport::Fn(fn_) => Some(fn_.binary_format()),
                FunctionOrImport::Import(_) => None,
            }).collect();
            Some(CodeSection { codes })
        };

        bin_module.export_section = Some(ExportSection { exports: self.exports });

        bin_module
    }
}

impl Function {
    pub fn binary_format(self) -> sections::Code {
        let locals: Vec<sections::LocalDeclaration> = self.locals.into_iter().map(|value_type| sections::LocalDeclaration { count: 1, type_: value_type }).collect();
        let expression: sections::Expression = self.body.binary_format();
        sections::Code {
            locals,
            expression,
        }
    }
}

impl Expression {
    pub fn binary_format(self) -> sections::Expression {

        fn binary_format_instructions(expr: Expression, instructions: &mut Vec<sections::Instruction>)  {
            match expr {
                Expression::Unreachable => instructions.push(sections::Instruction::Unreachable),
                Expression::Nop => instructions.push(sections::Instruction::Nop),

                // ===Control Instructions===
                Expression::Block { type_, body } => {
                    let mut body_instructions = vec![];
                    binary_format_instructions(*body, &mut body_instructions);
                    instructions.push(sections::Instruction::Block(type_, body_instructions))
                },
                Expression::Loop { type_, body } => {
                    let mut body_instructions = vec![];
                    binary_format_instructions(*body, &mut body_instructions);
                    instructions.push(sections::Instruction::Loop(type_, body_instructions))
                },
                Expression::IfThen { type_, then_body } => {
                    let mut then_body_instructions = vec![];
                    binary_format_instructions(*then_body, &mut then_body_instructions);
                    instructions.push(sections::Instruction::IfThen(type_, then_body_instructions))
                },
                Expression::IfThenElse { type_, then_body, else_body } => {
                    let mut then_body_instructions = vec![];
                    binary_format_instructions(*then_body, &mut then_body_instructions);

                    let mut else_body_instructions = vec![];
                    binary_format_instructions(*else_body, &mut else_body_instructions);

                    instructions.push(sections::Instruction::IfThenElse(type_, then_body_instructions, else_body_instructions))
                },
                // Expression::Br(index) => instructions.push(sections::Instruction::Br(index)),
                Expression::Call(index, args) => {
                    for arg in args {
                        binary_format_instructions(arg, instructions);
                    }
                    instructions.push(sections::Instruction::Call(index))
                },
                Expression::Return(expr) => todo!(),

                // ===Variable Instructions====
                Expression::LocalGet(index) => instructions.push(sections::Instruction::LocalGet(index)),
                Expression::LocalSet(index, arg) => {
                    binary_format_instructions(*arg, instructions);
                    instructions.push(sections::Instruction::LocalSet(index))
                },
                Expression::LocalTee(index) => instructions.push(sections::Instruction::LocalTee(index)),
                Expression::GlobalGet(index) => instructions.push(sections::Instruction::GlobalGet(index)),
                Expression::GlobalSet(index, arg) => {
                    binary_format_instructions(*arg, instructions);
                    instructions.push(sections::Instruction::GlobalSet(index))
                },

                // ===Numeric Instructions===
                Expression::Op0(Op0::Const(NumberLiteral::I32(x))) => instructions.push(sections::Instruction::I32Const(x)),
                Expression::Op0(Op0::Const(NumberLiteral::I64(x))) => instructions.push(sections::Instruction::I64Const(x)),
                Expression::Op0(Op0::Const(_)) => todo!(),

                Expression::Op1(op1, arg) => {
                    binary_format_instructions(*arg, instructions);

                    match op1 {
                        Op1::Int(Size::X32, int_op) => match int_op {
                            IntegerOp1::Clz => todo!(),
                            IntegerOp1::Ctz => todo!(),
                            IntegerOp1::Popcnt => todo!(),
                        },
                        _ => todo!(),
                    }
                },
                Expression::Op2(op2, arg0, arg1) => {
                    binary_format_instructions(*arg0, instructions);
                    binary_format_instructions(*arg1, instructions);
                    
                    match op2 {
                        Op2::Int(Size::X32, int_op) => match int_op {
                            IntegerOp2::Add => instructions.push(sections::Instruction::I32Add),
                            IntegerOp2::Sub => instructions.push(sections::Instruction::I32Sub),
                            IntegerOp2::Mul => instructions.push(sections::Instruction::I32Mul),
                            _ => todo!(),
                        },
                        _ => todo!(),
                    }
                },

                _ => todo!(),
            }
        }

        let mut instructions = vec![];
        binary_format_instructions(self, &mut instructions);
        sections::Expression { instructions }
    }
}

// ===Helpers===
fn i32_const(x: i32) -> Expression {
    Expression::Op0(Op0::Const(NumberLiteral::I32(x)))
}

fn i32_add(e0: Expression, e1: Expression) -> Expression {
    Expression::Op2(Op2::Int(Size::X32, IntegerOp2::Add), Box::new(e0), Box::new(e1))
}

fn i32_mul(e0: Expression, e1: Expression) -> Expression {
    Expression::Op2(Op2::Int(Size::X32, IntegerOp2::Mul), Box::new(e0), Box::new(e1))
}

fn local_get(i: u32) -> Expression {
    Expression::LocalGet(LocalIndex(i))
}

fn call(fn_index: FunctionIndex, args: Vec<Expression>) -> Expression {
    Expression::Call(fn_index, args)
}

const TYPE_I32: ValueType = ValueType::NumType(NumType::I32);
const TYPE_I64: ValueType = ValueType::NumType(NumType::I64);

fn fn_type(domain: Vec<ValueType>, codomain: Vec<ValueType>) -> FunctionType {
    FunctionType { domain, codomain }
}

// ===Examples===

pub fn example0() -> Module {
    use NumType::*;
    use Expression::*;

    let mut module = Module::empty();

    // (func (param i32)
    // )
    module.add_typed_function(TypedFunction {
        type_: FunctionType { domain: vec![ValueType::NumType(I32)], codomain: vec![], },
        locals: vec![],
        body: Nop,
    });

    module
}

pub fn example1() -> Module {
    let mut module = Module::empty();

    // (func (param i32) (result i32)
    //    local.get 0
    //    local.get 0
    //    i32.mul
    // )
    //
    // (func (param i32) (result i32)
    //    (i32.mul (local.get 0) (local.get 0))
    // )

    let square = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![TYPE_I32], vec![TYPE_I32]),
        locals: vec![],
        body: i32_mul(local_get(0), local_get(0)),
    });
    module.add_export(Export { name: "square".to_string(), export_description: ExportDescription::Function(square) });

    // (func (param $x i32) (param $y i32) (result i32)
    //   (i32.add (call $square (local.get $x)) (call $square (local.get $y)))
    // )
    let pyth = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![TYPE_I32, TYPE_I32], vec![TYPE_I32]),
        locals: vec![],
        body: i32_add(call(square, vec![local_get(0)]), call(square, vec![local_get(1)])),
    });
    module.add_export(Export { name: "pyth".to_string(), export_description: ExportDescription::Function(pyth) });

    module
}

pub fn example2() -> Module {
    let mut module = Module::empty();

    let log = module.add_typed_function_import(TypedFunctionImport { module_name: "console".to_string(), name: "log".to_string(), type_: fn_type(vec![TYPE_I32], vec![]) });

    let square = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![TYPE_I32], vec![TYPE_I32]),
        locals: vec![],
        body: i32_mul(local_get(0), local_get(0)),
    });
    module.add_export(Export { name: "square".to_string(), export_description: ExportDescription::Function(square) });

    let pyth = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![TYPE_I32, TYPE_I32], vec![TYPE_I32]),
        locals: vec![],
        body: i32_add(call(square, vec![local_get(0)]), call(square, vec![local_get(1)])),
    });
    module.add_export(Export { name: "pyth".to_string(), export_description: ExportDescription::Function(pyth) });

    let log_pyth = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![], vec![]),
        locals: vec![],
        body: call(log, vec![i32_const(512)]),
    });
    module.add_export(Export { name: "log_pyth".to_string(), export_description: ExportDescription::Function(log_pyth) });

    module
}
