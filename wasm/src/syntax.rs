use crate::base::{
    export::{Export, ExportDescription},
    import::{Import, ImportDescription},
    indices::{
        FunctionIndex, GlobalIndex, LabelIndex, LocalIndex, MemoryIndex, TableIndex, TypeIndex,
    },
    instructions,
    memory::{Limit, MemoryArgument},
    types::{BlockType, FunctionType, GlobalType, NumType, RefType, ValueType},
};
use crate::binary_format::sections;
use crate::binary_format::sections::{
    CodeSection, Element, ElementSection, ExportSection, FunctionSection,
    GlobalsSection, ImportSection, MemorySection, TableSection, TableType, TypeSection,
};
use crate::{ByteStream, Encoder};

pub struct Module {
    function_types: Vec<FunctionType>,
    // TODO: Bundling Function Imports and Functions into one place is convinient, but not exactly
    // performance friendly.
    functions: Vec<FunctionOrImport>,
    function_table: Vec<FunctionIndex>,
    globals: Vec<Global>,
    exports: Vec<Export>,
    memories: Vec<MemoryOrImport>,
    custom_section_at_the_end: Vec<CustomSection>,
}

enum FunctionOrImport {
    Fn(Function),
    Import(FunctionImport),
}

enum MemoryOrImport {
    Memory(Limit),
    Import(MemoryImport),
}

struct Function {
    type_index: TypeIndex,
    locals: Vec<ValueType>,
    body: Expression,
}

pub struct TypedFunction {
    pub type_: FunctionType,
    pub locals: Vec<ValueType>,
    pub body: Expression,
}

struct FunctionImport {
    module_name: String,
    name: String,
    type_index: TypeIndex,
}

pub struct MemoryImport {
    pub module_name: String,
    pub name: String,
    pub limit: Limit,
}

pub struct TypedFunctionImport {
    pub module_name: String,
    pub name: String,
    pub type_: FunctionType,
}

#[derive(Debug)]
pub struct Global {
    pub global_type: GlobalType,
    pub expression: Expression,
}

pub type CustomSection = sections::CustomSection;

#[derive(Debug)]
pub enum Expression {
    Seq(Vec<Expression>),

    Unreachable,
    Nop,

    // ===Control Instructions===
    Block {
        type_: BlockType,
        body: Box<Expression>,
    },
    Loop {
        type_: BlockType,
        body: Box<Expression>,
    },
    IfThen {
        type_: BlockType,
        test: Box<Expression>,
        then_body: Box<Expression>,
    },
    IfThenElse {
        type_: BlockType,
        test: Box<Expression>,
        then_body: Box<Expression>,
        else_body: Box<Expression>,
    },
    Br(LabelIndex),
    Call(FunctionIndex, Vec<Expression>),
    ReturnCall(FunctionIndex, Vec<Expression>),
    CallIndirect(TypeIndex, TableIndex, Vec<Expression>),
    ReturnCallIndirect(TypeIndex, TableIndex, Vec<Expression>),
    Return(Box<Expression>),

    // ===Variable Instructions===
    LocalGet(LocalIndex),
    LocalSet(LocalIndex, Box<Expression>),
    LocalTee(LocalIndex),
    GlobalGet(GlobalIndex),
    GlobalSet(GlobalIndex, Box<Expression>),

    // ===Memory Instructions===
    LoadInteger {
        size: Size,
        arg: MemoryArgument,
        address: Box<Expression>,
    },
    StoreInteger {
        size: Size,
        arg: MemoryArgument,
        address: Box<Expression>,
        value: Box<Expression>,
    },

    // ===Numeric Instructions===
    Op0(Op0),
    Op1(Op1, Box<Expression>),
    Op2(Op2, Box<Expression>, Box<Expression>),
    Rel1(Rel1, Box<Expression>),
    Rel2(Rel2, Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
pub enum Op0 {
    Const(NumberLiteral),
}

#[derive(Debug)]
pub enum NumberLiteral {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Debug)]
pub enum Op1 {
    Int(Size, IntegerOp1),
    Float(Size, FloatOp1),
}

#[derive(Debug)]
pub enum Op2 {
    Int(Size, IntegerOp2),
    Float(Size, FloatOp2),
}

#[derive(Debug)]
pub enum IntegerOp1 {
    Clz,
    Ctz,
    Popcnt,
}

#[derive(Debug)]
pub enum Size {
    X32,
    X64,
}

#[derive(Debug)]
pub enum IntegerOp2 {
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
    Rotr,
}

#[derive(Debug)]
pub enum FloatOp1 {
    Abs,
    Neg,
    Sqrt,
    Ceil,
    Floor,
    Trunc,
    Nearest,
}

#[derive(Debug)]
pub enum FloatOp2 {
    Add,
    Sub,
    Mul,
    Div,
    Min,
    Max,
    Copysign,
}

#[derive(Debug)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(Debug)]
pub enum Rel1 {
    Int(Size, IntegerRel1),
}

#[derive(Debug)]
pub enum Rel2 {
    Int(Size, IntegerRel2),
    Float(Size, FloatRel2),
}

#[derive(Debug)]
pub enum IntegerRel1 {
    Eqz,
}

#[derive(Debug)]
pub enum IntegerRel2 {
    Eq,
    Ne,
    Lt(Signedness),
    Gt(Signedness),
    Le(Signedness),
    Ge(Signedness),
}

#[derive(Debug)]
pub enum FloatRel2 {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

impl Module {
    pub fn empty() -> Self {
        Self {
            function_types: vec![],
            functions: vec![],
            exports: vec![],
            memories: vec![],
            function_table: vec![],
            globals: vec![],
            custom_section_at_the_end: vec![],
        }
    }

    pub fn add_function_type(&mut self, fn_type: FunctionType) -> TypeIndex {
        let type_index = TypeIndex(self.function_types.len() as u32);
        self.function_types.push(fn_type);
        type_index
    }

    fn add_function(&mut self, function: Function) -> FunctionIndex {
        let fn_index = FunctionIndex(self.functions.len() as u32);
        self.functions.push(FunctionOrImport::Fn(function));
        fn_index
    }

    pub fn add_typed_function(&mut self, typed_function: TypedFunction) -> FunctionIndex {
        let type_ = typed_function.type_;
        let type_index = self.add_function_type(type_);
        self.add_function(Function {
            type_index,
            locals: typed_function.locals,
            body: typed_function.body,
        })
    }

    pub fn add_global(&mut self, global: Global) -> GlobalIndex {
        let global_index = GlobalIndex(self.globals.len() as u32);
        self.globals.push(global);
        global_index
    }

    pub fn add_memory(&mut self, limit: Limit) -> MemoryIndex {
        let memory_index = MemoryIndex(self.memories.len() as u32);
        self.memories.push(MemoryOrImport::Memory(limit));
        memory_index
    }

    pub fn add_memory_import(&mut self, memory_import: MemoryImport) -> MemoryIndex {
        let memory_index = MemoryIndex(self.memories.len() as u32);
        self.memories.push(MemoryOrImport::Import(memory_import));
        memory_index
    }

    pub fn add_export(&mut self, export: Export) {
        self.exports.push(export);
    }

    fn add_function_import(&mut self, fn_import: FunctionImport) -> FunctionIndex {
        let fn_index = FunctionIndex(self.functions.len() as u32);
        self.functions.push(FunctionOrImport::Import(fn_import));
        fn_index
    }

    pub fn add_typed_function_import(&mut self, fn_import: TypedFunctionImport) -> FunctionIndex {
        let type_ = fn_import.type_;
        let type_index = self.add_function_type(type_);
        self.add_function_import(FunctionImport {
            module_name: fn_import.module_name,
            name: fn_import.name,
            type_index,
        })
    }

    pub fn add_custom_section_at_the_end(&mut self, custom_section: CustomSection) {
        self.custom_section_at_the_end.push(custom_section);
    }

    pub fn register_function_table(&mut self, function_table: Vec<FunctionIndex>) {
        self.function_table = function_table
    }

    pub fn binary_format(self) -> sections::Module {
        let mut bin_module = sections::Module::empty();

        bin_module.type_section = Some(TypeSection {
            function_types: self.function_types,
        });

        bin_module.function_section = {
            let function_types_map: Vec<TypeIndex> = self
                .functions
                .iter()
                .filter_map(|fn_| match fn_ {
                    FunctionOrImport::Fn(fn_) => Some(fn_.type_index),
                    FunctionOrImport::Import(_) => None,
                })
                .collect();
            Some(FunctionSection {
                type_indices: function_types_map,
            })
        };

        bin_module.memory_section = {
            let memory_types = self
                .memories
                .iter()
                .filter_map(|memory| match memory {
                    MemoryOrImport::Memory(limit) => Some(*limit),
                    MemoryOrImport::Import(_) => None,
                })
                .collect();
            Some(MemorySection { memory_types })
        };

        bin_module.table_section = {
            let number_of_closures = self.function_table.len() as u32;
            let table_type = TableType {
                reftype: RefType::FuncRef,
                limit: Limit::MinMax {
                    min: number_of_closures,
                    max: number_of_closures,
                },
            };
            Some(TableSection {
                table_types: vec![table_type],
            })
        };

        bin_module.import_section = {
            // TODO: Can this be done without cloning of strings?
            let mut imports: Vec<Import> = self
                .functions
                .iter()
                .filter_map(|fn_| match fn_ {
                    FunctionOrImport::Import(fn_import) => Some(Import {
                        module_name: fn_import.module_name.clone(),
                        name: fn_import.name.clone(),
                        import_description: ImportDescription::FunctionTypeIndex(
                            fn_import.type_index,
                        ),
                    }),
                    FunctionOrImport::Fn(_) => None,
                })
                .collect();

            // TODO: Can this be done without cloning of strings?
            imports.extend(self.memories.into_iter().filter_map(|memory| match memory {
                MemoryOrImport::Import(memory_import) => Some(Import {
                    module_name: memory_import.module_name.clone(),
                    name: memory_import.name.clone(),
                    import_description: ImportDescription::MemoryType(memory_import.limit),
                }),
                MemoryOrImport::Memory(_) => None,
            }));

            Some(ImportSection { imports })
        };

        bin_module.element_section = {
            Some(ElementSection {
                elements: vec![Element {
                    offset_expression: sections::Expression {
                        instructions: vec![instructions::Instruction::I32Const(0)],
                    },
                    function_references: self.function_table,
                }],
            })
        };

        bin_module.code_section = {
            let codes: Vec<sections::Code> = self
                .functions
                .into_iter()
                .filter_map(|fn_| match fn_ {
                    FunctionOrImport::Fn(fn_) => Some(fn_.binary_format()),
                    FunctionOrImport::Import(_) => None,
                })
                .collect();
            Some(CodeSection { codes })
        };

        bin_module.globals_section = {
            let globals = self
                .globals
                .into_iter()
                .map(|global| sections::Global {
                    global_type: global.global_type,
                    expression: global.expression.binary_format(),
                })
                .collect();
            Some(GlobalsSection { globals })
        };

        bin_module.export_section = Some(ExportSection {
            exports: self.exports,
        });

        bin_module.custom_section_at_the_end = self.custom_section_at_the_end;

        bin_module
    }

    pub fn emit(self) -> <sections::Module as Encoder>::S {
        self.binary_format().emit()
    }

    pub fn bytes(self) -> Vec<u8> {
        self.emit().to_vec()
    }
}

impl Function {
    pub fn binary_format(self) -> sections::Code {
        let locals: Vec<sections::LocalDeclaration> = self
            .locals
            .into_iter()
            .map(|value_type| sections::LocalDeclaration {
                count: 1,
                type_: value_type,
            })
            .collect();
        let expression: sections::Expression = self.body.binary_format();
        sections::Code { locals, expression }
    }
}

impl Expression {
    pub fn binary_format(self) -> sections::Expression {
        fn binary_format_instructions(
            expr: Expression,
            instructions: &mut Vec<instructions::Instruction>,
        ) {
            match expr {
                Expression::Seq(expressions) => {
                    if expressions.is_empty() {
                        instructions.push(instructions::Instruction::Nop);
                    } else {
                        for expr in expressions {
                            binary_format_instructions(expr, instructions);
                        }
                    }
                }

                Expression::Unreachable => {
                    instructions.push(instructions::Instruction::Unreachable)
                }
                Expression::Nop => instructions.push(instructions::Instruction::Nop),

                // ===Control Instructions===
                Expression::Block { type_, body } => {
                    let mut body_instructions = vec![];
                    binary_format_instructions(*body, &mut body_instructions);
                    instructions.push(instructions::Instruction::Block(type_, body_instructions))
                }
                Expression::Loop { type_, body } => {
                    let mut body_instructions = vec![];
                    binary_format_instructions(*body, &mut body_instructions);
                    instructions.push(instructions::Instruction::Loop(type_, body_instructions))
                }
                Expression::IfThen {
                    type_,
                    test,
                    then_body,
                } => {
                    binary_format_instructions(*test, instructions);
                    let mut then_body_instructions = vec![];
                    binary_format_instructions(*then_body, &mut then_body_instructions);

                    instructions.push(instructions::Instruction::IfThen(
                        type_,
                        then_body_instructions,
                    ))
                }
                Expression::IfThenElse {
                    type_,
                    test,
                    then_body,
                    else_body,
                } => {
                    binary_format_instructions(*test, instructions);

                    let mut then_body_instructions = vec![];
                    binary_format_instructions(*then_body, &mut then_body_instructions);

                    let mut else_body_instructions = vec![];
                    binary_format_instructions(*else_body, &mut else_body_instructions);

                    instructions.push(instructions::Instruction::IfThenElse(
                        type_,
                        then_body_instructions,
                        else_body_instructions,
                    ))
                }
                Expression::Br(index) => instructions.push(instructions::Instruction::Br(index)),
                Expression::Call(index, args) => {
                    for arg in args {
                        binary_format_instructions(arg, instructions);
                    }
                    instructions.push(instructions::Instruction::Call(index))
                }
                Expression::ReturnCall(index, args) => {
                    for arg in args {
                        binary_format_instructions(arg, instructions);
                    }
                    instructions.push(instructions::Instruction::ReturnCall(index))
                }
                Expression::CallIndirect(type_index, table_index, args) => {
                    for arg in args {
                        binary_format_instructions(arg, instructions);
                    }
                    instructions.push(instructions::Instruction::CallIndirect(
                        type_index,
                        table_index,
                    ))
                }
                Expression::ReturnCallIndirect(type_index, table_index, args) => {
                    for arg in args {
                        binary_format_instructions(arg, instructions);
                    }
                    instructions.push(instructions::Instruction::ReturnCallIndirect(
                        type_index,
                        table_index,
                    ))
                }
                Expression::Return(expr) => {
                    binary_format_instructions(*expr, instructions);
                    instructions.push(instructions::Instruction::Return)
                }

                // ===Variable Instructions====
                Expression::LocalGet(index) => {
                    instructions.push(instructions::Instruction::LocalGet(index))
                }
                Expression::LocalSet(var, value) => {
                    binary_format_instructions(*value, instructions);
                    instructions.push(instructions::Instruction::LocalSet(var));
                }
                Expression::LocalTee(index) => {
                    instructions.push(instructions::Instruction::LocalTee(index))
                }
                Expression::GlobalGet(index) => {
                    instructions.push(instructions::Instruction::GlobalGet(index))
                }
                Expression::GlobalSet(var, value) => {
                    binary_format_instructions(*value, instructions);
                    instructions.push(instructions::Instruction::GlobalSet(var));
                }

                // ===Memory Instructions===
                Expression::LoadInteger { size, arg, address } => {
                    binary_format_instructions(*address, instructions);
                    instructions.push(match size {
                        Size::X32 => instructions::Instruction::I32Load(arg),
                        Size::X64 => {
                            todo!()
                            // instructions::Instruction::I64Load(arg)
                        }
                    })
                }

                Expression::StoreInteger {
                    size,
                    arg,
                    address,
                    value,
                } => {
                    binary_format_instructions(*address, instructions);
                    binary_format_instructions(*value, instructions);
                    instructions.push(match size {
                        Size::X32 => instructions::Instruction::I32Store(arg),
                        Size::X64 => {
                            todo!()
                            // instructions::Instruction::I64Load(arg)
                        }
                    })
                }

                // ===Numeric Instructions===
                Expression::Op0(Op0::Const(NumberLiteral::I32(x))) => {
                    instructions.push(instructions::Instruction::I32Const(x))
                }
                Expression::Op0(Op0::Const(NumberLiteral::I64(x))) => {
                    instructions.push(instructions::Instruction::I64Const(x))
                }
                Expression::Op0(Op0::Const(NumberLiteral::F32(x))) => {
                    instructions.push(instructions::Instruction::F32Const(x))
                }
                Expression::Op0(Op0::Const(NumberLiteral::F64(_x))) => todo!(),

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
                }
                Expression::Op2(op2, arg0, arg1) => {
                    binary_format_instructions(*arg0, instructions);
                    binary_format_instructions(*arg1, instructions);

                    match op2 {
                        Op2::Int(Size::X32, int_op) => match int_op {
                            IntegerOp2::Add => instructions.push(instructions::Instruction::I32Add),
                            IntegerOp2::Sub => instructions.push(instructions::Instruction::I32Sub),
                            IntegerOp2::Mul => instructions.push(instructions::Instruction::I32Mul),
                            _ => todo!(),
                        },
                        _ => todo!(),
                    }
                }

                Expression::Rel2(rel2, arg0, arg1) => {
                    binary_format_instructions(*arg0, instructions);
                    binary_format_instructions(*arg1, instructions);

                    match rel2 {
                        Rel2::Int(Size::X32, int_rel) => match int_rel {
                            IntegerRel2::Eq => instructions.push(instructions::Instruction::I32Eq),
                            _ => todo!(),
                        },
                        _ => todo!(),
                    }
                }

                _ => todo!(),
            }
        }

        let mut instructions = vec![];
        binary_format_instructions(self, &mut instructions);
        sections::Expression { instructions }
    }
}

// ===Helpers===
pub fn seq(expressions: Vec<Expression>) -> Expression {
    Expression::Seq(expressions)
}

pub fn typed_loop(type_: ValueType, body: Expression) -> Expression {
    Expression::Loop {
        type_: BlockType::ValueType(type_),
        body: Box::new(body),
    }
}

pub fn typed_if_then_else(
    type_: ValueType,
    test: Expression,
    then_body: Expression,
    else_body: Expression,
) -> Expression {
    Expression::IfThenElse {
        type_: BlockType::ValueType(type_),
        test: Box::new(test),
        then_body: Box::new(then_body),
        else_body: Box::new(else_body),
    }
}

pub fn i32_const(x: i32) -> Expression {
    Expression::Op0(Op0::Const(NumberLiteral::I32(x)))
}

pub fn f32_const(x: f32) -> Expression {
    Expression::Op0(Op0::Const(NumberLiteral::F32(x)))
}

pub fn i32_add(e0: Expression, e1: Expression) -> Expression {
    Expression::Op2(
        Op2::Int(Size::X32, IntegerOp2::Add),
        Box::new(e0),
        Box::new(e1),
    )
}

pub fn i32_mul(e0: Expression, e1: Expression) -> Expression {
    Expression::Op2(
        Op2::Int(Size::X32, IntegerOp2::Mul),
        Box::new(e0),
        Box::new(e1),
    )
}

pub fn i32_sub(e0: Expression, e1: Expression) -> Expression {
    Expression::Op2(
        Op2::Int(Size::X32, IntegerOp2::Sub),
        Box::new(e0),
        Box::new(e1),
    )
}

pub fn i32_eq(e0: Expression, e1: Expression) -> Expression {
    Expression::Rel2(
        Rel2::Int(Size::X32, IntegerRel2::Eq),
        Box::new(e0),
        Box::new(e1),
    )
}

pub fn local_get(i: u32) -> Expression {
    Expression::LocalGet(LocalIndex(i))
}

pub fn local_set(i: u32, value: Expression) -> Expression {
    Expression::LocalSet(LocalIndex(i), Box::new(value))
}

pub fn branch(i: u32) -> Expression {
    Expression::Br(LabelIndex(i))
}

pub fn call(fn_index: FunctionIndex, args: Vec<Expression>) -> Expression {
    Expression::Call(fn_index, args)
}

pub fn return_call(fn_index: FunctionIndex, args: Vec<Expression>) -> Expression {
    Expression::ReturnCall(fn_index, args)
}

pub fn call_indirect(
    type_index: TypeIndex,
    table_index: TableIndex,
    args: Vec<Expression>,
) -> Expression {
    Expression::CallIndirect(type_index, table_index, args)
}

pub fn return_call_indirect(
    type_index: TypeIndex,
    table_index: TableIndex,
    args: Vec<Expression>,
) -> Expression {
    Expression::ReturnCallIndirect(type_index, table_index, args)
}

pub fn i32_memory_get(address: Expression) -> Expression {
    Expression::LoadInteger {
        size: Size::X32,
        arg: MemoryArgument {
            align: 2,
            offset: 0,
        },
        address: Box::new(address),
    }
}

pub fn i64_memory_get(address: Expression) -> Expression {
    Expression::LoadInteger {
        size: Size::X64,
        arg: MemoryArgument {
            align: 3,
            offset: 0,
        },
        address: Box::new(address),
    }
}

pub fn i32_memory_set(address: Expression, value: Expression) -> Expression {
    Expression::StoreInteger {
        size: Size::X32,
        arg: MemoryArgument {
            align: 2,
            offset: 0,
        },
        address: Box::new(address),
        value: Box::new(value),
    }
}

pub fn i64_memory_set(address: Expression, value: Expression) -> Expression {
    Expression::StoreInteger {
        size: Size::X64,
        arg: MemoryArgument {
            align: 3,
            offset: 0,
        },
        address: Box::new(address),
        value: Box::new(value),
    }
}

pub const TYPE_I32: ValueType = ValueType::NumType(NumType::I32);
pub const TYPE_I64: ValueType = ValueType::NumType(NumType::I64);
pub const TYPE_F32: ValueType = ValueType::NumType(NumType::F32);

pub fn fn_type(domain: Vec<ValueType>, codomain: Vec<ValueType>) -> FunctionType {
    FunctionType { domain, codomain }
}

// ===Examples===

pub fn example0() -> Module {
    use Expression::*;
    use NumType::*;

    let mut module = Module::empty();

    // (func (param i32)
    // )
    module.add_typed_function(TypedFunction {
        type_: FunctionType {
            domain: vec![ValueType::NumType(I32)],
            codomain: vec![],
        },
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
    module.add_export(Export {
        name: "square".to_string(),
        export_description: ExportDescription::Function(square),
    });

    // (func (param $x i32) (param $y i32) (result i32)
    //   (i32.add (call $square (local.get $x)) (call $square (local.get $y)))
    // )
    let pyth = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![TYPE_I32, TYPE_I32], vec![TYPE_I32]),
        locals: vec![],
        body: i32_add(
            call(square, vec![local_get(0)]),
            call(square, vec![local_get(1)]),
        ),
    });
    module.add_export(Export {
        name: "pyth".to_string(),
        export_description: ExportDescription::Function(pyth),
    });

    module
}

pub fn example2() -> Module {
    let mut module = Module::empty();

    let log = module.add_typed_function_import(TypedFunctionImport {
        module_name: "console".to_string(),
        name: "log".to_string(),
        type_: fn_type(vec![TYPE_I32], vec![]),
    });

    let square = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![TYPE_I32], vec![TYPE_I32]),
        locals: vec![],
        body: i32_mul(local_get(0), local_get(0)),
    });
    module.add_export(Export {
        name: "square".to_string(),
        export_description: ExportDescription::Function(square),
    });

    let pyth = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![TYPE_I32, TYPE_I32], vec![TYPE_I32]),
        locals: vec![],
        body: i32_add(
            call(square, vec![local_get(0)]),
            call(square, vec![local_get(1)]),
        ),
    });
    module.add_export(Export {
        name: "pyth".to_string(),
        export_description: ExportDescription::Function(pyth),
    });

    let log_pyth = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![], vec![]),
        locals: vec![],
        body: call(log, vec![i32_const(512)]),
    });
    module.add_export(Export {
        name: "log_pyth".to_string(),
        export_description: ExportDescription::Function(log_pyth),
    });

    module
}

pub fn example_factorial() -> Module {
    let mut module = Module::empty();

    // (func $fct (param $x i32) (result i32)
    //   (local $state i32)
    //
    //   (local.set $state (i32.const 1))
    //   (loop $loop (result i32)
    //     (if (result i32)
    //       (i32.eq (local.get $x) (i32.const 0))
    //       (then
    //         (local.get $state)
    //       )
    //       (else
    //         (local.set $state (i32.mul (local.get $state) (local.get $x)))
    //         (local.set $state (i32.sub (local.get $x) (i32.const 1)))
    //         (br $loop)
    //       )
    //   )
    // )
    let fct = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![TYPE_I32], vec![TYPE_I32]),
        locals: vec![TYPE_I32],
        body: seq(vec![
            local_set(1, i32_const(1)),
            typed_loop(
                TYPE_I32,
                typed_if_then_else(
                    TYPE_I32,
                    i32_eq(local_get(0), i32_const(0)),
                    local_get(1),
                    seq(vec![
                        local_set(1, i32_mul(local_get(1), local_get(0))),
                        local_set(0, i32_sub(local_get(0), i32_const(1))),
                        branch(1),
                    ]),
                ),
            ),
        ]),
    });
    module.add_export(Export {
        name: "fct".to_string(),
        export_description: ExportDescription::Function(fct),
    });

    module
}

pub fn example_memory0() -> Module {
    let mut module = Module::empty();

    let memory = module.add_memory(Limit::MinMax { min: 1, max: 1 });
    module.add_export(Export {
        name: "main_memory".to_string(),
        export_description: ExportDescription::Memory(memory),
    });

    let some_fn = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![], vec![TYPE_I32]),
        locals: vec![],
        body: seq(vec![
            i32_memory_set(i32_const(0), i32_const(123)),
            i32_memory_get(i32_const(0)),
        ]),
    });

    module.add_export(Export {
        name: "some_fn".to_string(),
        export_description: ExportDescription::Function(some_fn),
    });

    module
}

pub fn example_memory1() -> Module {
    let mut module = Module::empty();

    let memory = module.add_memory(Limit::MinMax { min: 1, max: 1 });
    module.add_export(Export {
        name: "main_memory".to_string(),
        export_description: ExportDescription::Memory(memory),
    });

    // pointer <- 0
    // offset <- 1
    //
    // memory[pointer] <- 6
    //
    // pointer += offset
    // memory[pointer] <- 7
    //
    // pointer += offset
    // memory[pointer] <- memory[0] + memory[4]
    //
    // memory[pointer]
    let some_fn = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![], vec![TYPE_I32]),
        locals: vec![TYPE_I32, TYPE_I32],
        body: {
            let pointer = 0;
            let offset = 1;
            seq(vec![
                local_set(offset, i32_const(4)),
                local_set(pointer, i32_const(0)),
                i32_memory_set(local_get(pointer), i32_const(6)),
                local_set(pointer, i32_add(local_get(pointer), local_get(offset))),
                i32_memory_set(local_get(pointer), i32_const(7)),
                local_set(pointer, i32_add(local_get(pointer), local_get(offset))),
                i32_memory_set(
                    local_get(pointer),
                    i32_add(i32_memory_get(i32_const(0)), i32_memory_get(i32_const(4))),
                ),
                i32_memory_get(local_get(pointer)),
            ])
        },
    });

    module.add_export(Export {
        name: "some_fn".to_string(),
        export_description: ExportDescription::Function(some_fn),
    });

    module
}
