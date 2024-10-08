pub mod base;
mod binary_format;
pub mod syntax;

use crate::binary_format::primitives::byte_stream::ByteStream;
use crate::binary_format::primitives::encoder::Encoder;

use crate::base::{
    export::{Export, ExportDescription},
    import::{Import, ImportDescription},
    indices::{FunctionIndex, TypeIndex},
    instructions::Instruction,
    memory::Limit,
    types::{FunctionType, GlobalType, Mutability, NumType, RefType, ValueType},
};
use crate::binary_format::sections::{
    Code, CodeSection, DataCountSection, DataItem, DataSection, Element, ElementSection,
    ExportSection, Expression, FunctionSection, Global, GlobalsSection, ImportSection,
    MemorySection, Module, StartSection, TableSection, TableType, TypeSection,
};
use NumType::*;

// ====Examples====
pub fn generate0() -> Vec<u8> {
    let mut module = Module::empty();
    module.type_section = Some(TypeSection {
        function_types: vec![
            // FuncType { domain: vec![ ValueType::NumType(I32), ValueType::NumType(F64) ], codomain: vec![ ValueType::NumType(I32), ValueType::NumType(I64)] },
            // FuncType { domain: vec![ ValueType::NumType(I32) ], codomain: vec![] },
            // FuncType { domain: vec![ ValueType::NumType(I32) ], codomain: vec![ ValueType::NumType(I32) ] },
            FunctionType {
                domain: vec![],
                codomain: vec![ValueType::NumType(I32)],
            },
            FunctionType {
                domain: vec![ValueType::NumType(I32)],
                codomain: vec![],
            },
            FunctionType {
                domain: vec![],
                codomain: vec![],
            },
        ],
    });
    module.import_section = Some(ImportSection {
        imports: vec![Import {
            module_name: "console".to_string(),
            name: "log".to_string(),
            import_description: ImportDescription::FunctionTypeIndex(TypeIndex(1)),
        }],
    });
    module.function_section = Some(FunctionSection {
        type_indices: vec![
            TypeIndex(0),
            TypeIndex(2), // empty
        ],
    });
    module.table_section = Some(TableSection {
        table_types: vec![
            // Limit::MinToInfinity { min: 1 },
            TableType {
                reftype: RefType::FuncRef,
                limit: Limit::MinMax { min: 1, max: 10 },
            },
        ],
    });
    module.memory_section = Some(MemorySection {
        memory_types: vec![
            // Limit::MinToInfinity { min: 1 },
            // TODO: How can I determine the max?
            Limit::MinMax { min: 1, max: 10 },
        ],
    });
    module.globals_section = Some(GlobalsSection {
        globals: vec![Global {
            global_type: GlobalType {
                type_: ValueType::NumType(I32),
                mutability: Mutability::Var,
            },
            expression: Expression {
                instructions: vec![Instruction::I32Const(1)],
            },
        }],
    });
    module.export_section = Some(ExportSection {
        exports: vec![Export {
            name: "foo😥q".to_string(),
            export_description: ExportDescription::Function(FunctionIndex(0)),
        }],
    });
    module.start_section = Some(StartSection {
        start: FunctionIndex(2),
    });

    // () -> i32
    use Instruction::*;
    let code0 = vec![
        Instruction::I32Const(-4344),
        Instruction::I32Const(1),
        I32Add,
    ];
    //let code1 = vec![
    //    LocalGet(LocalIndex(0)),
    //    LocalGet(LocalIndex(0)),
    //    I32Add,
    //    // Nop,
    //];
    //let code2 = vec![
    //    // LocalGet(LocalIndex(0)),
    //    I32Const(0),
    //    IfThenElse(
    //        BlockType::ValueType(ValueType::NumType(I32)),
    //        vec![LocalGet(LocalIndex(0))],
    //        vec![LocalGet(LocalIndex(0))],
    //    ),
    //];

    let code_empty = vec![];

    module.code_section = Some(CodeSection {
        codes: vec![
            Code {
                locals: vec![],
                expression: Expression {
                    instructions: code0,
                },
            },
            Code {
                locals: vec![],
                expression: Expression {
                    instructions: code_empty,
                },
            },
        ],
    });

    module.element_section = Some(ElementSection {
        elements: vec![Element {
            offset_expression: Expression {
                instructions: vec![Instruction::I32Const(0)],
            },
            function_references: vec![FunctionIndex(0)],
        }],
    });

    module.data_section = Some(DataSection {
        data_items: vec![
            DataItem::Passive {
                initialize: vec![0, 0, 0, 7, 8, 9],
            },
            DataItem::Passive {
                initialize: vec![1, 1, 1, 1, 1, 1, 1],
            },
            DataItem::Active {
                initialize: vec![1, 1, 1, 1, 1, 1, 1],
                offset_expression: Expression {
                    instructions: vec![Instruction::I32Const(75)],
                },
            },
        ],
    });
    module.data_count_section = Some(DataCountSection { count: 3 });

    let mut bytes = module.emit();

    bytes.to_vec()
}

pub fn generate1() -> Vec<u8> {
    let module = syntax::example2();
    let mut bytes = module.binary_format().emit();
    bytes.to_vec()
}

pub fn generate_factorial() -> Vec<u8> {
    let module = syntax::example_factorial();
    let mut bytes = module.binary_format().emit();
    bytes.to_vec()
}

pub fn generate_memory0() -> Vec<u8> {
    let module = syntax::example_memory0();
    let mut bytes = module.binary_format().emit();
    bytes.to_vec()
}

pub fn generate_memory1() -> Vec<u8> {
    let module = syntax::example_memory1();
    let mut bytes = module.binary_format().emit();
    bytes.to_vec()
}
