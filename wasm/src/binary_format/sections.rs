use crate::binary_format::primitives::byte_stream::{ByteStream, Response, Enclose, EVec, evector, bytes4, byte, Byte, Bytes4, CVec, cvector, Seq, U32ToFixed40LEB128};
use crate::binary_format::primitives::encoder::Encoder;
use crate::binary_format::indices::IndexStream;
use crate::binary_format::instructions::Instruction;

use crate::base::{
    types::{FunctionType, ValueType, GlobalType},
    indices::{TypeIndex, FunctionIndex },
    memory::Limit,
    export::Export,
    import::Import,
};

type SectionId = u8;

// === 1 Type Section ===
pub struct TypeSection {
    pub function_types: Vec<FunctionType>
}

impl TypeSection {
    pub const ID: SectionId = 1;
}

impl Encoder for TypeSection {
    type S = Seq<Byte, Enclose<CVec<<FunctionType as Encoder>::S>>>;
    fn emit(&self) -> Self::S {
        let fn_types = self.function_types.iter().map(|fn_type| fn_type.emit()).collect();
        byte(Self::ID)
            .seq(cvector(fn_types).enclose())
    }
}

// === 2 Import Section ===
pub struct ImportSection {
    pub imports: Vec<Import>
}

impl ImportSection {
    pub const ID: SectionId = 2;
}

impl Encoder for ImportSection {
    type S = Seq<Byte, Enclose<CVec<<Import as Encoder>::S>>>;
    fn emit(&self) -> Self::S {
        let imports = self.imports.iter().map(|import| import.emit()).collect();
        byte(Self::ID)
            .seq(cvector(imports).enclose())
    }
}

// === 3 Function Section ===
pub struct FunctionSection {
    pub type_indices: Vec<TypeIndex>
}

impl FunctionSection {
    pub const ID: SectionId = 3;
}

impl Encoder for FunctionSection {
    type S = Seq<Byte, Enclose<CVec<<TypeIndex as Encoder>::S>>>;

    fn emit(&self) -> Self::S {
        let type_indices = self.type_indices.iter().map(|type_index| type_index.emit()).collect();
        byte(Self::ID)
            .seq(cvector(type_indices).enclose())
    }
}

// === 5 Memory Section ===
pub struct MemorySection {
    pub memory_types: Vec<Limit>
}

impl MemorySection {
    pub const ID: SectionId = 5;
}

impl Encoder for MemorySection {
    type S = Seq<Byte, Enclose<CVec<<Limit as Encoder>::S>>>;

    fn emit(&self) -> Self::S {
        let memory_types = self.memory_types.iter().map(|limit| limit.emit()).collect();
        byte(Self::ID)
            .seq(cvector(memory_types).enclose())
    }
}

// === 6 Globals Section ===
pub struct GlobalsSection {
    pub globals: Vec<Global>,
}

impl GlobalsSection {
    pub const ID: SectionId = 6;
}

impl Encoder for GlobalsSection {
    type S = Seq<Byte, Enclose<CVec<<Global as Encoder>::S>>>;

    fn emit(&self) -> Self::S {
        let globals = self.globals.iter().map(|global| global.emit()).collect();
        byte(Self::ID)
            .seq(cvector(globals).enclose())
    }
}

pub struct Global {
    pub global_type: GlobalType,
    pub expression: Expression,
}

impl Encoder for Global {
    type S = Seq<<GlobalType as Encoder>::S, <Expression as Encoder>::S>;
    fn emit(&self) -> Self::S {
        self.global_type.emit().seq(self.expression.emit())
    }
}

// === 7 Export Section ===
pub struct ExportSection {
    pub exports: Vec<Export>
}

impl ExportSection {
    pub const ID: SectionId = 7;
}

impl Encoder for ExportSection {
    type S = Seq<Byte, Enclose<CVec<<Export as Encoder>::S>>>;

    fn emit(&self) -> Self::S {
        let exports = self.exports.iter().map(|export| export.emit()).collect();
        byte(Self::ID)
            .seq(cvector(exports).enclose())
    }
}

// === 8 Start Section ===
pub struct StartSection {
    pub start: FunctionIndex,
}

impl StartSection {
    pub const ID: SectionId = 8;
}

impl Encoder for StartSection {
    type S = Seq<Byte, Enclose<IndexStream>>;

    fn emit(&self) -> Self::S {
        byte(Self::ID)
            .seq(self.start.emit().enclose())
    }
}

// === 10 Code Section ===
pub struct CodeSection {
    pub codes: Vec<Code>,
}

impl CodeSection {
    pub const ID: SectionId = 10;
}

impl Encoder for CodeSection {
    type S = Seq<Byte, Enclose<CVec<<Code as Encoder>::S>>>;

    fn emit(&self) -> Self::S {
        let codes = self.codes.iter().map(|code| code.emit()).collect();
        byte(Self::ID)
            .seq(cvector(codes).enclose())
    }
}


pub struct Code {
    pub locals: Vec<LocalDeclaration>,
    pub expression: Expression,
}

impl Encoder for Code {
    type S = Enclose<Seq<CVec<<LocalDeclaration as Encoder>::S>, <Expression as Encoder>::S>>;

    fn emit(&self) -> Self::S {
        let locals = self.locals.iter().map(|local| local.emit()).collect();
        let expression = self.expression.emit();
        cvector(locals).seq(expression).enclose()
    }
}

pub struct LocalDeclaration {
    pub count: u32,
    pub type_: ValueType,
}

impl Encoder for LocalDeclaration {
    type S = Seq<U32ToFixed40LEB128, <ValueType as Encoder>::S>;

    fn emit(&self) -> Self::S {
        let count = U32ToFixed40LEB128::new(self.count);
        let type_ = self.type_.emit();
        count.seq(type_)
    }
}

pub struct Expression {
    pub instructions: Vec<Instruction>
}

impl Encoder for Expression {
    type S = EVec<<Instruction as Encoder>::S, Byte>;

    fn emit(&self) -> Self::S {
        let instructions = self.instructions.iter().map(|instruction| instruction.emit()).collect();
        evector(instructions, byte(Instruction::END))
    }
}

// === 11 Data Section ===
pub struct DataSection {
    pub data_items: Vec<DataItem>
}

impl DataSection {
    pub const ID: SectionId = 11;
}

impl Encoder for DataSection {
    type S = Seq<Byte, Enclose<CVec<<DataItem as Encoder>::S>>>;

    fn emit(&self) -> Self::S {
        let data_items = self.data_items.iter().map(|data_item| data_item.emit()).collect();
        byte(Self::ID)
            .seq(cvector(data_items).enclose())
    }
}

pub enum DataItem {
    Active { initialize: Vec<u8>, offset_expression: Expression },
    Passive { initialize: Vec<u8> }
}

pub enum DataItemStream {
    Active(Seq<U32ToFixed40LEB128, Seq<<Expression as Encoder>::S, CVec<Byte>>>),
    Passive(Seq<U32ToFixed40LEB128, CVec<Byte>>),
}

impl ByteStream for DataItemStream {
    fn next(&mut self) -> Response {
        match self {
            Self::Active(s) => s.next(),
            Self::Passive(s) => s.next(),
        }
    }
}

impl Encoder for DataItem {
    type S = DataItemStream;
    fn emit(&self) -> Self::S {
        use DataItem::*;
        match self {
            Active { initialize, offset_expression: offset } => {
                let initialize: Vec<_> = initialize.iter().map(|b| byte(*b)).collect();
                DataItemStream::Active(U32ToFixed40LEB128::new(0).seq(offset.emit().seq(cvector(initialize))))
            },
            Passive { initialize } => {
                let initialize: Vec<_> = initialize.iter().map(|b| byte(*b)).collect();
                DataItemStream::Passive(U32ToFixed40LEB128::new(1).seq(cvector(initialize)))
            },
        }
    }
}

// === 12 Data Count Section ===
pub struct DataCountSection {
    pub count: u32,
}

impl DataCountSection {
    pub const ID: SectionId = 12;
}

impl Encoder for DataCountSection {
    type S = Seq<Byte, Enclose<U32ToFixed40LEB128>>;

    fn emit(&self) -> Self::S {
        byte(Self::ID)
            .seq(U32ToFixed40LEB128::new(self.count).enclose())
    }
}

// ====== Module =======
pub struct Module {
    pub type_section: Option<TypeSection>,
    pub import_section: Option<ImportSection>,
    pub function_section: Option<FunctionSection>,
    pub memory_section: Option<MemorySection>,
    pub globals_section: Option<GlobalsSection>,
    pub export_section: Option<ExportSection>,
    pub start_section: Option<StartSection>,
    pub data_count_section: Option<DataCountSection>,
    pub code_section: Option<CodeSection>,
    pub data_section: Option<DataSection>,
}

impl Module {
    pub const MAGIC: [u8; 4] = [0x00, 0x61, 0x73, 0x6d]; // "\0asm" encoded in UTF8
    pub const VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00]; // version 1

    pub fn empty() -> Self {
        Self {
            type_section: None,
            import_section: None,
            function_section: None,
            memory_section: None,
            globals_section: None,
            export_section: None,
            start_section: None,
            data_count_section: None,
            code_section: None,
            data_section: None,
        }
    }

}

type HeaderBytes = Seq<Bytes4, Bytes4>;

impl Encoder for Module {
    type S =
        Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<
            HeaderBytes,
            Option<<TypeSection as Encoder>::S>
        >,  Option<<ImportSection as Encoder>::S>
        >,  Option<<FunctionSection as Encoder>::S>,
        >,  Option<<MemorySection as Encoder>::S>,
        >,  Option<<GlobalsSection as Encoder>::S>,
        >,  Option<<ExportSection as Encoder>::S>,
        >,  Option<<StartSection as Encoder>::S>,
        >,  Option<<DataCountSection as Encoder>::S>,
        >,  Option<<CodeSection as Encoder>::S>,
        >,  Option<<DataSection as Encoder>::S>,
        >;

    fn emit(&self) -> Self::S {
        let header = bytes4(Module::MAGIC).seq(bytes4(Module::VERSION));

        let type_section = match &self.type_section {
            Some(type_section) => Some(type_section.emit()),
            None => None,
        };

        let import_section = match &self.import_section {
            Some(import_section) => Some(import_section.emit()),
            None => None,
        };

        let function_section = match &self.function_section {
            Some(type_indices) => Some(type_indices.emit()),
            None => None,
        };

        let memory_section = match &self.memory_section {
            Some(memory_section) => Some(memory_section.emit()),
            None => None,
        };

        let globals_section = match &self.globals_section {
            Some(globals_section) => Some(globals_section.emit()),
            None => None,
        };

        let export_section = match &self.export_section {
            Some(exports) => Some(exports.emit()),
            None => None,
        };

        let start_section = match &self.start_section {
            Some(start_section) => Some(start_section.emit()),
            None => None,
        };

        let data_count_section = match &self.data_count_section {
            Some(data_count_section) => Some(data_count_section.emit()),
            None => None,
        };

        let code_section = match &self.code_section {
            Some(code) => Some(code.emit()),
            None => None,
        };

        let data_section = match &self.data_section {
            Some(data_section) => Some(data_section.emit()),
            None => None,
        };

        header
            .seq(type_section)
            .seq(import_section)
            .seq(function_section)
            .seq(memory_section)
            .seq(globals_section)
            .seq(export_section)
            .seq(start_section)
            .seq(data_count_section)
            .seq(code_section)
            .seq(data_section)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::base::types::{ValueType, NumType};

    #[test]
    fn empty_module() {
        let module = Module::empty();

        let bytes = module.emit().to_vec();

        assert!(bytes == vec![0x00, 0x61, 0x73, 0x6d,   0x01, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn module_with_empty_type_section() {
        let mut module = Module::empty();
        module.type_section = Some(TypeSection { function_types: vec![] });

        let bytes = module.emit().to_vec();
        assert!(
            bytes ==
            vec![
                0x00, 0x61, 0x73, 0x6d,   0x01, 0x00, 0x00, 0x00,
                1,  128 + 5, 128, 128, 128, 0,      128, 128, 128, 128, 0,
            ]
        );
    }

    #[test]
    fn module_with_single_type() {
        let mut module = Module::empty();
        module.type_section = Some(TypeSection { function_types: vec![
            FunctionType { domain: vec![ ValueType::NumType(NumType::I32) ], codomain: vec![ ValueType::NumType(NumType::I32) ] },
        ]});

        let bytes = module.emit().to_vec();
        assert!(
            bytes ==
            vec![
                0x00, 0x61, 0x73, 0x6d,   0x01, 0x00, 0x00, 0x00,
                1,  128 + 18, 128, 128, 128, 0,      128 + 1, 128, 128, 128, 0,
                            0x60,
                                128 + 1, 128, 128, 128, 0,   0x7F,
                                128 + 1, 128, 128, 128, 0,   0x7F,
            ]
        );
    }

}
