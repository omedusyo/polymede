use crate::binary_format::indices::IndexStream;
use crate::binary_format::primitives::byte_stream::{
    byte, bytes, bytes4, cvector, evector, string, vector, Byte, ByteStream, Bytes, Bytes4, CVec,
    EVec, Enclose, Response, Seq, U32ToFixed40LEB128, Vector, UTF8,
};
use crate::binary_format::primitives::encoder::Encoder;

use crate::base::{
    export::Export,
    import::Import,
    indices::{FunctionIndex, TypeIndex},
    instructions::Instruction,
    memory::Limit,
    types::{FunctionType, GlobalType, RefType, ValueType},
};

type SectionId = u8;

// === 0 Custom Section ===
#[derive(Debug)]
pub struct CustomSection {
    pub name: String,
    pub bytes: Vec<u8>,
}

impl CustomSection {
    pub const ID: SectionId = 0;
}

impl Encoder for CustomSection {
    type S = Seq<Byte, Enclose<Seq<UTF8, Bytes>>>;
    fn emit(&self) -> Self::S {
        byte(Self::ID)
            // TODO: Can we get rid of the cloning of bytes?
            .seq((string(&self.name).seq(bytes(self.bytes.to_vec()))).enclose())
    }
}

// === 1 Type Section ===
#[derive(Debug)]
pub struct TypeSection {
    pub function_types: Vec<FunctionType>,
}

impl TypeSection {
    pub const ID: SectionId = 1;
}

impl Encoder for TypeSection {
    type S = Seq<Byte, Enclose<CVec<<FunctionType as Encoder>::S>>>;
    fn emit(&self) -> Self::S {
        let fn_types = self
            .function_types
            .iter()
            .map(|fn_type| fn_type.emit())
            .collect();
        byte(Self::ID).seq(cvector(fn_types).enclose())
    }
}

// === 2 Import Section ===
#[derive(Debug)]
pub struct ImportSection {
    pub imports: Vec<Import>,
}

impl ImportSection {
    pub const ID: SectionId = 2;
}

impl Encoder for ImportSection {
    type S = Seq<Byte, Enclose<CVec<<Import as Encoder>::S>>>;
    fn emit(&self) -> Self::S {
        let imports = self.imports.iter().map(|import| import.emit()).collect();
        byte(Self::ID).seq(cvector(imports).enclose())
    }
}

// === 3 Function Section ===
#[derive(Debug)]
pub struct FunctionSection {
    pub type_indices: Vec<TypeIndex>,
}

impl FunctionSection {
    pub const ID: SectionId = 3;
}

impl Encoder for FunctionSection {
    type S = Seq<Byte, Enclose<CVec<<TypeIndex as Encoder>::S>>>;

    fn emit(&self) -> Self::S {
        let type_indices = self
            .type_indices
            .iter()
            .map(|type_index| type_index.emit())
            .collect();
        byte(Self::ID).seq(cvector(type_indices).enclose())
    }
}

// === 4 Table Section ===
#[derive(Debug)]
pub struct TableSection {
    pub table_types: Vec<TableType>,
}

impl TableSection {
    pub const ID: SectionId = 4;
}

impl Encoder for TableSection {
    type S = Seq<Byte, Enclose<CVec<<TableType as Encoder>::S>>>;

    fn emit(&self) -> Self::S {
        let table_types = self
            .table_types
            .iter()
            .map(|table_type| table_type.emit())
            .collect();
        byte(Self::ID).seq(cvector(table_types).enclose())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct TableType {
    pub reftype: RefType,
    pub limit: Limit,
}

impl Encoder for TableType {
    type S = Seq<<RefType as Encoder>::S, <Limit as Encoder>::S>;

    fn emit(&self) -> Self::S {
        self.reftype.emit().seq(self.limit.emit())
    }
}

// === 5 Memory Section ===
#[derive(Debug)]
pub struct MemorySection {
    pub memory_types: Vec<Limit>,
}

impl MemorySection {
    pub const ID: SectionId = 5;
}

impl Encoder for MemorySection {
    type S = Seq<Byte, Enclose<CVec<<Limit as Encoder>::S>>>;

    fn emit(&self) -> Self::S {
        let memory_types = self.memory_types.iter().map(|limit| limit.emit()).collect();
        byte(Self::ID).seq(cvector(memory_types).enclose())
    }
}

// === 6 Globals Section ===
#[derive(Debug)]
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
        byte(Self::ID).seq(cvector(globals).enclose())
    }
}

#[derive(Debug)]
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
#[derive(Debug)]
pub struct ExportSection {
    pub exports: Vec<Export>,
}

impl ExportSection {
    pub const ID: SectionId = 7;
}

impl Encoder for ExportSection {
    type S = Seq<Byte, Enclose<CVec<<Export as Encoder>::S>>>;

    fn emit(&self) -> Self::S {
        let exports = self.exports.iter().map(|export| export.emit()).collect();
        byte(Self::ID).seq(cvector(exports).enclose())
    }
}

// === 8 Start Section ===
#[derive(Debug)]
pub struct StartSection {
    pub start: FunctionIndex,
}

impl StartSection {
    pub const ID: SectionId = 8;
}

impl Encoder for StartSection {
    type S = Seq<Byte, Enclose<IndexStream>>;

    fn emit(&self) -> Self::S {
        byte(Self::ID).seq(self.start.emit().enclose())
    }
}

// === 9 Element Section ===
#[derive(Debug)]
pub struct ElementSection {
    pub elements: Vec<Element>,
}

impl ElementSection {
    pub const ID: SectionId = 9;
}

impl Encoder for ElementSection {
    type S = Seq<Byte, Enclose<CVec<<Element as Encoder>::S>>>;

    fn emit(&self) -> Self::S {
        let elements = self.elements.iter().map(|element| element.emit()).collect();
        byte(Self::ID).seq(cvector(elements).enclose())
    }
}

#[derive(Debug)]
pub struct Element {
    pub offset_expression: Expression,
    pub function_references: Vec<FunctionIndex>,
}

impl Encoder for Element {
    type S = Seq<
        U32ToFixed40LEB128,
        Seq<<Expression as Encoder>::S, CVec<<FunctionIndex as Encoder>::S>>,
    >;

    fn emit(&self) -> Self::S {
        let fn_references = self
            .function_references
            .iter()
            .map(|fn_ref| fn_ref.emit())
            .collect();
        U32ToFixed40LEB128::new(0).seq(self.offset_expression.emit().seq(cvector(fn_references)))
    }
}

// === 10 Code Section ===
#[derive(Debug)]
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
        byte(Self::ID).seq(cvector(codes).enclose())
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Expression {
    pub instructions: Vec<Instruction>,
}

impl Encoder for Expression {
    type S = EVec<<Instruction as Encoder>::S, Byte>;

    fn emit(&self) -> Self::S {
        let instructions = self
            .instructions
            .iter()
            .map(|instruction| instruction.emit())
            .collect();
        evector(instructions, byte(Instruction::END))
    }
}

// === 11 Data Section ===
#[derive(Debug)]
pub struct DataSection {
    pub data_items: Vec<DataItem>,
}

impl DataSection {
    pub const ID: SectionId = 11;
}

impl Encoder for DataSection {
    type S = Seq<Byte, Enclose<CVec<<DataItem as Encoder>::S>>>;

    fn emit(&self) -> Self::S {
        let data_items = self
            .data_items
            .iter()
            .map(|data_item| data_item.emit())
            .collect();
        byte(Self::ID).seq(cvector(data_items).enclose())
    }
}

#[derive(Debug)]
pub enum DataItem {
    Active {
        initialize: Vec<u8>,
        offset_expression: Expression,
    },
    Passive {
        initialize: Vec<u8>,
    },
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
            Active {
                initialize,
                offset_expression: offset,
            } => {
                let initialize: Vec<_> = initialize.iter().map(|b| byte(*b)).collect();
                DataItemStream::Active(
                    U32ToFixed40LEB128::new(0).seq(offset.emit().seq(cvector(initialize))),
                )
            }
            Passive { initialize } => {
                let initialize: Vec<_> = initialize.iter().map(|b| byte(*b)).collect();
                DataItemStream::Passive(U32ToFixed40LEB128::new(1).seq(cvector(initialize)))
            }
        }
    }
}

// === 12 Data Count Section ===
#[derive(Debug)]
pub struct DataCountSection {
    pub count: u32,
}

impl DataCountSection {
    pub const ID: SectionId = 12;
}

impl Encoder for DataCountSection {
    type S = Seq<Byte, Enclose<U32ToFixed40LEB128>>;

    fn emit(&self) -> Self::S {
        byte(Self::ID).seq(U32ToFixed40LEB128::new(self.count).enclose())
    }
}

// ====== Module =======
#[derive(Debug)]
pub struct Module {
    pub custom_section_before_type_section: Vec<CustomSection>,
    pub type_section: Option<TypeSection>,
    pub custom_section_before_import_section: Vec<CustomSection>,
    pub import_section: Option<ImportSection>,
    pub custom_section_before_function_section: Vec<CustomSection>,
    pub function_section: Option<FunctionSection>,
    pub custom_section_before_table_section: Vec<CustomSection>,
    pub table_section: Option<TableSection>,
    pub custom_section_before_memory_section: Vec<CustomSection>,
    pub memory_section: Option<MemorySection>,
    pub custom_section_before_globals_section: Vec<CustomSection>,
    pub globals_section: Option<GlobalsSection>,
    pub custom_section_before_export_section: Vec<CustomSection>,
    pub export_section: Option<ExportSection>,
    pub custom_section_before_start_section: Vec<CustomSection>,
    pub start_section: Option<StartSection>,
    pub custom_section_before_data_count_section: Vec<CustomSection>,
    pub data_count_section: Option<DataCountSection>,
    pub custom_section_before_element_section: Vec<CustomSection>,
    pub element_section: Option<ElementSection>,
    pub custom_section_before_code_section: Vec<CustomSection>,
    pub code_section: Option<CodeSection>,
    pub custom_section_before_data_section: Vec<CustomSection>,
    pub data_section: Option<DataSection>,
    pub custom_section_at_the_end: Vec<CustomSection>,
}

impl Module {
    pub const MAGIC: [u8; 4] = [0x00, 0x61, 0x73, 0x6d]; // "\0asm" encoded in UTF8
    pub const VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00]; // version 1

    pub fn empty() -> Self {
        Self {
            custom_section_before_type_section: vec![],
            type_section: None,
            custom_section_before_import_section: vec![],
            import_section: None,
            custom_section_before_function_section: vec![],
            function_section: None,
            custom_section_before_table_section: vec![],
            table_section: None,
            custom_section_before_memory_section: vec![],
            memory_section: None,
            custom_section_before_globals_section: vec![],
            globals_section: None,
            custom_section_before_export_section: vec![],
            export_section: None,
            custom_section_before_start_section: vec![],
            start_section: None,
            custom_section_before_data_count_section: vec![],
            data_count_section: None,
            custom_section_before_element_section: vec![],
            element_section: None,
            custom_section_before_code_section: vec![],
            code_section: None,
            custom_section_before_data_section: vec![],
            data_section: None,
            custom_section_at_the_end: vec![],
        }
    }
}

type HeaderBytes = Seq<Bytes4, Bytes4>;

impl Encoder for Module {
    type S =
        Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<Seq<
            HeaderBytes,
            Vector<<CustomSection as Encoder>::S>
        >,  Option<<TypeSection as Encoder>::S>
        >,  Vector<<CustomSection as Encoder>::S>
        >,  Option<<ImportSection as Encoder>::S>
        >,  Vector<<CustomSection as Encoder>::S>
        >,  Option<<FunctionSection as Encoder>::S>,
        >,  Vector<<CustomSection as Encoder>::S>
        >,  Option<<TableSection as Encoder>::S>,
        >,  Vector<<CustomSection as Encoder>::S>
        >,  Option<<MemorySection as Encoder>::S>,
        >,  Vector<<CustomSection as Encoder>::S>
        >,  Option<<GlobalsSection as Encoder>::S>,
        >,  Vector<<CustomSection as Encoder>::S>
        >,  Option<<ExportSection as Encoder>::S>,
        >,  Vector<<CustomSection as Encoder>::S>
        >,  Option<<StartSection as Encoder>::S>,
        >,  Vector<<CustomSection as Encoder>::S>
        >,  Option<<ElementSection as Encoder>::S>,
        >,  Vector<<CustomSection as Encoder>::S>
        >,  Option<<DataCountSection as Encoder>::S>,
        >,  Vector<<CustomSection as Encoder>::S>
        >,  Option<<CodeSection as Encoder>::S>,
        >,  Vector<<CustomSection as Encoder>::S>
        >,  Option<<DataSection as Encoder>::S>,
        >,  Vector<<CustomSection as Encoder>::S>
        >;

    fn emit(&self) -> Self::S {
        let header = bytes4(Module::MAGIC).seq(bytes4(Module::VERSION));

        let custom_section_before_type_section = vector(
            self.custom_section_before_type_section
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        let type_section = self
            .type_section
            .as_ref()
            .map(|type_section| type_section.emit());

        let custom_section_before_import_section = vector(
            self.custom_section_before_import_section
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        let import_section = self
            .import_section
            .as_ref()
            .map(|import_section| import_section.emit());

        let custom_section_before_function_section = vector(
            self.custom_section_before_function_section
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        let function_section = self
            .function_section
            .as_ref()
            .map(|type_indices| type_indices.emit());

        let custom_section_before_table_section = vector(
            self.custom_section_before_table_section
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        let table_section = self
            .table_section
            .as_ref()
            .map(|table_section| table_section.emit());

        let custom_section_before_memory_section = vector(
            self.custom_section_before_memory_section
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        let memory_section = self
            .memory_section
            .as_ref()
            .map(|memory_section| memory_section.emit());

        let custom_section_before_globals_section = vector(
            self.custom_section_before_globals_section
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        let globals_section = self
            .globals_section
            .as_ref()
            .map(|globals_section| globals_section.emit());

        let custom_section_before_export_section = vector(
            self.custom_section_before_export_section
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        let export_section = self.export_section.as_ref().map(|exports| exports.emit());

        let custom_section_before_start_section = vector(
            self.custom_section_before_start_section
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        let start_section = self
            .start_section
            .as_ref()
            .map(|start_section| start_section.emit());

        let custom_section_before_element_section = vector(
            self.custom_section_before_element_section
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        let element_section = self.element_section.as_ref().map(|element| element.emit());

        let custom_section_before_data_count_section = vector(
            self.custom_section_before_data_count_section
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        let data_count_section = self
            .data_count_section
            .as_ref()
            .map(|data_count_section| data_count_section.emit());

        let custom_section_before_code_section = vector(
            self.custom_section_before_code_section
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        let code_section = self.code_section.as_ref().map(|code| code.emit());

        let custom_section_before_data_section = vector(
            self.custom_section_before_data_section
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        let data_section = self
            .data_section
            .as_ref()
            .map(|data_section| data_section.emit());

        let custom_section_at_the_end = vector(
            self.custom_section_at_the_end
                .iter()
                .map(|custom_section| custom_section.emit())
                .collect(),
        );

        header
            .seq(custom_section_before_type_section)
            .seq(type_section)
            .seq(custom_section_before_import_section)
            .seq(import_section)
            .seq(custom_section_before_function_section)
            .seq(function_section)
            .seq(custom_section_before_table_section)
            .seq(table_section)
            .seq(custom_section_before_memory_section)
            .seq(memory_section)
            .seq(custom_section_before_globals_section)
            .seq(globals_section)
            .seq(custom_section_before_export_section)
            .seq(export_section)
            .seq(custom_section_before_start_section)
            .seq(start_section)
            .seq(custom_section_before_element_section)
            .seq(element_section)
            .seq(custom_section_before_data_count_section)
            .seq(data_count_section)
            .seq(custom_section_before_code_section)
            .seq(code_section)
            .seq(custom_section_before_data_section)
            .seq(data_section)
            .seq(custom_section_at_the_end)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::base::types::{NumType, ValueType};

    #[test]
    fn empty_module() {
        let module = Module::empty();

        let bytes = module.emit().to_vec();

        assert!(bytes == vec![0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn module_with_empty_type_section() {
        let mut module = Module::empty();
        module.type_section = Some(TypeSection {
            function_types: vec![],
        });

        let bytes = module.emit().to_vec();
        assert!(
            bytes
                == vec![
                    0x00,
                    0x61,
                    0x73,
                    0x6d,
                    0x01,
                    0x00,
                    0x00,
                    0x00,
                    1,
                    128 + 5,
                    128,
                    128,
                    128,
                    0,
                    128,
                    128,
                    128,
                    128,
                    0,
                ]
        );
    }

    #[test]
    fn module_with_single_type() {
        let mut module = Module::empty();
        module.type_section = Some(TypeSection {
            function_types: vec![FunctionType {
                domain: vec![ValueType::NumType(NumType::I32)],
                codomain: vec![ValueType::NumType(NumType::I32)],
            }],
        });

        let bytes = module.emit().to_vec();
        assert!(
            bytes
                == vec![
                    0x00,
                    0x61,
                    0x73,
                    0x6d,
                    0x01,
                    0x00,
                    0x00,
                    0x00,
                    1,
                    128 + 18,
                    128,
                    128,
                    128,
                    0,
                    128 + 1,
                    128,
                    128,
                    128,
                    0,
                    0x60,
                    128 + 1,
                    128,
                    128,
                    128,
                    0,
                    0x7F,
                    128 + 1,
                    128,
                    128,
                    128,
                    0,
                    0x7F,
                ]
        );
    }
}
