use crate::binary_format::primitives::byte_stream::{ByteStream, Response, Enclose, EVec, evector, bytes4, byte, Byte, Bytes4, CVec, cvector, Seq, U32ToFixed40LEB128, I32ToSignedLEB128, I64ToSignedLEB128};
use crate::binary_format::primitives::encoder::Encoder;
use crate::binary_format::indices::IndexStream;

use crate::base::{
    types::{FunctionType, BlockType, ValueType, GlobalType},
    indices::{TypeIndex, LocalIndex, GlobalIndex, FunctionIndex, LabelIndex, Index},
    memory::Limit,
    export::Export,
    import::Import,
};

// ===Instructions===

pub enum Instruction {
    Unreachable,
    Nop,
    
    // ===Control Instructions===
    Block(BlockType, Vec<Instruction>),
    Loop(BlockType, Vec<Instruction>),
    IfThen(BlockType, Vec<Instruction>),
    IfThenElse(BlockType, Vec<Instruction>, Vec<Instruction>),

    Br(LabelIndex),
    BrIf(LabelIndex),
    Call(FunctionIndex),

    Return,

    // ===Variable Instructions====
    LocalGet(LocalIndex),
    LocalSet(LocalIndex),
    LocalTee(LocalIndex),
    GlobalGet(GlobalIndex),
    GlobalSet(GlobalIndex),

    // ===Numeric Instructions===
    // i32
    I32Const(i32),

    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,

    I32Clz,
    I32Ctz,
    I32Popcnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,

    // i64
    I64Const(i64),
    I64Add,
}

impl Instruction {
    // control
    pub const UNREACHABLE: u8 = 0x00;
    pub const NOP: u8 = 0x01;
    pub const BLOCK: u8 = 0x02;
    pub const LOOP: u8 = 0x03;
    pub const IF: u8 = 0x04;
    pub const ELSE: u8 = 0x05;

    pub const BR: u8 = 0x0c;
    pub const BR_IF: u8 = 0x0d;

    pub const RETURN: u8 = 0x0f;
    pub const CALL: u8 = 0x10;

    pub const END: u8 = 0x0B;

    // var
    pub const LOCAL_GET: u8 = 0x20;
    pub const LOCAL_SET: u8 = 0x21;
    pub const LOCAL_TEE: u8 = 0x22;
    pub const GLOBAL_GET: u8 = 0x23;
    pub const GLOBAL_SET: u8 = 0x24;

    // i32
    pub const I32_CONST: u8 = 0x41;

    pub const I32_EQZ: u8 = 0x45;
    pub const I32_EQ: u8 = 0x46;
    pub const I32_NE: u8 = 0x47;
    pub const I32_LT_S: u8 = 0x48;
    pub const I32_LT_U: u8 = 0x49;
    pub const I32_GT_S: u8 = 0x4a;
    pub const I32_GT_U: u8 = 0x4b;
    pub const I32_LE_S: u8 = 0x4c;
    pub const I32_LE_U: u8 = 0x4d;
    pub const I32_GE_S: u8 = 0x4e;
    pub const I32_GE_U: u8 = 0x4f;

    pub const I32_CLZ: u8 = 0x67;
    pub const I32_CTZ: u8 = 0x68;
    pub const I32_POPCNT: u8 = 0x69;
    pub const I32_ADD: u8 = 0x6a;
    pub const I32_SUB: u8 = 0x6b;
    pub const I32_MUL: u8 = 0x6c;
    pub const I32_DIV_S: u8 = 0x6d;
    pub const I32_DIV_U: u8 = 0x6e;
    pub const I32_REM_S: u8 = 0x6f;
    pub const I32_REM_U: u8 = 0x70;
    pub const I32_AND: u8 = 0x71;
    pub const I32_OR: u8 = 0x72;
    pub const I32_XOR: u8 = 0x73;
    pub const I32_SHL: u8 = 0x74;
    pub const I32_SHR_S: u8 = 0x75;
    pub const I32_SHR_U: u8 = 0x76;
    pub const I32_ROTL: u8 = 0x77;
    pub const I32_ROTR: u8 = 0x78;


    // i64
    pub const I64_CONST: u8 = 0x42;
    pub const I64_ADD: u8 = 0x7c;

}

pub enum InstructionStream {
    Simple(Byte),
    ConstI32(Seq<Byte, I32ToSignedLEB128>),
    ConstI64(Seq<Byte, I64ToSignedLEB128>),
    SimpleWithIndex(Seq<Byte, IndexStream>),
    // Not just a block instruction, but also if-then, loop
    BlockExpr(Seq<
        Seq<Byte, <BlockType as Encoder>::S>,
        EVec<InstructionStream, Byte>
    >),
    IfThenElseExpr(Seq<
        Seq<Byte, <BlockType as Encoder>::S>,
        Seq<EVec<InstructionStream, Byte>, EVec<InstructionStream, Byte>>
    >),
}

impl InstructionStream {
    fn simple(opcode: u8) -> Self {
        Self::Simple(byte(opcode))
    }

    fn i32_const(x: i32) -> Self {
        let s = byte(Instruction::I32_CONST).seq(I32ToSignedLEB128::new(x));
        Self::ConstI32(s)
    }

    fn i64_const(x: i64) -> Self {
        let s = byte(Instruction::I64_CONST).seq(I64ToSignedLEB128::new(x));
        Self::ConstI64(s)
    }

    fn simple_with_index<I: Index>(opcode: u8, index: I) -> Self {
        Self::SimpleWithIndex(byte(opcode).seq(index.emit()))
    }

    fn block_expr(opcode: u8, block_type: &BlockType, instructions: &[Instruction]) -> Self {
        let header = byte(opcode).seq(block_type.emit());
        let instructions = instructions.iter().map(|instruction| instruction.emit()).collect();
        let s = header.seq(evector(instructions, byte(Instruction::END)));
        Self::BlockExpr(s)
    }

    fn if_then_else_expr(block_type: &BlockType, instructions_then: &[Instruction], instructions_else: &[Instruction]) -> Self {
        let header = byte(Instruction::IF).seq(block_type.emit());
        let instructions_then = instructions_then.iter().map(|instruction| instruction.emit()).collect();
        let instructions_else = instructions_else.iter().map(|instruction| instruction.emit()).collect();
        let instructions =
            evector(instructions_then, byte(Instruction::ELSE))
                .seq(evector(instructions_else, byte(Instruction::END)));

        let s = header.seq(instructions);
        Self::IfThenElseExpr(s)
    }
}

impl ByteStream for InstructionStream {
    fn next(&mut self) -> Response {
        match self {
            Self::Simple(s) => s.next(),
            Self::ConstI32(s) => s.next(),
            Self::ConstI64(s) => s.next(),
            Self::SimpleWithIndex(s) => s.next(),
            Self::BlockExpr(s) => s.next(),
            Self::IfThenElseExpr(s) => s.next(),
        }
    }
}

impl Encoder for Instruction {
    type S = InstructionStream;

    fn emit(&self) -> Self::S {
        use Instruction::*;

        match self {
            Unreachable => InstructionStream::simple(Self::UNREACHABLE),
            Nop => InstructionStream::simple(Self::NOP),

            // ===Control Instructions===
            Block(block_type, instructions) => InstructionStream::block_expr(Instruction::BLOCK, block_type, instructions),
            Loop(block_type, instructions) => InstructionStream::block_expr(Instruction::LOOP, block_type, instructions),
            IfThen(block_type, instructions) => InstructionStream::block_expr(Instruction::IF, block_type, instructions),
            IfThenElse(block_type, instructions_then, instructions_else) => InstructionStream::if_then_else_expr(block_type, instructions_then, instructions_else),

            Br(i) => InstructionStream::simple_with_index(Self::BR, *i),
            BrIf(i) => InstructionStream::simple_with_index(Self::BR_IF, *i),
            Call(i) => InstructionStream::simple_with_index(Self::CALL, *i),
            Return => InstructionStream::simple(Self::RETURN),

            // ===Variables Instructions===
            LocalGet(i) => InstructionStream::simple_with_index(Self::LOCAL_GET, *i),
            LocalSet(i) => InstructionStream::simple_with_index(Self::LOCAL_SET, *i),
            LocalTee(i) => InstructionStream::simple_with_index(Self::LOCAL_TEE, *i),
            GlobalGet(i) => InstructionStream::simple_with_index(Self::GLOBAL_GET, *i),
            GlobalSet(i) => InstructionStream::simple_with_index(Self::GLOBAL_SET, *i),

            // ===Numeric Instructions===
            // i32
            I32Const(x) => InstructionStream::i32_const(*x),

            I32Eqz => InstructionStream::simple(Self::I32_EQZ),
            I32Eq => InstructionStream::simple(Self::I32_EQ),
            I32Ne => InstructionStream::simple(Self::I32_NE),
            I32LtS => InstructionStream::simple(Self::I32_LT_S),
            I32LtU => InstructionStream::simple(Self::I32_LT_U),
            I32GtS => InstructionStream::simple(Self::I32_GT_S),
            I32GtU => InstructionStream::simple(Self::I32_GT_U),
            I32LeS => InstructionStream::simple(Self::I32_LE_S),
            I32LeU => InstructionStream::simple(Self::I32_LE_U),
            I32GeS => InstructionStream::simple(Self::I32_GE_S),
            I32GeU => InstructionStream::simple(Self::I32_GE_U),

            I32Clz => InstructionStream::simple(Self::I32_CLZ),
            I32Ctz => InstructionStream::simple(Self::I32_CTZ),
            I32Popcnt => InstructionStream::simple(Self::I32_POPCNT),
            I32Add => InstructionStream::simple(Self::I32_ADD),
            I32Sub => InstructionStream::simple(Self::I32_SUB),
            I32Mul => InstructionStream::simple(Self::I32_MUL),
            I32DivS => InstructionStream::simple(Self::I32_DIV_S),
            I32DivU => InstructionStream::simple(Self::I32_DIV_U),
            I32RemS => InstructionStream::simple(Self::I32_REM_S),
            I32RemU => InstructionStream::simple(Self::I32_REM_U),
            I32And => InstructionStream::simple(Self::I32_AND),
            I32Or => InstructionStream::simple(Self::I32_OR),
            I32Xor => InstructionStream::simple(Self::I32_XOR),
            I32Shl => InstructionStream::simple(Self::I32_SHL),
            I32ShrS => InstructionStream::simple(Self::I32_SHR_S),
            I32ShrU => InstructionStream::simple(Self::I32_SHR_U),
            I32Rotl => InstructionStream::simple(Self::I32_ROTL),
            I32Rotr => InstructionStream::simple(Self::I32_ROTR),

            // i64
            I64Const(x) => InstructionStream::i64_const(*x),
            I64Add => InstructionStream::simple(Self::I64_ADD),

            _ => todo!(),
        }
    }
}

// =======Sections========
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
