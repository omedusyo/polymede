use std::marker::PhantomData;
use bimap::BiMap;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct TypeName(usize);
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
struct ConstructorName(usize);
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
struct FunctionName(usize);
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
struct VarName(usize);
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
struct FieldName(usize);
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct TypeVar(usize);

// TODO: This should actually be a bidirectional table.
struct NameTable<Identifier> {
    _phantom_name_type: PhantomData<Identifier>,

    // TODO: change this to
    // BiHashMap<String, usize>
    names: Vec<String>,
}

// TODO: I need to be able to convert the identifier to `usize` so I can index the vector
impl <Identifier> NameTable<Identifier> 
where Identifier: Into<usize> + From<usize>
{
    fn name(&self, identifier: Identifier) -> Option<String> {
        self.names.get(identifier.into()).cloned()
    }

    fn fresh_identifier(&mut self, name: String) -> Identifier {
        let id: Identifier = self.names.len().into();
        self.names.push(name);
        id
    }

    // lookup(name: String) -> Option<Identifier>
}

type FunctionNameTable = NameTable<FunctionName>;
type TypeNameTable = NameTable<TypeName>;
type ConstructorNameTable = NameTable<ConstructorName>;
type VarNameTable = NameTable<VarName>; // For locals variables

// ===Types===

struct TypeDeclaration  {
    name: TypeName,
    number_of_parameters: usize,
    type_module: TypeModule,
}

enum TypeModule {
    Enum(EnumModule),
    Inductive(InductiveModule),
    Structure(StructureModule),
}

struct EnumModule {
    constructors: Vec<EnumConstructor>,
}

struct EnumConstructor {
    name: ConstructorName,
    number_of_parameters: usize,
    parameters: Vec<Type>,
}

struct InductiveModule {
    // TODO: You need to do some sort of positivity check, right?
    //       Can't just have a lambda.
    //       Maybe just disallow inductive definitions with lambdas in them.
    constructors: Vec<EnumConstructor>,
}

struct StructureModule {
    fields: Vec<Field>,
}

struct Field {
    field_name: FieldName,
    type_: Type,
}

pub enum Type {
    Primitive(PrimitiveType),
    VarUse(TypeVar),
    TypeApplication(TypeName, Vec<Type>),
    Arrow(Vec<Type>, Box<Type>),
    Command(Box<Type>), // TODO: Do I allow closures to be embedded in commands? Maybe I need to
                        // introduce Value/Computation type distinction?
}

pub enum PrimitiveType {
    Int32,
}

// ===Terms===
struct FunctionDeclaration {
    name: FunctionName,
    type_: FunctionType,
    number_of_parameters: usize, // TODO: This should be obvious from the `type_`
                                 // TODO: Where should I store the actual string names?
    body: Term,
}

struct FunctionType {
    input_types: Vec<Type>,
    output_type: Type,
}

enum Term {
    Primitive(PrimitiveTerm),
    FunctionApplication(FunctionName, Vec<Term>),
    VarUse(VarName),
    Constructor(ConstructorName, Vec<Term>),
    // TODO
    // Let(, Term),
    // pattern matching
    Match(Box<Term>, Vec<MatchBranch>),
    Fold(Box<Term>, Vec<FoldBranch>),
    // struct
    Projection(Box<Term>, FieldName),
    // anonymous functions
    Lambda(Box<LambdaTerm>),
    LambdaApplication(Box<Term>, Vec<Term>),
}

struct LambdaTerm {
    number_of_parameters: usize,
    type_: FunctionType,
    body: Term,
}

enum PrimitiveTerm {
    I32(i32),
}

struct MatchBranch {
    pattern: Pattern,
    body: Term,
}

pub struct FoldBranch {
    pattern: Pattern,
    body: Term,
}

pub enum Pattern {
    Constructor(ConstructorName, Vec<Pattern>),
    Var(VarName),
    Anything,
}



// ===Program===
pub struct Program {
    function_names: FunctionNameTable,
    type_names: TypeNameTable,
    constructor_names: ConstructorNameTable,
    type_declarations: Vec<TypeDeclaration>,
    function_declaration: Vec<FunctionDeclaration>,
}

