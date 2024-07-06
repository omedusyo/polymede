#[derive(Debug, Clone)]
pub enum Token {
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Keyword(Keyword),
    Identifier(String),
    Separator(SeparatorSymbol),
    BindingSeparator,
    Nat32(u32),
    End,
}

#[derive(Debug, Copy, Clone)]
pub enum Keyword {
    Let,
    LocalLet,
    Fn,
    Eq,
    Arrow,
    Type_,
    Enum,
    Ind,
    Match,
    Fold,
    Forall,
    TypeAnnotationStart,
    TypeAnnotationSeparator,
    Function,
    Apply,
    To,
}

#[derive(Debug, Clone, Copy)]
pub enum SeparatorSymbol {
    Comma,
    Or,
    And,
}

impl SeparatorSymbol {
    pub const COMMA: char = ',';
    pub const OR: char = '|';
    pub const AND: char = '&';
}

pub const BINDING_SEPARATOR: &'static str = ".";

impl Keyword {
    pub const LET: &'static str = "let";
    pub const LOCAL_LET: &'static str = "let";
    pub const FN: &'static str = "fn";
    pub const EQ: &'static str = "=";
    pub const ARROW: &'static str = "->";
    pub const ENUM: &'static str = "enum";
    pub const IND: &'static str = "ind";
    pub const TYPE: &'static str = "type";
    pub const MATCH: &'static str = "match";
    pub const FOLD: &'static str = "fold";
    pub const TYPE_OF_VALUE: &'static str = "#";
    pub const VALUE_AFTER_TYPE: &'static str = ":";
    pub const FORALL: &'static str = "forall";
    pub const FUNCTION: &'static str = "fn";
    pub const APPLY: &'static str = "apply";
    pub const TO: &'static str = "to";

    pub fn string(&self) -> &str {
        use Keyword::*;
        match self {
            Let => Self::LET,
            LocalLet => Self::LOCAL_LET,
            Fn => Self::FN,
            Eq => Self::EQ,
            Arrow => Self::ARROW,
            Enum => Self::ENUM,
            Ind => Self::IND,
            Type_ => Self::TYPE,
            Match => Self::MATCH,
            Fold => Self::FOLD,
            Forall => Self::FORALL,
            TypeAnnotationStart => Self::TYPE_OF_VALUE,
            TypeAnnotationSeparator => Self::VALUE_AFTER_TYPE,
            Function => Self::FUNCTION,
            Apply => Self::APPLY,
            To => Self::TO,
        }
    }
}
