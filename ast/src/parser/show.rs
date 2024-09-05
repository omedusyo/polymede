use crate::base::{
    ConstructorDefinition, EnumDefinition, FunctionDefinition, FunctionType, IndDefinition,
    Program, RunDefinition, Type, TypeDefinition,
};
use crate::identifier::{Identifier, Interner};

pub struct Show<'a> {
    interner: &'a Interner,
}

impl<'show> Show<'show> {
    pub fn new<'a: 'show>(interner: &'a Interner) -> Show<'show> {
        Self { interner }
    }

    fn interner(&self) -> &Interner {
        self.interner
    }

    pub fn show_program_definitions(&self, program: &Program) -> String {
        let type_definitions = program
            .type_definition_in_source_ordering()
            .iter()
            .map(|def| self.show_type_definition(def))
            .collect::<Vec<_>>()
            .join("\n\n");
        let function_definitions = program
            .function_definitions_in_source_ordering()
            .iter()
            .map(|def| self.show_function_definition(def))
            .collect::<Vec<_>>()
            .join("\n");
        let run_definition = program
            .run_definition
            .iter()
            .map(|def| self.show_run_definition(def))
            .collect::<Vec<_>>()
            .join("\n");
        format!("{type_definitions}\n\n{function_definitions}\n{run_definition}")
    }

    fn show_type_definition(&self, type_definition: &TypeDefinition) -> String {
        use TypeDefinition::*;
        match type_definition {
            Enum(enum_definition) => self.show_enum_definition(enum_definition),
            Ind(ind_definition) => self.show_ind_definition(ind_definition),
        }
    }

    fn show_enum_definition(&self, definition: &EnumDefinition) -> String {
        let name_str = definition.name.str(self.interner());
        let constructor_strs = definition
            .constructors_in_source_ordering()
            .iter()
            .map(|def| self.show_constructor_definition(def))
            .collect::<Vec<_>>()
            .join("\n");
        let after_equals_str = format!("enum {{\n{constructor_strs}\n}}");
        if definition.type_parameters.is_empty() {
            format!("type {name_str} = {after_equals_str}")
        } else {
            let parameter_strs = self.show_sequence_of_identifiers(&definition.type_parameters);
            format!("type {name_str}({parameter_strs}) = {after_equals_str}")
        }
    }

    fn show_ind_definition(&self, definition: &IndDefinition) -> String {
        let name_str = definition.name.str(self.interner());
        let rec_var_str = definition.recursive_type_var.str(self.interner());
        let constructor_strs = definition
            .constructors_in_source_ordering()
            .iter()
            .map(|def| self.show_constructor_definition(def))
            .collect::<Vec<_>>()
            .join("\n");
        let after_equals_str = format!("ind {{ {rec_var_str} .\n{constructor_strs}\n}}");
        if definition.type_parameters.is_empty() {
            format!("type {name_str} = {after_equals_str}")
        } else {
            let parameter_strs = self.show_sequence_of_identifiers(&definition.type_parameters);
            format!("type {name_str}({parameter_strs}) = {after_equals_str}")
        }
    }

    fn show_constructor_definition(
        &self,
        constructor_definition: &ConstructorDefinition,
    ) -> String {
        let name_str = constructor_definition.name.str(self.interner());
        if constructor_definition.parameters.is_empty() {
            format!("| {name_str}")
        } else {
            let parameter_strs = constructor_definition
                .parameters
                .iter()
                .map(|type_| self.show_type(type_))
                .collect::<Vec<_>>()
                .join(", ");
            format!("| {name_str}({parameter_strs})")
        }
    }

    fn show_sequence_of_identifiers<I: Identifier>(&self, identifiers: &[I]) -> String {
        identifiers
            .iter()
            .map(|id| id.str(self.interner()).to_string())
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn show_function_definition(&self, definition: &FunctionDefinition) -> String {
        let fn_name = definition.name();
        let name_str = fn_name.str(self.interner());
        match definition {
            FunctionDefinition::User(definition) => {
                let type_ = self.show_function_type(&definition.function.type_);
                if definition.type_parameters.is_empty() {
                    format!("fn {name_str} : {type_} ")
                } else {
                    let type_var_strs =
                        self.show_sequence_of_identifiers(&definition.type_parameters);
                    format!("fn {name_str} : forall {{ {type_var_strs} . {type_} }}")
                }
            }
            FunctionDefinition::Foreign(definition) => {
                let type_ = self.show_function_type(&definition.type_);
                format!("fn {name_str} : {type_} ")
            }
        }
    }

    fn show_run_definition(&self, definition: &RunDefinition) -> String {
        let type_ = self.show_type(&definition.body.type_);
        format!("run {type_} ")
    }

    pub fn show_type(&self, type_: &Type) -> String {
        use Type::*;
        match type_ {
            VariableUse(variable) => variable.str(self.interner()).to_string(),
            TypeApplication(constructor_name, types) => {
                let constructor_name_str = constructor_name.str(self.interner());
                if types.is_empty() {
                    constructor_name_str.to_string()
                } else {
                    let type_strs = types
                        .iter()
                        .map(|type_| self.show_type(type_))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{constructor_name_str}({type_strs})")
                }
            }
            Arrow(function_type) => {
                let fn_type_str = self.show_function_type(function_type);
                format!("Fn({fn_type_str})")
            }
            I32 => "I32".to_string(),
            F32 => "F32".to_string(),
            String => "String".to_string(),
            Command(type_) => format!("Cmd({})", self.show_type(type_)),
        }
    }

    fn show_function_type(&self, function_type: &FunctionType) -> String {
        let input_types = function_type
            .input_types
            .iter()
            .map(|type_| self.show_type(type_))
            .collect::<Vec<_>>()
            .join(", ");
        let output_type = self.show_type(&function_type.output_type);
        format!("{input_types} -> {output_type}")
    }

    pub fn show_identifier<I: Identifier>(&self, var: &I) -> String {
        format!(
            "{}@(line={}, column={})",
            var.str(self.interner),
            var.position().line,
            var.position().column
        )
    }
}
