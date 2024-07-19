use crate::base::{Program, Type, FunctionType, TypeDeclaration, FunctionDeclaration, RunDeclaration, EnumDeclaration, IndDeclaration, ConstructorDeclaration};
use crate::identifier::{Identifier, Interner};

pub struct Show<'a> {
    interner: &'a Interner,
}

impl <'show>Show<'show> {
    pub fn new<'a: 'show>(interner: &'a Interner) -> Show<'show> {
        Self { interner }
    }

    fn interner(&self) -> &Interner {
        self.interner
    }

    pub fn show_program_declarations(&self, program: &Program) -> String {
        let type_declarations =  program.type_declarations_in_source_ordering().iter().map(|decl| self.show_type_declaration(decl)).collect::<Vec<_>>().join("\n\n");
        let function_declarations = program.function_declarations_in_source_ordering().iter().map(|decl| self.show_function_declaration(decl)).collect::<Vec<_>>().join("\n");
        let run_declaration = program.run_declaration.iter().map(|decl| self.show_run_declaration(decl)).collect::<Vec<_>>().join("\n");
        format!("{type_declarations}\n\n{function_declarations}\n{run_declaration}")
    }

    fn show_type_declaration(&self, type_declaration: &TypeDeclaration) -> String {
        use TypeDeclaration::*;
        match type_declaration {
            Enum(enum_declaration) => self.show_enum_declaration(enum_declaration),
            Ind(ind_declaration) => self.show_ind_declaration(ind_declaration),
        }
    }

    fn show_enum_declaration(&self, declaration: &EnumDeclaration) -> String {
        let name_str = declaration.name.str(self.interner());
        let constructor_strs = declaration.constructors_in_source_ordering().iter().map(|decl| self.show_constructor_declaration(decl)).collect::<Vec<_>>().join("\n");
        let after_equals_str = format!("enum {{\n{constructor_strs}\n}}");
        if declaration.type_parameters.is_empty() {
            format!("type {name_str} = {after_equals_str}")
        } else {
            let parameter_strs = self.show_sequence_of_identifiers(&declaration.type_parameters);
            format!("type {name_str}({parameter_strs}) = {after_equals_str}")
        }
    }

    fn show_ind_declaration(&self, declaration: &IndDeclaration) -> String {
        let name_str = declaration.name.str(self.interner());
        let rec_var_str = declaration.recursive_type_var.str(self.interner());
        let constructor_strs = declaration.constructors_in_source_ordering().iter().map(|decl| self.show_constructor_declaration(decl)).collect::<Vec<_>>().join("\n");
        let after_equals_str = format!("ind {{ {rec_var_str} .\n{constructor_strs}\n}}");
        if declaration.type_parameters.is_empty() {
            format!("type {name_str} = {after_equals_str}")
        } else {
            let parameter_strs = self.show_sequence_of_identifiers(&declaration.type_parameters);
            format!("type {name_str}({parameter_strs}) = {after_equals_str}")
        }
    }

    fn show_constructor_declaration(&self, constructor_declaration: &ConstructorDeclaration) -> String {
        let name_str = constructor_declaration.name.str(self.interner());
        if constructor_declaration.parameters.is_empty() {
            format!("| {name_str}")
        } else {
            let parameter_strs = constructor_declaration.parameters.iter().map(|type_| self.show_type(type_)).collect::<Vec<_>>().join(", ");
            format!("| {name_str}({parameter_strs})")
        }
    }

    fn show_sequence_of_identifiers(&self, identifiers: &[Identifier]) -> String {
        identifiers.iter().map(|id| id.str(self.interner()).to_string()).collect::<Vec<_>>().join(", ")
    }

    fn show_function_declaration(&self, declaration: &FunctionDeclaration) -> String {
        let name_str = declaration.name.str(self.interner());
        let type_ = self.show_function_type(&declaration.function.type_);
        if declaration.type_parameters.is_empty() {
            format!("fn {name_str} : {type_} ")
        } else {
            let type_var_strs = self.show_sequence_of_identifiers(&declaration.type_parameters);
            format!("fn {name_str} : forall {{ {type_var_strs} . {type_} }}")
        }
    }

    fn show_run_declaration(&self, declaration: &RunDeclaration) -> String {
        let type_ = self.show_type(&declaration.body.type_);
        format!("run {type_} ")
    }

    pub fn show_type(&self, type_: &Type) -> String {
        use Type::*;
        match type_ {
            VariableUse(variable) => variable.str(self.interner()).to_string(),
            TypeApplication(constructor_name, types) => {
                let constructor_name_str = constructor_name.str(self.interner());
                if types.is_empty() {
                    format!("{constructor_name_str}")
                } else {
                    let type_strs = types.into_iter().map(|type_| self.show_type(type_)).collect::<Vec<_>>().join(", ");
                    format!("{constructor_name_str}({type_strs})")
                }
            },
            Arrow(function_type) => {
                let fn_type_str = self.show_function_type(&*function_type);
                format!("Fn({fn_type_str})")
            },
        }
    }

    fn show_function_type(&self, function_type: &FunctionType) -> String {
        let input_types = function_type.input_types.iter().map(|type_| self.show_type(type_)).collect::<Vec<_>>().join(", ");
        let output_type = self.show_type(&function_type.output_type);
        format!("{input_types} -> {output_type}")
    }

    pub fn show_identifier(&self, var: &Identifier) -> String {
        var.str(self.interner).to_string()
    }
}
