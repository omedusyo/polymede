use crate::parser::{
    base::{Program, Type, FunctionType, TypeDeclaration, FunctionDeclaration, LetDeclaration, EnumDeclaration, IndDeclaration, ConstructorDeclaration},
    identifier::Identifier,
};

pub fn show_program_declarations(program: Program) -> String {
    let type_declarations = program.type_declarations.into_iter().map(show_type_declaration).collect::<Vec<_>>().join("\n");
    let function_declarations = program.function_declarations.into_iter().map(show_function_declaration).collect::<Vec<_>>().join("\n");
    let let_declarations = program.let_declarations.into_iter().map(show_let_declaration).collect::<Vec<_>>().join("\n");
    format!("{type_declarations}\n{function_declarations}\n{let_declarations}")
}

fn show_type_declaration(type_declaration: TypeDeclaration) -> String {
    use TypeDeclaration::*;
    match type_declaration {
        Enum(enum_declaration) => show_enum_declaration(enum_declaration),
        Ind(ind_declaration) => show_ind_declaration(ind_declaration),
    }
}

fn show_enum_declaration(declaration: EnumDeclaration) -> String {
    let name_str = declaration.name.str();
    let constructor_strs = declaration.constructors.into_iter().map(show_constructor_declaration).collect::<Vec<_>>().join("\n");
    let after_equals_str = format!("enum {{\n{constructor_strs}\n}}");
    if declaration.type_parameters.is_empty() {
        format!("type {name_str} = {after_equals_str}")
    } else {
        let parameter_strs = show_sequence_of_identifiers(declaration.type_parameters);
        format!("type {name_str}({parameter_strs}) = {after_equals_str}")
    }
}

fn show_ind_declaration(declaration: IndDeclaration) -> String {
    let name_str = declaration.name.str();
    let rec_var_str = declaration.recursive_type_var.str();
    let constructor_strs = declaration.constructors.into_iter().map(show_constructor_declaration).collect::<Vec<_>>().join("\n");
    let after_equals_str = format!("ind {{ {rec_var_str} .\n{constructor_strs}\n}}");
    if declaration.type_parameters.is_empty() {
        format!("type {name_str} = {after_equals_str}")
    } else {
        let parameter_strs = show_sequence_of_identifiers(declaration.type_parameters);
        format!("type {name_str}({parameter_strs}) = {after_equals_str}")
    }
}

fn show_constructor_declaration(constructor_declaration: ConstructorDeclaration) -> String {
    let name_str = constructor_declaration.name.str();
    if constructor_declaration.parameters.is_empty() {
        format!("| {name_str}")
    } else {
        let parameter_strs = constructor_declaration.parameters.into_iter().map(show_type).collect::<Vec<_>>().join(", ");
        format!("| {name_str}({parameter_strs})")
    }
}

fn show_sequence_of_identifiers(identifiers: Vec<Identifier>) -> String {
    identifiers.into_iter().map(|id: Identifier| id.str().to_string()).collect::<Vec<_>>().join(", ")
}

fn show_function_declaration(declaration: FunctionDeclaration) -> String {
    let name_str = declaration.name.str();
    let type_ = show_function_type(declaration.function.type_);
    if declaration.type_parameters.is_empty() {
        format!("fn {name_str} : {type_} ")
    } else {
        let type_var_strs = show_sequence_of_identifiers(declaration.type_parameters);
        format!("fn {name_str} : forall {{ {type_var_strs} . {type_} }}")
    }
}

fn show_let_declaration(declaration: LetDeclaration) -> String {
    let name_str = declaration.name.str();
    let type_ = show_type(declaration.body.type_);
    if declaration.type_parameters.is_empty() {
        format!("let {name_str} : {type_} ")
    } else {
        let type_var_strs = show_sequence_of_identifiers(declaration.type_parameters);
        format!("let {name_str} : forall {{ {type_var_strs} . {type_} }}")
    }
}

fn show_type(type_: Type) -> String {
    use Type::*;
    match type_ {
        VariableUse(variable) => variable.str().to_string(),
        TypeApplication(constructor_name, types) => {
            let constructor_name_str = constructor_name.str();
            if types.is_empty() {
                format!("{constructor_name_str}")
            } else {
                let type_strs = types.into_iter().map(show_type).collect::<Vec<_>>().join(", ");
                format!("{constructor_name_str}({type_strs})")
            }
        },
        Arrow(function_type) => {
            let fn_type_str = show_function_type(*function_type);
            format!("Fn({fn_type_str})")
        },
    }
}

fn show_function_type(function_type: FunctionType) -> String {
    let input_types = function_type.input_types.into_iter().map(show_type).collect::<Vec<_>>().join(", ");
    let output_type = show_type(function_type.output_type);
    format!("{input_types} -> {output_type}")
}
