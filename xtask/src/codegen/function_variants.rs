use std::{
    fmt::Write,
    fs::{self, File},
    io::Write as IoWrite,
};

use camino::{Utf8Path, Utf8PathBuf};
use tracing::{debug, info, info_span};

const PRELUDE: &str = r#"// Generated by mica xtask. Do not modify directly.
// Edit xtask/src/codegen/function_variants.rs instead, and regenerate with:
//  $ cargo xtask codegen

#![allow(unused_variables)]

use crate::{
    ll::bytecode::{FunctionParameterCount, MethodParameterCount},
    ffvariants, Arguments, ForeignFunction, IntoValue, MutSelfFromRawValue, RawForeignFunction,
    RawSelf, SelfFromRawValue, TryFromValue, wrap_in_language_error,
};
"#;

pub fn generate() -> anyhow::Result<Utf8PathBuf> {
    let _span = info_span!("function_variants").entered();

    let output_filename = Utf8Path::new("src/hl/generated/function_variants.rs");
    let output_directory = output_filename.parent().unwrap();

    fs::create_dir_all(output_directory)?;
    info!("created output directory");

    let mut output = File::create(output_filename)?;
    writeln!(output, "{}", PRELUDE)?;
    debug!("write prelude");

    generate_for_params(&mut output, &[])?;
    generate_for_params(&mut output, &["A"])?;
    generate_for_params(&mut output, &["A", "B"])?;
    generate_for_params(&mut output, &["A", "B", "C"])?;
    generate_for_params(&mut output, &["A", "B", "C", "D"])?;
    generate_for_params(&mut output, &["A", "B", "C", "D", "E"])?;
    generate_for_params(&mut output, &["A", "B", "C", "D", "E", "F"])?;
    generate_for_params(&mut output, &["A", "B", "C", "D", "E", "F", "G"])?;
    generate_for_params(&mut output, &["A", "B", "C", "D", "E", "F", "G", "H"])?;

    Ok(output_filename.to_owned())
}

fn generate_for_params(
    w: &mut dyn IoWrite,
    function_param_value_types: &[&str],
) -> anyhow::Result<()> {
    let _span = info_span!("params", params = ?function_param_value_types);

    const BOUND_RET: &str = "Ret: IntoValue";
    const BOUND_ERR: &str = "Err: std::error::Error";
    const BOUND_SELF: &str = "Recv: SelfFromRawValue";
    const BOUND_MUT_SELF: &str = "Recv: MutSelfFromRawValue";

    const FUNCTION_PARAMETER_COUNT: &str = r#"
        type ParameterCount = FunctionParameterCount;
        const PARAMETER_COUNT: Self::ParameterCount = Self::ParameterCount::Fixed($COUNT);
    "#;
    const METHOD_PARAMETER_COUNT: &str = r#"
        type ParameterCount = MethodParameterCount;
        const PARAMETER_COUNT: Self::ParameterCount = Self::ParameterCount::from_count_with_self($COUNT);
    "#;

    let infallible_options = GenerateVariantOptions {
        variant: "Infallible",
        variant_args: &[],
        function_param_user_types: &[],
        function_param_value_types,
        function_return_type: "Ret",
        parameter_count_definition: FUNCTION_PARAMETER_COUNT,
        user_generic_params: &[("Ret")],
        user_generic_bounds: &[BOUND_RET],
        map_result_action: r#"
            Ok(result.into_value_with_environment(env).to_raw(gc))
        "#,
        self_mode: SelfMode::Disabled,
    };
    let fallible_options = GenerateVariantOptions {
        variant: "Fallible",
        variant_args: &[],
        function_param_user_types: &[],
        function_param_value_types,
        function_return_type: "Result<Ret, Err>",
        parameter_count_definition: FUNCTION_PARAMETER_COUNT,
        user_generic_params: &["Ret", "Err"],
        user_generic_bounds: &[BOUND_RET, BOUND_ERR],
        map_result_action: r#"
            wrap_in_language_error(result.map(|v| v.into_value_with_environment(env).to_raw(gc)))
        "#,
        self_mode: SelfMode::Disabled,
    };

    generate_variant(w, infallible_options)?;
    generate_variant(w, fallible_options)?;

    const SETUP_RAW_SELF: &str = r#"
        let arg_self = RawSelf(arguments.raw_self());
    "#;

    generate_variant(
        w,
        GenerateVariantOptions {
            variant: "InfallibleRawSelf",
            function_param_user_types: &["RawSelf<'_>"],
            parameter_count_definition: METHOD_PARAMETER_COUNT,
            self_mode: SelfMode::Enabled { setup_code: SETUP_RAW_SELF },
            ..infallible_options
        },
    )?;
    generate_variant(
        w,
        GenerateVariantOptions {
            variant: "FallibleRawSelf",
            function_param_user_types: &["RawSelf<'_>"],
            parameter_count_definition: METHOD_PARAMETER_COUNT,
            self_mode: SelfMode::Enabled { setup_code: SETUP_RAW_SELF },
            ..fallible_options
        },
    )?;

    const VARIANT_ARG_SELF: &str = "ffvariants::ImmutableSelf<Recv>";
    const VARIANT_ARG_MUT_SELF: &str = "ffvariants::MutableSelf<Recv>";
    const SETUP_SELF: &str = r#"
        let (arg_self, _guard) = wrap_in_language_error(unsafe {
            <Recv as SelfFromRawValue>::self_from_raw_value(arguments.raw_self())
        })?;
    "#;
    const SETUP_MUT_SELF: &str = r#"
        let (arg_self, _guard) = wrap_in_language_error(unsafe {
            <Recv as MutSelfFromRawValue>::mut_self_from_raw_value(arguments.raw_self())
        })?;
    "#;

    generate_variant(
        w,
        GenerateVariantOptions {
            variant: "InfallibleSelf",
            variant_args: &[VARIANT_ARG_SELF],
            function_param_user_types: &["&Recv"],
            parameter_count_definition: METHOD_PARAMETER_COUNT,
            user_generic_params: &["Ret", "Recv"],
            user_generic_bounds: &[BOUND_RET, BOUND_SELF],
            self_mode: SelfMode::Enabled { setup_code: SETUP_SELF },
            ..infallible_options
        },
    )?;
    generate_variant(
        w,
        GenerateVariantOptions {
            variant: "InfallibleSelf",
            variant_args: &[VARIANT_ARG_MUT_SELF],
            function_param_user_types: &["&mut Recv"],
            parameter_count_definition: METHOD_PARAMETER_COUNT,
            user_generic_params: &["Ret", "Recv"],
            user_generic_bounds: &[BOUND_RET, BOUND_MUT_SELF],
            self_mode: SelfMode::Enabled { setup_code: SETUP_MUT_SELF },
            ..infallible_options
        },
    )?;
    generate_variant(
        w,
        GenerateVariantOptions {
            variant: "FallibleSelf",
            variant_args: &[VARIANT_ARG_SELF],
            function_param_user_types: &["&Recv"],
            parameter_count_definition: METHOD_PARAMETER_COUNT,
            user_generic_params: &["Ret", "Err", "Recv"],
            user_generic_bounds: &[BOUND_RET, BOUND_ERR, BOUND_SELF],
            self_mode: SelfMode::Enabled { setup_code: SETUP_SELF },
            ..fallible_options
        },
    )?;
    generate_variant(
        w,
        GenerateVariantOptions {
            variant: "FallibleSelf",
            variant_args: &[VARIANT_ARG_MUT_SELF],
            function_param_user_types: &["&mut Recv"],
            parameter_count_definition: METHOD_PARAMETER_COUNT,
            user_generic_params: &["Ret", "Err", "Recv"],
            user_generic_bounds: &[BOUND_RET, BOUND_ERR, BOUND_MUT_SELF],
            self_mode: SelfMode::Enabled { setup_code: SETUP_MUT_SELF },
            ..fallible_options
        },
    )?;

    debug!("generation finished");
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SelfMode<'a> {
    Disabled,
    Enabled {
        // A variable `arg_self` must be declared in this snippet of code.
        setup_code: &'a str,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct GenerateVariantOptions<'a> {
    variant: &'a str,
    variant_args: &'a [&'a str],
    function_param_user_types: &'a [&'a str],
    function_param_value_types: &'a [&'a str],
    function_return_type: &'a str,
    parameter_count_definition: &'a str,
    user_generic_params: &'a [&'a str],
    user_generic_bounds: &'a [&'a str],
    map_result_action: &'a str,
    self_mode: SelfMode<'a>,
}

fn generate_variant(
    w: &mut dyn IoWrite,
    opts: GenerateVariantOptions<'_>,
) -> Result<(), anyhow::Error> {
    let GenerateVariantOptions {
        variant,
        variant_args,
        function_param_user_types,
        function_param_value_types,
        function_return_type,
        parameter_count_definition,
        user_generic_params,
        user_generic_bounds,
        map_result_action,
        self_mode,
    } = opts;

    let variant_args = to_comma_separated_list(variant_args.iter());

    let user_generic_param_names = to_comma_separated_list(user_generic_params.iter());

    let value_params = to_comma_separated_list(function_param_value_types.iter());
    let user_params = to_comma_separated_list(function_param_user_types.iter());
    let params_bounds = to_comma_separated_list(
        user_generic_bounds
            .iter()
            .map(|str| str.to_string())
            .chain(function_param_value_types.iter().map(|p| format!("{p}: TryFromValue")))
            .map(|bound| format!("{bound} + 'static")),
    );

    let mut into_raw_foreign_function =
        String::from(r#" let arguments = Arguments::new(args, env); "#);

    if let SelfMode::Enabled { setup_code } = &self_mode {
        into_raw_foreign_function.push_str(setup_code);
    }

    let variable_list: Vec<_> =
        function_param_value_types.iter().enumerate().map(|(i, _)| format!("arg_{i}")).collect();
    for (i, variable) in variable_list.iter().enumerate() {
        writeln!(
            into_raw_foreign_function,
            "let {variable} = wrap_in_language_error(arguments.get({i}))?;"
        )?;
    }
    let self_variable = match &self_mode {
        SelfMode::Disabled => None,
        SelfMode::Enabled { .. } => Some("arg_self".to_string()),
    };
    let variable_list: Vec<_> =
        self_variable.into_iter().chain(variable_list.into_iter()).collect();

    let args = variable_list.join(", ");
    write!(
        into_raw_foreign_function,
        r#"
            let result = self({args});
            {map_result_action}
        "#
    )?;

    let parameter_count =
        function_param_value_types.len() + (self_mode != SelfMode::Disabled) as usize;
    let parameter_count_definition =
        parameter_count_definition.replace("$COUNT", &parameter_count.to_string());
    write!(
        w,
        r#"
            impl<
                Fun,
                {user_generic_param_names}
                {value_params}
            > ForeignFunction<ffvariants::{variant}<{variant_args} ({user_params} {value_params})>> for Fun
            where
                Fun: Fn({user_params} {value_params}) -> {function_return_type} + 'static,
                {params_bounds}
            {{
                {parameter_count_definition}

                fn into_raw_foreign_function(self) -> RawForeignFunction {{
                    Box::new(move |env, gc, args| {{
                        {into_raw_foreign_function}
                    }})
                }}
            }}
        "#
    )?;
    Ok(())
}

fn to_comma_separated_list(list: impl Iterator<Item = impl AsRef<str>>) -> String {
    let mut result = String::new();
    for item in list {
        result.push_str(item.as_ref());
        result.push(',');
    }
    result
}
