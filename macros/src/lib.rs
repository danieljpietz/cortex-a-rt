use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn, Lit, Expr, ExprLit, parse::Parse, parse::ParseStream, token::Comma, punctuated::Punctuated};


static ENTRIES: std::sync::OnceLock<std::sync::Mutex<Vec<(usize, String, String)>>> = std::sync::OnceLock::new();

struct EntryArgs {
    core_id: usize,
    stack_section: String,
}

impl Parse for EntryArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let args = Punctuated::<Expr, Comma>::parse_terminated(input)?;

        if args.len() != 2 {
            return Err(syn::Error::new_spanned(&args, "Expected two arguments: (core_id, stack_section)"));
        }

        // Extract core_id (first argument)
        let core_id = if let Some(&Expr::Lit(ExprLit { lit: Lit::Int(ref id), .. })) = args.first() {
            id.base10_parse::<usize>()?
        } else {
            return Err(syn::Error::new_spanned(args.first().unwrap(), "First argument must be an integer core ID"));
        };

        // Extract stack_section (second argument)
        let stack_section = if let Some(&Expr::Lit(ExprLit { lit: Lit::Str(ref section), .. })) = args.iter().nth(1) {
            section.value()
        } else {
            return Err(syn::Error::new_spanned(args.iter().nth(1).unwrap(), "Second argument must be a string stack section"));
        };

        Ok(EntryArgs { core_id, stack_section })
    }
}

fn get_entries() -> &'static std::sync::Mutex<Vec<(usize, String, String)>> {
    ENTRIES.get_or_init(|| std::sync::Mutex::new(vec![]))
}

fn safe_entries_access() -> std::sync::MutexGuard<'static, Vec<(usize, String, String)>> {
    get_entries().lock().unwrap_or_else(|poisoned| poisoned.into_inner())
}
 
#[proc_macro_attribute]
pub fn entry(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as EntryArgs);
    let input_fn = parse_macro_input!(item as ItemFn);
    let fn_name = input_fn.sig.ident.clone();

    let mut entries = safe_entries_access();
    if entries.iter().any(|(id, _, _)| *id == args.core_id) {
        panic!("Duplicate entry for core {}", args.core_id);
    }

    entries.push((args.core_id, fn_name.to_string(), args.stack_section.clone()));

    let fn_body = input_fn.block;

    let output_fn = quote! {
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn #fn_name() #fn_body
    };

    output_fn.into()
}

#[proc_macro]
pub fn _start(_: TokenStream) -> TokenStream {
    let entries = safe_entries_access();
    let mut cmp_instructions = vec![];
    let mut core_labels = vec![];

    for (core_id, fn_name, linker_section) in entries.iter() {
        let linker_section_ident = syn::Ident::new(
            &format!("{}", linker_section),
            proc_macro2::Span::call_site(),
        );

        cmp_instructions.push(format!(
            "cmp x0, #{core_id}\n    b.eq .L_core_{core_id}"
        ));

        core_labels.push(format!(
            ".L_core_{core_id}:\n\
                adrp x1, {linker_section_ident}\n\
                mov sp, x1\n\
                adr lr, .L_park_core\n\
                b {fn_name}\n\
                b .L_park_core"
        ));
    }

    let assembly_code = format!(
        r#"
        .section .text._start
        .global _start
        _start:
            mrs x0, MPIDR_EL1
            and x0, x0, #0b11

            {}

            b .L_park_core

            {}

        .L_park_core:
            wfi
            b .L_park_core
        "#,
        cmp_instructions.join("\n    "),
        core_labels.join("\n\n    ")
    );

    let tokens = quote! {
        use core::arch::global_asm;
        global_asm!(#assembly_code);
    };

    tokens.into()
}
