use proc_macro2::TokenStream;
use quote::ToTokens;
use std::collections::HashSet;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use rustc_hash::FxHashMap;

use super::internals::{ast, attr, ctxt};

pub fn compute_tl_id(cx: &ctxt::Ctxt, mut id: attr::TlId, scheme: &attr::Scheme) -> Option<u32> {
    let (scheme, lit) = match load_scheme(cx, scheme) {
        Some(scheme) => scheme,
        None => return None,
    };
    let scheme = match tl_scheme::Scheme::parse(&scheme) {
        Ok(scheme) => scheme,
        Err(e) => {
            cx.error_spanned_by(lit, format!("invalid scheme: {e:#}"));
            return None;
        }
    };

    let mut state = State {
        scheme: &scheme,
        unique_ids: Default::default(),
        all_ids: Default::default(),
        all_ids_loaded: false,
        output: None,
    };
    state.update_id(cx, &mut id)
}

pub fn compute_tl_ids(cx: &ctxt::Ctxt, container: &mut ast::Container) {
    let scheme = match &container.attrs.scheme {
        Some(scheme) => scheme,
        None => return,
    };
    let (scheme, lit) = match load_scheme(cx, scheme) {
        Some(scheme) => scheme,
        None => return,
    };
    let scheme = match tl_scheme::Scheme::parse(&scheme) {
        Ok(scheme) => scheme,
        Err(e) => {
            cx.error_spanned_by(lit, format!("invalid scheme: {e:#}"));
            return;
        }
    };

    let mut state = State {
        scheme: &scheme,
        unique_ids: Default::default(),
        all_ids: Default::default(),
        all_ids_loaded: false,
        output: None,
    };

    if let Some(id) = &mut container.attrs.id {
        state.update_id(cx, id);
    }
    match &mut container.data {
        ast::Data::Enum(variants) if container.attrs.boxed => {
            for variant in variants.iter_mut() {
                if let Some(id) = &mut variant.attrs.id {
                    state.update_id(cx, id);
                }
            }
        }
        _ => {}
    }
}

struct State<'s, 'a> {
    scheme: &'a tl_scheme::Scheme<'s>,
    unique_ids: HashSet<u32>,
    all_ids: AllIds<'s, 'a>,
    all_ids_loaded: bool,
    output: Option<(tl_scheme::ConstructorKind, tl_scheme::OutputType<'s>)>,
}

impl<'s, 'a: 's> State<'s, 'a> {
    fn update_id(&mut self, cx: &ctxt::Ctxt, id: &mut attr::TlId) -> Option<u32> {
        match id {
            attr::TlId::FromScheme { value, lit } => match self.scheme.find_constructor(value) {
                Some((kind, constructor)) => {
                    let value = constructor.compute_tl_id();

                    self.check_id(cx, value, lit);
                    check_constructor(&mut self.output, cx, kind, constructor, lit);

                    *id = attr::TlId::Explicit {
                        value,
                        lit: lit.to_token_stream(),
                    };

                    Some(value)
                }
                None => {
                    cx.error_spanned_by(lit, format!("unknown variant: {value}"));
                    None
                }
            },
            attr::TlId::Explicit { value, lit } => {
                self.check_id(cx, *value, lit);

                if !self.all_ids_loaded {
                    self.all_ids = self.scheme.compute_all_ids();
                    self.all_ids_loaded = true;
                }

                match self.all_ids.get(value) {
                    Some((kind, constructor)) => {
                        check_constructor(&mut self.output, cx, *kind, constructor, lit);
                        Some(*value)
                    }
                    None => {
                        cx.error_spanned_by(lit, "unknown TL id");
                        None
                    }
                }
            }
        }
    }

    fn check_id(&mut self, cx: &ctxt::Ctxt, id: u32, lit: &TokenStream) {
        if !self.unique_ids.insert(id) {
            cx.error_spanned_by(lit, "duplicate id found for #[tl(id = ...)]");
        }
    }
}

fn check_constructor<'a, 's>(
    output: &mut Option<(tl_scheme::ConstructorKind, tl_scheme::OutputType<'s>)>,
    cx: &ctxt::Ctxt,
    kind: tl_scheme::ConstructorKind,
    constructor: &'s tl_scheme::Constructor<'a>,
    lit: &TokenStream,
) {
    match output {
        Some((output_kind, output)) => {
            if *output_kind != kind {
                cx.error_spanned_by(
                    lit,
                    format!(
                        "constructor kind mismatch. Expected: {}. Got: {}",
                        output_kind, kind
                    ),
                )
            } else if kind == tl_scheme::ConstructorKind::Type && output != &constructor.output {
                cx.error_spanned_by(
                    lit,
                    format!(
                        "constructor output type mismatch. Expected: {}. Got: {}",
                        output, constructor.output
                    ),
                );
            }
        }
        None => *output = Some((kind, constructor.output.clone())),
    }
}

fn load_scheme<'a>(cx: &ctxt::Ctxt, scheme: &'a attr::Scheme) -> Option<(String, &'a syn::LitStr)> {
    match &scheme.source {
        attr::SchemeSource::File { path } => {
            let root = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
            let full_path = Path::new(&root).join("src/").join(&path.value());
            if full_path.file_name().is_none() {
                cx.error_spanned_by(path, "scheme attribute should point to a file");
                return None;
            };

            match read_file(&full_path) {
                Ok(data) => Some((data, path)),
                Err(e) => {
                    cx.error_spanned_by(path, format!("error opening {:?}: {e:?}", full_path));
                    None
                }
            }
        }
        attr::SchemeSource::Inline { content } => Some((content.value(), content)),
    }
}

fn read_file<P: AsRef<Path>>(path: P) -> std::io::Result<String> {
    let mut file = File::open(path.as_ref())?;
    let mut string = String::new();
    file.read_to_string(&mut string)?;
    Ok(string)
}

type AllIds<'a, 'b> = FxHashMap<u32, (tl_scheme::ConstructorKind, &'b tl_scheme::Constructor<'a>)>;
