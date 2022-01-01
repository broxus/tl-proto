use super::ast::*;
use super::ctxt::*;

pub fn check(cx: &Ctxt, container: &Container) {
    check_boxed(cx, container);
}

fn check_boxed(cx: &Ctxt, container: &Container) {
    if !container.attrs.boxed {
        return;
    }

    match &container.data {
        Data::Enum(variants) => {
            if container.attrs.id.is_some() {
                cx.error_spanned_by(
                    container.original,
                    "#[tl(id = 0x...)] is not allowed in an enum",
                )
            }

            for variant in variants {
                if variant.attrs.id.is_none() {
                    cx.error_spanned_by(
                        variant.original,
                        "#[tl(id = 0x...)] is required for boxed enum variant",
                    )
                }
            }
        }
        Data::Struct(_, _) => {
            if container.attrs.id.is_none() {
                cx.error_spanned_by(
                    container.original,
                    "#[tl(id = 0x...)] is required for struct with #[tl(boxed)]",
                )
            }
        }
    }
}
