use std::collections::HashSet;

use super::ast::*;
use super::attr;
use super::ctxt::*;
use crate::Derive;

pub(crate) fn check(cx: &Ctxt, container: &Container, derive: Derive) {
    check_boxed(cx, container, derive);
    check_size_hints(cx, container);
    check_flags(cx, container);
}

fn check_boxed(cx: &Ctxt, container: &Container, derive: Derive) {
    match &container.data {
        Data::Enum(variants) => {
            let must_be_boxed = derive == Derive::Read;

            if container.attrs.id.is_some() {
                cx.error_spanned_by(
                    container.original,
                    "#[tl(id = 0x...)] is not allowed in an enum",
                )
            }

            if container.attrs.boxed && variants.is_empty() {
                cx.error_spanned_by(
                    container.original,
                    "#[tl(boxed)] is not allowed in an empty enum",
                )
            }

            if !container.attrs.boxed && must_be_boxed {
                cx.error_spanned_by(
                    container.original,
                    "#[tl(boxed)] is required to implement TlRead for enums",
                )
            }

            let mut unique_ids = HashSet::new();
            for variant in variants {
                if container.attrs.boxed && variant.attrs.id.is_none() {
                    cx.error_spanned_by(
                        variant.original,
                        "#[tl(id = 0x...)] is required for boxed enum variant",
                    )
                }

                if !must_be_boxed && !container.attrs.boxed && variant.attrs.id.is_some() {
                    cx.error_spanned_by(
                        variant.original,
                        "#[tl(id = 0x...)] is not allowed for bare enum variant",
                    )
                }

                if let Some(id) = variant.attrs.id {
                    if !unique_ids.insert(id) {
                        cx.error_spanned_by(
                            variant.original,
                            "duplicate value found for #[tl(id = 0x...)]",
                        )
                    }
                }
            }
        }
        Data::Struct(_, _) => {
            if container.attrs.boxed && container.attrs.id.is_none() {
                cx.error_spanned_by(
                    container.original,
                    "#[tl(id = 0x...)] is required for struct with #[tl(boxed)]",
                )
            }

            if !container.attrs.boxed && container.attrs.id.is_some() {
                cx.error_spanned_by(
                    container.original,
                    "#[tl(id = 0x...)] can't be used without #[tl(boxed)]",
                )
            }
        }
    }
}

fn check_size_hints(cx: &Ctxt, container: &Container) {
    check_size_hint(cx, container.original, &container.attrs.size_hint);

    match &container.data {
        Data::Enum(variants) => {
            for variant in variants {
                check_size_hint(cx, variant.original, &variant.attrs.size_hint);

                if matches!(
                    variant.attrs.size_hint,
                    Some(attr::SizeHint::Explicit { value }) if value > 0
                ) && variant.fields.is_empty()
                {
                    cx.error_spanned_by(
                        variant.original,
                        "a unit variant cannot have a non-zero size hint",
                    );
                }
            }
        }
        Data::Struct(_, fields) => {
            for field in fields {
                check_size_hint(cx, field.original, &field.attrs.size_hint);
            }

            if matches!(
                container.attrs.size_hint,
                Some(attr::SizeHint::Explicit { value }) if value > 0
            ) && fields.is_empty()
            {
                cx.error_spanned_by(
                    container.original,
                    "an empty struct cannot have a non-zero size hint",
                );
            }
        }
    }
}

fn check_size_hint<T>(cx: &Ctxt, object: T, size_hint: &Option<attr::SizeHint>)
where
    T: quote::ToTokens,
{
    if let Some(attr::SizeHint::Explicit { value }) = &size_hint {
        match *value {
            hint if hint % 4 != 0 => {
                cx.error_spanned_by(object, "size hint must be aligned to 4 bytes")
            }
            _ => {}
        }
    }
}

fn check_flags(cx: &Ctxt, container: &Container) {
    let check_fields = |cx: &Ctxt, fields: &[Field]| {
        let mut has_flags_field = false;
        let mut default_flags = None;

        for field in fields {
            if field.attrs.flags {
                default_flags = field.attrs.default_flags;

                if has_flags_field {
                    cx.error_spanned_by(
                        field.original,
                        "only single field with #[tl(flags)] is allowed",
                    );
                }

                if field.attrs.flags_bit.is_some() {
                    cx.error_spanned_by(
                        field.original,
                        "#[tl(flags)] can't be used for one field with #[tl(flags_bit = ...)]",
                    )
                }

                if field.attrs.signature {
                    cx.error_spanned_by(
                        field.original,
                        "#[tl(flags)] can't be used for one field with #[tl(signature)]",
                    );
                }

                if field.attrs.skip_read || field.attrs.skip_write {
                    cx.error_spanned_by(field.original, "field with #[tl(flags)] can't be skipped");
                }

                if field.attrs.size_hint.is_some() {
                    cx.error_spanned_by(
                        field.original,
                        "#[tl(size_hint = ...)] is not allowed for flags field",
                    );
                }

                has_flags_field = true;
            } else if field.attrs.default_flags.is_some() {
                cx.error_spanned_by(
                    field.original,
                    "#[tl(default_flags)] can only be used on the field with #[tl(flags)]",
                );
            }

            if let Some(flags_bit) = field.attrs.flags_bit {
                if !has_flags_field {
                    cx.error_spanned_by(
                        field.original,
                        "the field with #[tl(flags_bit = ...)] must \
                be declared after the field with #[tl(flags)]",
                    )
                }

                if flags_bit > 31 {
                    cx.error_spanned_by(
                        field.original,
                        "#[tl(flags_bit = ...)] bit index output of range",
                    )
                }

                if let Some(default_flags) = default_flags {
                    if default_flags & (0x1 << flags_bit) != 0 {
                        cx.error_spanned_by(
                            field.original,
                            format!(
                                "flags bit {} is already set by default flags (0x{:08x})",
                                flags_bit, default_flags
                            ),
                        )
                    }
                }
            }
        }
    };

    match &container.data {
        Data::Enum(variants) => {
            for variant in variants {
                check_fields(cx, &variant.fields);
            }
        }
        Data::Struct(_, fields) => {
            check_fields(cx, fields);
        }
    }
}
