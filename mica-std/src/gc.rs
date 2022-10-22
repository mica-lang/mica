//! The `Gc` type.

use mica_hl::{
    language::{bytecode::Control, gc::AutoStrategy},
    Arguments, Engine, RawFunctionKind, RawValue, TypeBuilder, UserData,
};

struct GcType;

impl UserData for GcType {}

pub(crate) fn load_gc(engine: &mut Engine) -> Result<(), mica_hl::Error> {
    engine.add_type(
        TypeBuilder::<GcType>::new("Gc")
            .add_raw_static(
                "disable",
                Some(1),
                RawFunctionKind::Foreign(Box::new(|gc, _| {
                    gc.auto_strategy = AutoStrategy::Disabled;
                    Ok(RawValue::from(()))
                })),
            )
            .add_raw_static(
                "enable_always_run",
                Some(1),
                RawFunctionKind::Foreign(Box::new(|gc, _| {
                    gc.auto_strategy = AutoStrategy::AlwaysRun;
                    Ok(RawValue::from(()))
                })),
            )
            .add_raw_static(
                "enable_with_ceiling",
                Some(3),
                RawFunctionKind::Foreign(Box::new(|gc, args| {
                    let arguments = Arguments::new(args);
                    gc.auto_strategy = AutoStrategy::Ceiling {
                        next_run: arguments.nth(0).unwrap().ensure_number()? as usize,
                        growth_factor: arguments.nth(1).unwrap().ensure_number()? as usize,
                    };
                    Ok(RawValue::from(()))
                })),
            )
            .add_raw_static("collect", Some(1), RawFunctionKind::Control(Control::GcCollect))
            .add_raw_static(
                "allocated_bytes",
                Some(1),
                RawFunctionKind::Foreign(Box::new(|gc, _| {
                    let bytes = gc.allocated_bytes() as f64;
                    Ok(RawValue::from(bytes))
                })),
            ),
    )?;

    Ok(())
}
