//! The virtual machine.

use std::{collections::HashSet, fmt, pin::Pin, ptr, rc::Rc};

use super::bytecode::{FunctionIndex, GlobalIndex, ImplementedTraitIndex, MethodIndex};
use crate::ll::{
    bytecode::{
        CaptureKind, Chunk, Control, DispatchTable, Environment, FunctionKind,
        MethodParameterCount, MethodSignature, Opcode, PrototypeIndex, TraitIndex,
    },
    error::{Error, ErrorKind, Location, RenderedSignature, StackTraceEntry},
    gc::{GcRaw, Memory},
    value::{create_trait, Closure, Dict, List, RawValue, Struct, Trait, Upvalue, ValueKind},
};

/// Storage for global variables.
#[derive(Debug)]
pub struct Globals {
    values: Vec<RawValue>,
}

impl Globals {
    /// Creates a new storage.
    pub fn new() -> Self {
        Self { values: Vec::new() }
    }

    /// Sets the global in the given slot.
    pub fn set(&mut self, slot: GlobalIndex, value: RawValue) {
        let slot = slot.to_usize();
        if slot >= self.values.len() {
            self.values.resize(slot + 1, ().into());
        }
        // Safety: the vec has just been extended to have the correct size so `slot` is in bounds.
        unsafe {
            *self.values.get_unchecked_mut(slot) = value;
        }
    }

    /// Returns the global in the given slot, or `Nil` if there's no global there.
    pub fn get(&self, slot: GlobalIndex) -> RawValue {
        let slot = slot.to_usize();
        self.values.get(slot).cloned().unwrap_or(().into())
    }

    /// Returns the global in the given slot without performing bounds checks.
    ///
    /// # Safety
    ///
    /// This is only safe to use by the VM. The code generator ensures that all globals are set
    /// before use.
    unsafe fn get_unchecked(&self, slot: GlobalIndex) -> RawValue {
        let slot = slot.to_usize();
        *self.values.get_unchecked(slot)
    }

    /// Returns an iterator over all globals.
    pub(crate) fn iter(&self) -> impl Iterator<Item = RawValue> + '_ {
        self.values.iter().copied()
    }
}

impl Default for Globals {
    fn default() -> Self {
        Self::new()
    }
}

/// The return point saved before entering a function.
#[derive(Debug)]
struct ReturnPoint {
    chunk: Option<Rc<Chunk>>,
    closure: Option<GcRaw<Closure>>,
    pc: usize,
    stack_bottom: usize,
}

/// The virtual machine state.
pub struct Fiber {
    pc: usize,
    chunk: Rc<Chunk>,
    closure: Option<GcRaw<Closure>>,

    stack: Vec<RawValue>,
    stack_bottom: usize,
    open_upvalues: Vec<(u32, Pin<Rc<Upvalue>>)>,
    call_stack: Vec<ReturnPoint>,
    breakable_block_stack: Vec<usize>,

    halted: bool,
}

impl Fiber {
    /// Creates a new VM.
    pub fn new(chunk: Rc<Chunk>, stack: Vec<RawValue>) -> Self {
        Self {
            pc: 0,
            chunk,
            closure: None,
            stack,
            stack_bottom: 0,
            open_upvalues: Vec::new(),
            call_stack: Vec::new(),
            breakable_block_stack: Vec::new(),
            halted: false,
        }
    }

    /// Returns whether the fiber has halted execution.
    pub fn halted(&self) -> bool {
        self.halted
    }

    /// Halts the VM and produces an error.
    fn error(&mut self, env: &Environment, kind: ErrorKind) -> Error {
        self.halted = true;
        Error::Runtime {
            kind,
            call_stack: self
                .call_stack
                .iter()
                .filter_map(|return_point| {
                    Some(StackTraceEntry {
                        function_name: if let Some(closure) = &return_point.closure {
                            let closure = unsafe { closure.get() };
                            let function =
                                unsafe { env.get_function_unchecked(closure.function_id) };
                            if function.hidden_in_stack_traces {
                                return None;
                            }
                            Rc::clone(&function.name)
                        } else {
                            Rc::from("<main>")
                        },
                        module_name: if let Some(chunk) = &return_point.chunk {
                            Rc::clone(&chunk.module_name)
                        } else {
                            Rc::from("<FFI>")
                        },
                        location: if let Some(chunk) = &return_point.chunk {
                            chunk.location(return_point.pc - Opcode::INSTRUCTION_SIZE)
                        } else {
                            Location::UNINIT
                        },
                    })
                })
                .collect(),
        }
    }

    /// Pushes a value onto the stack.
    fn push(&mut self, value: RawValue) {
        self.stack.push(value);
        #[cfg(feature = "trace-vm-stack-ops")]
        {
            println!("push | {:?}", &self.stack);
        }
    }

    /// Pops a value off the stack.
    fn pop(&mut self) -> RawValue {
        #[cfg(debug_assertions)]
        let value = { self.stack.pop().unwrap() };
        #[cfg(not(debug_assertions))]
        let value = unsafe { self.stack.pop().unwrap_unchecked() };
        #[cfg(feature = "trace-vm-stack-ops")]
        {
            println!("pop  | {:?} -> {:?}", &self.stack, value);
        }
        value
    }

    /// Returns a reference to the value at the top of the stack.
    fn stack_top(&self) -> RawValue {
        #[cfg(debug_assertions)]
        {
            self.stack.last().copied().unwrap()
        }
        #[cfg(not(debug_assertions))]
        unsafe {
            *self.stack.get_unchecked(self.stack.len() - 1)
        }
    }

    /// Returns a mutable reference to the value at the top of the stack.
    fn stack_top_mut(&mut self) -> &mut RawValue {
        #[cfg(debug_assertions)]
        {
            self.stack.last_mut().unwrap()
        }
        #[cfg(not(debug_assertions))]
        unsafe {
            let last = self.stack.len() - 1;
            self.stack.get_unchecked_mut(last)
        }
    }

    /// Returns a reference to the `n`th value counted from the top of the stack.
    fn nth_from_top(&self, n: usize) -> &RawValue {
        #[cfg(debug_assertions)]
        {
            &self.stack[self.stack.len() - n]
        }
        #[cfg(not(debug_assertions))]
        unsafe {
            let i = self.stack.len() - n;
            self.stack.get_unchecked(i)
        }
    }

    /// Saves the current program counter and other execution state before entering a function.
    fn save_return_point(&mut self) {
        self.call_stack.push(ReturnPoint {
            chunk: Some(Rc::clone(&self.chunk)),
            closure: self.closure,
            pc: self.pc,
            stack_bottom: self.stack_bottom,
        });
    }

    /// Restores previous VM state from the return point stack.
    fn restore_return_point(&mut self) {
        let return_point = self.call_stack.pop().unwrap();
        // Remove unnecessary values from the stack (eg. arguments).
        while self.stack.len() > self.stack_bottom {
            self.stack.pop().unwrap();
        }
        // Restore state.
        // SAFETY: Assume the matching chunk is a bytecode chunk.
        self.chunk = unsafe { return_point.chunk.unwrap_unchecked() };
        self.closure = return_point.closure;
        self.pc = return_point.pc;
        self.stack_bottom = return_point.stack_bottom;
    }

    /// Returns an upvalue for the local at the given stack slot.
    fn get_upvalue_for_local(&mut self, stack_slot: u32) -> Pin<Rc<Upvalue>> {
        if let Some((_, upvalue)) =
            self.open_upvalues.iter().rev().find(|(slot, _)| *slot == stack_slot)
        {
            Pin::clone(upvalue)
        } else {
            let stack_ptr = &mut self.stack[stack_slot as usize] as *mut _;
            // SAFETY: Vec storage is never null.
            let stack_ptr = unsafe { ptr::NonNull::new_unchecked(stack_ptr) };
            let upvalue = Upvalue::new(stack_ptr);
            self.open_upvalues.push((stack_slot, Pin::clone(&upvalue)));
            upvalue
        }
    }

    /// Allocates `n` storage slots for local variables.
    fn allocate_chunk_storage_slots(&mut self, n: usize) {
        self.stack.extend(std::iter::repeat_with(|| RawValue::from(())).take(n));
    }

    /// Calls a function. For bytecode functions this saves the stack and begins executing the
    /// function's chunk. For foreign functions it simply calls the function.
    fn enter_function(
        &mut self,
        env: &Environment,
        globals: &mut Globals,
        gc: &mut Memory,
        closure: GcRaw<Closure>,
        argument_count: usize,
    ) -> Result<(), Error> {
        let function = unsafe { env.get_function_unchecked(closure.get().function_id) };
        match &function.kind {
            FunctionKind::Bytecode { chunk, .. } => {
                self.save_return_point();
                self.chunk = Rc::clone(chunk);
                self.closure = Some(closure);
                self.pc = 0;
                self.stack_bottom = self.stack.len() - argument_count;
                self.allocate_chunk_storage_slots(chunk.preallocate_stack_slots as usize);
            }
            FunctionKind::Foreign(f) => {
                let arguments =
                    unsafe { self.stack.get_unchecked(self.stack.len() - argument_count..) };
                let result = match f(gc, arguments) {
                    Ok(value) => value,
                    Err(kind) => {
                        return Err(self.error_outside_function_call(Some(closure), env, kind));
                    }
                };
                for _ in 0..argument_count {
                    self.pop();
                }
                self.push(result);
            }
            &FunctionKind::Control(ctl) => {
                self.call_control(env, globals, gc, ctl, argument_count)?;
            }
        }
        Ok(())
    }

    /// Handles a call to a control function.
    fn call_control(
        &mut self,
        env: &Environment,
        globals: &mut Globals,
        gc: &mut Memory,
        ctl: Control,
        argument_count: usize,
    ) -> Result<(), Error> {
        match ctl {
            Control::GcCollect => {
                if argument_count != 1 {
                    return Err(self.error_outside_function_call(
                        None,
                        env,
                        ErrorKind::TooManyArguments,
                    ));
                }
                unsafe { gc.collect(self.roots(globals)) }
                self.pop();
                self.push(RawValue::from(()));
            }
        }
        Ok(())
    }

    /// Constructs an error that wasn't triggered by a function call.
    fn error_outside_function_call(
        &mut self,
        closure: Option<GcRaw<Closure>>,
        env: &Environment,
        kind: ErrorKind,
    ) -> Error {
        self.save_return_point();
        if let Some(closure) = closure {
            self.call_stack.push(ReturnPoint {
                chunk: None,
                closure: Some(closure),
                pc: 0,
                stack_bottom: 0,
            });
        }
        let error = self.error(env, kind);
        if closure.is_some() {
            self.call_stack.pop();
        }
        self.call_stack.pop();
        error
    }

    /// Constructs a closure from surrounding stack variables and upvalues.
    fn create_closure(
        &mut self,
        env: &Environment,
        gc: &mut Memory,
        function_id: FunctionIndex,
        name: Rc<str>,
    ) -> GcRaw<Closure> {
        let function = unsafe { env.get_function_unchecked(function_id) };
        let mut captures = Vec::new();
        if let FunctionKind::Bytecode { captured_locals, .. } = &function.kind {
            for capture in captured_locals {
                captures.push(match capture {
                    CaptureKind::Local(slot) => {
                        self.get_upvalue_for_local(self.stack_bottom as u32 + slot.to_u32())
                    }
                    CaptureKind::Upvalue(index) => {
                        let closure = unsafe { self.closure.unwrap_unchecked() };
                        let closure = unsafe { closure.get() };
                        Pin::clone(&closure.captures[index.to_u32() as usize])
                    }
                });
            }
        }
        gc.allocate(Closure { name, function_id, captures })
    }

    /// Returns the dispatch table of the given value.
    fn get_dispatch_table<'v>(value: &'v RawValue, env: &'v Environment) -> &'v DispatchTable {
        unsafe {
            match value.kind() {
                ValueKind::Nil => &env.builtin_dtables.nil,
                ValueKind::Boolean => &env.builtin_dtables.boolean,
                ValueKind::Number => &env.builtin_dtables.number,
                ValueKind::String => &env.builtin_dtables.string,
                ValueKind::Function => &env.builtin_dtables.function,
                ValueKind::List => &env.builtin_dtables.list,
                ValueKind::Dict => &env.builtin_dtables.dict,
                ValueKind::Struct => value.get_raw_struct_unchecked().get().dtable(),
                ValueKind::Trait => value.get_raw_trait_unchecked().get().dtable(),
                ValueKind::UserData => value.get_raw_user_data_unchecked().get().dtable(),
            }
        }
    }

    /// Initializes a dispatch table with methods obtained from a method ID to function ID map.
    /// Each function's name is prepended with `type_name.`.
    fn initialize_dtable(
        &mut self,
        methods: impl Iterator<Item = (MethodIndex, FunctionIndex)>,
        env: &Environment,
        gc: &mut Memory,
        dtable: &mut DispatchTable,
    ) {
        for (method_id, function_id) in methods {
            let function = unsafe { env.get_function_unchecked(function_id) };
            let name = Rc::from(format!("{}.{}", dtable.pretty_name, function.name));
            let closure = self.create_closure(env, gc, function_id, name);
            dtable.set_method(method_id, closure);
        }
    }

    fn initialize_dtable_with_trait_methods(
        &mut self,
        methods: impl Iterator<
            Item = (Rc<str>, MethodParameterCount, ImplementedTraitIndex, FunctionIndex),
        >,
        traits: &[GcRaw<Trait>],
        env: &Environment,
        gc: &mut Memory,
        dtable: &mut DispatchTable,
        unimplemented_methods: &mut HashSet<MethodIndex>,
    ) -> Result<(), ErrorKind> {
        for (name, parameter_count, trait_index, function_id) in methods {
            let trait_handle = unsafe { traits[trait_index.to_usize()].get() };
            let trait_id = trait_handle.id;
            let method_signature = MethodSignature {
                name: Rc::clone(&name),
                parameter_count,
                trait_id: Some(trait_id),
            };
            // NOTE: This should never panic because trait method indices are created during the
            // trait's declaration. New method indices should not be made here.
            let method_id = env
                .get_method_index(&method_signature)
                .expect("existing method index must be found");
            let closure = self.create_closure(env, gc, function_id, name);
            dtable.set_method(method_id, closure);

            if !unimplemented_methods.remove(&method_id) {
                return Err(ErrorKind::DoubleMethodImplementation {
                    type_name: Rc::clone(&dtable.pretty_name),
                    signature: method_signature.render(env),
                });
            }
        }

        if !unimplemented_methods.is_empty() {
            let mut methods: Vec<_> = unimplemented_methods
                .iter()
                .map(|&method_id| {
                    env.get_method_signature(method_id)
                        .map(|signature| signature.render(env))
                        .unwrap_or_else(RenderedSignature::invalid)
                })
                .collect();
            methods.sort_unstable_by_key(|signature| {
                // Maybe doing the Rc clones here isn't ideal, but this is the sad path which
                // doesn't have to be The Fastest.
                (
                    signature.trait_name.clone(),
                    Rc::clone(&signature.name),
                    signature.parameter_count,
                )
            });
            return Err(ErrorKind::MethodsUnimplemented {
                type_name: Rc::clone(&dtable.pretty_name),
                methods,
            });
        }

        Ok(())
    }

    /// Returns an iterator over all GC roots.
    fn roots<'a>(&'a self, globals: &'a mut Globals) -> impl Iterator<Item = RawValue> + 'a {
        globals.iter().chain(self.stack.iter().copied()).chain(self.closure.map(RawValue::from))
    }

    /// Interprets bytecode in the chunk, with the provided user state.
    pub fn interpret(
        &mut self,
        env: &Environment,
        globals: &mut Globals,
        gc: &mut Memory,
    ) -> Result<RawValue, Error> {
        self.allocate_chunk_storage_slots(self.chunk.preallocate_stack_slots as usize);

        loop {
            #[cfg(feature = "trace-vm-opcodes")]
            {
                print!("op   @ {:06x} ", self.pc);
            }
            let (opcode, operand) = unsafe { self.chunk.read_instruction(&mut self.pc) };
            #[cfg(feature = "trace-vm-opcodes")]
            {
                println!("{:?} ({})", opcode, operand);
            }

            macro_rules! wrap_error {
                ($exp:expr) => {{
                    let result = $exp;
                    match result {
                        Ok(value) => value,
                        Err(kind) => {
                            self.save_return_point();
                            return Err(self.error(env, kind));
                        }
                    }
                }};
            }

            macro_rules! binary_operator {
                ($op:tt) => {{
                    let right = wrap_error!(self.pop().ensure_number());
                    let left = wrap_error!(self.pop().ensure_number());
                    self.stack.push(RawValue::from(left $op right));
                }};
            }

            match opcode {
                Opcode::Nop => (),

                Opcode::PushNil => self.push(RawValue::from(())),
                Opcode::PushTrue => self.push(RawValue::from(true)),
                Opcode::PushFalse => self.push(RawValue::from(false)),
                Opcode::PushNumber => {
                    let number = unsafe { self.chunk.read_number(&mut self.pc) };
                    self.push(RawValue::from(number));
                }
                Opcode::PushString => {
                    let string = unsafe { self.chunk.read_string(&mut self.pc) }.to_owned();
                    unsafe { gc.auto_collect(self.roots(globals)) };
                    let rc = gc.allocate(string);
                    self.push(RawValue::from(rc));
                }
                Opcode::CreateClosure => {
                    unsafe { gc.auto_collect(self.roots(globals)) };
                    let function_id = FunctionIndex::from_opr24(operand);
                    let function = unsafe { env.get_function_unchecked(function_id) };
                    let closure =
                        self.create_closure(env, gc, function_id, Rc::clone(&function.name));
                    self.push(RawValue::from(closure));
                }
                Opcode::CreateType => {
                    let name = unsafe { self.chunk.read_string(&mut self.pc) };
                    let mut dispatch_table = DispatchTable::new_for_type(name);
                    dispatch_table.pretty_name = Rc::from(format!("unimplemented type {name}"));
                    let dispatch_table = gc.allocate(dispatch_table);
                    let struct_v = Struct::new_type(dispatch_table);
                    self.stack.push(RawValue::from(gc.allocate(struct_v)));
                }
                Opcode::CreateStruct => {
                    unsafe { gc.auto_collect(self.roots(globals)) };
                    let type_struct = unsafe { self.pop().get_raw_struct_unchecked() };
                    let field_count = usize::from(operand);
                    let instance = unsafe { type_struct.get().new_instance(field_count) };
                    let instance = gc.allocate(instance);
                    self.push(RawValue::from(instance));
                }
                Opcode::CreateTrait => {
                    let trait_index = TraitIndex::from_opr24(operand);
                    let instance = create_trait(env, gc, trait_index);
                    self.push(RawValue::from(instance));
                }
                Opcode::CreateList => {
                    unsafe { gc.auto_collect(self.roots(globals)) };
                    let len = usize::from(operand);
                    let elements = self.stack.drain(self.stack.len() - len..).collect();
                    let list = gc.allocate(List::new(elements));
                    self.push(RawValue::from(list));
                }
                Opcode::CreateDict => {
                    unsafe { gc.auto_collect(self.roots(globals)) };
                    let npairs = usize::from(operand);
                    let dict = Dict::new();
                    {
                        let mut pairs = self.stack.drain(self.stack.len() - npairs * 2..);
                        while let Some(key) = pairs.next() {
                            let value = pairs.next().unwrap();
                            dict.insert(key, value);
                        }
                    }
                    let dict = gc.allocate(dict);
                    self.push(RawValue::from(dict));
                }

                Opcode::AssignGlobal => {
                    let global_index = GlobalIndex::from_opr24(operand);
                    let value = self.stack_top();
                    globals.set(global_index, value);
                }
                Opcode::SinkGlobal => {
                    let global_index = GlobalIndex::from_opr24(operand);
                    let value = self.pop();
                    globals.set(global_index, value);
                }
                Opcode::GetGlobal => {
                    let global_index = GlobalIndex::from_opr24(operand);
                    let value = unsafe { globals.get_unchecked(global_index) };
                    self.push(value);
                }
                Opcode::AssignLocal => {
                    let slot = usize::from(operand);
                    let value = self.stack_top();
                    self.stack[self.stack_bottom + slot] = value;
                }
                Opcode::SinkLocal => {
                    let slot = usize::from(operand);
                    let value = self.pop();
                    self.stack[self.stack_bottom + slot] = value;
                }
                Opcode::GetLocal => {
                    let slot = usize::from(operand);
                    let value = self.stack[self.stack_bottom + slot];
                    self.push(value);
                }
                Opcode::AssignUpvalue => {
                    let index = usize::from(operand);
                    let closure = unsafe { self.closure.as_ref().unwrap_unchecked().get() };
                    let value = self.stack_top();
                    unsafe { Upvalue::set(&closure.captures[index], value) }
                }
                Opcode::SinkUpvalue => {
                    let index = usize::from(operand);
                    let value = self.pop();
                    let closure = unsafe { self.closure.as_ref().unwrap_unchecked().get() };
                    unsafe { Upvalue::set(&closure.captures[index], value) }
                }
                Opcode::GetUpvalue => {
                    let index = usize::from(operand);
                    let closure = unsafe { self.closure.as_ref().unwrap_unchecked().get() };
                    let value = unsafe { closure.captures[index].get() };
                    self.push(value);
                }
                Opcode::CloseLocal => {
                    let stack_slot = self.stack_bottom as u32 + u32::from(operand);
                    // This is O(n) and I can't say I'm a fan of that, but I haven't benchmarked the
                    // performance impact this makes yet.
                    let index = self
                        .open_upvalues
                        .iter()
                        .rposition(|(slot, _)| *slot == stack_slot)
                        .unwrap();
                    let (_, upvalue) = self.open_upvalues.remove(index);
                    unsafe { upvalue.close() };
                }
                Opcode::AssignField => {
                    let struct_v = self.pop();
                    let value = self.pop();
                    let struct_v = unsafe { struct_v.get_raw_struct_unchecked() };
                    self.push(value);
                    unsafe { struct_v.get().set_field(usize::from(operand), value) }
                }
                Opcode::SinkField => {
                    let struct_v = self.pop();
                    let value = self.pop();
                    let struct_v = unsafe { struct_v.get_raw_struct_unchecked() };
                    unsafe { struct_v.get().set_field(usize::from(operand), value) }
                }
                Opcode::GetField => {
                    let struct_v = self.pop();
                    let struct_v = unsafe { struct_v.get_raw_struct_unchecked() };
                    let value = unsafe { struct_v.get().get_field(usize::from(operand)) };
                    self.push(value);
                }

                Opcode::Swap => {
                    let len = self.stack.len();
                    self.stack.swap(len - 2, len - 1);
                }
                Opcode::Discard => {
                    self.pop();
                }

                Opcode::JumpForward => {
                    let amount = usize::from(operand);
                    self.pc += amount;
                }
                Opcode::JumpForwardIfFalsy => {
                    let amount = usize::from(operand);
                    if self.stack_top().is_falsy() {
                        self.pc += amount;
                    }
                }
                Opcode::JumpForwardIfTruthy => {
                    let amount = usize::from(operand);
                    if self.stack_top().is_truthy() {
                        self.pc += amount;
                    }
                }
                Opcode::JumpBackward => {
                    let amount = usize::from(operand);
                    self.pc -= amount;
                }

                Opcode::EnterBreakableBlock => {
                    self.breakable_block_stack.push(self.stack.len());
                }
                Opcode::ExitBreakableBlock => {
                    let result = self.pop();
                    let n = u32::from(operand);
                    for _ in 0..n {
                        unsafe {
                            let top = self.breakable_block_stack.pop().unwrap_unchecked();
                            while self.stack.len() > top {
                                self.stack.pop().unwrap();
                            }
                        }
                    }
                    self.push(result);
                }

                Opcode::Call => {
                    // Add 1 to count in the called function itself, which is treated like an
                    // argument.
                    let argument_count = usize::from(operand) + 1;
                    let function = self.nth_from_top(argument_count);
                    let closure = wrap_error!(function.ensure_raw_function());
                    self.enter_function(env, globals, gc, closure, argument_count)?;
                }
                Opcode::CallMethod => {
                    let (method_index, argument_count) = operand.unpack();
                    let method_index = MethodIndex::from_u16(method_index);
                    let receiver = self.nth_from_top(argument_count as usize);
                    let dtable = Self::get_dispatch_table(receiver, env);
                    #[cfg(feature = "trace-vm-calls")]
                    {
                        println!(
                            "call # m. idx={}, argc={}; {:?}",
                            method_index,
                            argument_count,
                            env.get_method_signature(method_index)
                        );
                    }
                    if let Some(closure) = dtable.get_method(method_index) {
                        self.enter_function(env, globals, gc, closure, argument_count as usize)?;
                    } else {
                        let signature = env
                            .get_method_signature(method_index)
                            .map(|signature| signature.render(env))
                            .unwrap_or_else(RenderedSignature::invalid);
                        let error_kind = ErrorKind::MethodDoesNotExist {
                            type_name: Rc::clone(&dtable.pretty_name),
                            signature,
                        };
                        return Err(self.error_outside_function_call(None, env, error_kind));
                    }
                }
                Opcode::Return => {
                    let result = self.pop();
                    self.restore_return_point();
                    self.push(result);
                }

                Opcode::Implement => {
                    let prototype_index = PrototypeIndex::from_opr24(operand);
                    let proto = unsafe { env.get_prototype_unchecked(prototype_index) };
                    let struct_position = proto.implemented_trait_count as usize + 1;

                    let traits: Vec<_> = {
                        // TODO: Maybe get rid of this allocation, or hoist it into the fiber?
                        let mut traits = vec![];
                        for trait_value in &self.stack[self.stack.len() - struct_position + 1..] {
                            traits.push(wrap_error!(trait_value.ensure_raw_trait()));
                        }
                        traits
                    };
                    let mut unimplemented_trait_methods: HashSet<_> = traits
                        .iter()
                        .enumerate()
                        .map(|(trait_index, trait_handle)| {
                            (trait_index, unsafe { trait_handle.get() }.id)
                        })
                        .flat_map(|(_, trait_id)| {
                            env.get_trait(trait_id).unwrap().required.iter().copied()
                        })
                        .collect();

                    let impld_struct =
                        wrap_error!(self.nth_from_top(struct_position).ensure_raw_struct());
                    let impld_struct = unsafe { impld_struct.get() };
                    let type_name = Rc::clone(&unsafe { impld_struct.dtable() }.type_name);

                    let mut type_dtable = DispatchTable::new_for_type(Rc::clone(&type_name));
                    self.initialize_dtable(
                        proto.statics.iter().map(|(&k, &v)| (k, v)),
                        env,
                        gc,
                        &mut type_dtable,
                    );

                    let mut instance_dtable = DispatchTable::new_for_instance(type_name);
                    self.initialize_dtable(
                        proto.instance.iter().map(|(&k, &v)| (k, v)),
                        env,
                        gc,
                        &mut instance_dtable,
                    );
                    wrap_error!(self.initialize_dtable_with_trait_methods(
                        proto.trait_instance.iter().map(
                            |((name, arity, trait_index), &function_index)| {
                                (Rc::clone(name), *arity, *trait_index, function_index)
                            },
                        ),
                        &traits,
                        env,
                        gc,
                        &mut instance_dtable,
                        &mut unimplemented_trait_methods,
                    ));

                    let instance_dtable = gc.allocate(instance_dtable);
                    type_dtable.instance = Some(instance_dtable);
                    let type_dtable = gc.allocate(type_dtable);

                    for _ in 0..proto.implemented_trait_count {
                        let _ = self.pop();
                    }

                    impld_struct
                        .implement(type_dtable)
                        .map_err(|kind| self.error_outside_function_call(None, env, kind))?;
                }

                Opcode::Negate => {
                    let number = wrap_error!(self.pop().ensure_number());
                    self.push(RawValue::from(-number));
                }
                Opcode::Add => binary_operator!(+),
                Opcode::Subtract => binary_operator!(-),
                Opcode::Multiply => binary_operator!(*),
                Opcode::Divide => binary_operator!(/),

                Opcode::Not => {
                    let value = self.stack_top();
                    *self.stack_top_mut() = RawValue::from(!value.is_truthy());
                }
                Opcode::Equal => {
                    let right = self.pop();
                    let left = self.stack_top();
                    *self.stack_top_mut() = RawValue::from(left.eq(&right));
                }
                Opcode::Less => {
                    let right = self.pop();
                    let left = self.stack_top();
                    let is_less = if let Some(ordering) = wrap_error!(left.try_partial_cmp(&right))
                    {
                        ordering.is_lt()
                    } else {
                        false
                    };
                    *self.stack_top_mut() = RawValue::from(is_less);
                }
                Opcode::LessEqual => {
                    let right = self.pop();
                    let left = self.stack_top();
                    let is_less = if let Some(ordering) = wrap_error!(left.try_partial_cmp(&right))
                    {
                        ordering.is_le()
                    } else {
                        false
                    };
                    *self.stack_top_mut() = RawValue::from(is_less);
                }

                Opcode::Halt => {
                    self.halted = true;
                    break;
                }
            }
        }

        let result = self.stack.pop().expect("no result found on the top of the stack");

        self.stack.resize(
            self.stack.len() - self.chunk.preallocate_stack_slots as usize,
            RawValue::from(()),
        );

        Ok(result)
    }
}

impl fmt::Debug for Fiber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Fiber").finish_non_exhaustive()
    }
}
