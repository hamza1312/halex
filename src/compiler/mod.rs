pub mod mir;
mod types;

use std::{collections::HashMap, path::Path};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue},
    AddressSpace, OptimizationLevel,
};

use crate::args::HalexArgs;

use self::{
    mir::{Literal, Mir, Type},
    types::{Value, Variable},
};

pub struct Compiler<'a> {
    pub(crate) ir: Vec<Mir>,
    pub(crate) target_machine: TargetMachine,
    pub(crate) module: Module<'a>,
    pub(crate) builder: Builder<'a>,
    pub(crate) context: &'a Context,
    pub(crate) args: &'a HalexArgs,
    pub(crate) fn_map: HashMap<String, FunctionValue<'a>>,
    pub(crate) main_fn: FunctionValue<'a>,
    pub(crate) variables: Vec<Variable<'a>>,
}

impl<'a> Compiler<'a> {
    pub fn new(ir: Vec<Mir>, context: &'a Context, args: &'a HalexArgs) -> Self {
        let module = context.create_module(args.path.to_str().unwrap_or("halex"));
        let builder = context.create_builder();
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let optimization = match args.opt {
            0 => OptimizationLevel::None,
            1 => OptimizationLevel::Less,
            2 => OptimizationLevel::Default,
            3 => OptimizationLevel::Aggressive,
            _ => {
                eprintln!("Invalidoptimization level");
                std::process::exit(1);
            }
        };
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                optimization,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();
        let main_fn_ty = context.i32_type().fn_type(&[], false);
        let main_fn = module.add_function("main", main_fn_ty, None);
        let entry = context.append_basic_block(main_fn, "entry");

        builder.position_at_end(entry);
        Self {
            builder,
            target_machine,
            variables: Vec::new(),
            args,
            fn_map: HashMap::new(),
            main_fn,
            context,
            module,
            ir,
        }
    }
    fn compile_literal<'f>(&mut self, literal: &'f Literal) -> Value<'a> {
        match literal {
            Literal::Int(i) => Value::Int(self.context.i64_type().const_int(*i as u64, false)),
            Literal::Float(f) => Value::Float(self.context.f64_type().const_float(*f)),

            Literal::Str(s) => Value::Pointer(
                self.builder
                    .build_global_string_ptr(&s, &s)
                    .unwrap()
                    .as_pointer_value(),
            ),
            Literal::Bool(b) => Value::Bool(self.context.bool_type().const_int(*b as u64, false)),
            Literal::Unit => Value::None(
                self.context
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .const_null(),
            ),
        }
    }
    fn compile_expr<'f>(&mut self, expr: &'f Mir) -> Value<'a> {
        match expr {
            Mir::Lit(lit) => self.compile_literal(lit),
            Mir::Call(name, args) => {
                let f = *self.fn_map.get(name).expect("Function not found");
                let mut arguments = Vec::new();
                for arg in args {
                    arguments.push(BasicMetadataValueEnum::from(
                        self.compile_expr(arg).as_basic_value(),
                    ));
                }
                let return_value = self
                    .builder
                    .build_call(f, arguments.as_slice(), "call")
                    .unwrap()
                    .try_as_basic_value()
                    .left();
                match return_value {
                    Some(x) => match x {
                        BasicValueEnum::IntValue(i) => Value::Int(i),
                        BasicValueEnum::FloatValue(f) => Value::Float(f),
                        BasicValueEnum::PointerValue(p) => Value::Pointer(p),
                        _ => unimplemented!(),
                    },
                    None => self.null(),
                }
            }
            Mir::Let(index, ty, value) => {
                let ty = self.get_type(ty);
                let alloca = self.builder.build_alloca(ty, &index.to_string()).unwrap();
                let value = self.compile_expr(value);
                self.builder
                    .build_store(alloca, value.as_basic_value())
                    .unwrap();
                self.variables.insert(
                    *index,
                    Variable {
                        ptr: alloca,
                        var_type: ty,
                    },
                );
                self.null()
            }
            Mir::GetVar(index) => {
                let var = self.variables.get(*index).unwrap();
                Value::from(
                    self.builder
                        .build_load(var.var_type, var.ptr, &index.to_string())
                        .unwrap(),
                )
            }
            Mir::ExternFunction {
                name,
                params,
                return_type,
                is_varadic,
            } => {
                let mut param_types = Vec::new();
                for p in params {
                    param_types.push(self.get_type(&p).into())
                }

                let fn_type = self
                    .get_type(return_type)
                    .fn_type(param_types.as_slice(), *is_varadic);
                let function = self
                    .module
                    .add_function(&name, fn_type, Some(Linkage::External));
                self.fn_map.insert(name.into(), function);
                self.null()
            }
            Mir::Ref(x) => {
                let value = self.compile_expr(x.as_ref());
                let ptr = self
                    .builder
                    .build_alloca(value.as_basic_value().get_type(), "")
                    .expect("Couldn't make pointer");
                self.builder
                    .build_store(ptr, value.as_basic_value())
                    .unwrap();

                Value::Pointer(ptr)
            }
            Mir::Deref(v) => {
                let ptr = self.compile_expr(v.as_ref()).as_ptr();
                Value::from(
                    self.builder
                        .build_load(ptr.get_type(), ptr, "deref")
                        .unwrap(),
                )
            }
            _ => todo!(),
        }
    }

    fn get_type<'f>(&self, ty: &'f Type) -> BasicTypeEnum<'a> {
        use Type::*;
        match ty {
            I64 => self.context.i64_type().into(),
            I32 => self.context.i32_type().into(),
            F64 => self.context.f64_type().into(),
            F32 => self.context.f32_type().into(),
            Pointer(t) => {
                let t = self.get_type(t.as_ref());
                match t {
                    BasicTypeEnum::IntType(int_ty) => int_ty
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum(),
                    BasicTypeEnum::FloatType(float_ty) => float_ty
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum(),
                    BasicTypeEnum::PointerType(elem_ty) => elem_ty
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum(),
                    BasicTypeEnum::ArrayType(e) => {
                        e.ptr_type(AddressSpace::default()).as_basic_type_enum()
                    }
                    BasicTypeEnum::StructType(x) => {
                        x.ptr_type(AddressSpace::default()).as_basic_type_enum()
                    }
                    _ => unimplemented!(),
                }
            }
            Unit => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            Str => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            _ => todo!(),
        }
    }
    fn null(&self) -> Value<'a> {
        Value::None(
            self.context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .const_null(),
        )
    }

    pub fn compile(&mut self) {
        for x in self.ir.clone() {
            println!("{:?}", self.compile_expr(&x));
        }
    }
    pub fn finish(&mut self) {
        self.builder
            .position_at_end(self.main_fn.get_last_basic_block().unwrap());
        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0, false)))
            .unwrap();
        println!("{}", self.module.print_to_string().to_string());
        self.module.verify().unwrap();
        self.target_machine
            .write_to_file(
                &self.module,
                FileType::Object,
                &Path::new(&self.args.object),
            )
            .expect("Error while making the object file.");
    }
}
