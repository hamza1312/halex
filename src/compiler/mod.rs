pub mod mir;
use self::mir::{BinOp, Literal, Mir};
use crate::HalexArgs;
use cranelift::prelude::{settings::Flags, *};

use codegen::ir::FuncRef;
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule, ObjectProduct};
use std::{collections::HashMap, io::Write, process::Command};

pub struct Compiler {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    data: DataDescription,
    module: ObjectModule,
    ir: Vec<Mir>,
}
impl Compiler {
    pub fn compile_all(ir: Vec<Mir>, args: &HalexArgs) {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = ObjectBuilder::new(isa, "main", cranelift_module::default_libcall_names())
            .expect("Couldn't make the builder");
        let module = ObjectModule::new(builder);
        let mut compiler = Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data: DataDescription::new(),
            module,
            ir,
        };
        compiler.compile();

        // Now convert it to an object file
        let product = compiler.module.finish();
        let raw_data = product.emit().unwrap();
        // Write it to the <object> file
        let mut f = std::fs::File::create(args.object.clone()).unwrap();
        f.write_all(&raw_data).unwrap();
        Command::new("gcc")
            .arg(args.object.to_string())
            .arg("-o")
            .arg(args.object.replace(".o", ""))
            .spawn()
            .expect("Failed to execute gcc, made an object file instead");
    }
    fn translate(&mut self) {
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        let mut handler = Handler {
            module: &mut self.module,
            builder,
            functions: HashMap::new(),
            data: DataDescription::new(),
        };
        for expr in self.ir.iter() {
            handler.translate_expr(expr);
        }
        let zero = handler.builder.ins().iconst(types::I64, 0);
        handler.builder.ins().return_(&[zero]);
        handler.builder.finalize();
    }
    fn compile(&mut self) {
        self.translate();
        self.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(types::I64));
        let id = self
            .module
            .declare_function("main", Linkage::Export, &self.ctx.func.signature)
            .unwrap();
        self.module.define_function(id, &mut self.ctx).unwrap();

        self.data.clear();

        let mut s = String::new();

        s.clear();

        codegen::write_function(&mut s, &self.ctx.func).unwrap();
        println!("CLIF:\n{}", s);
        if let Err(e) = codegen::verify_function(
            &self.ctx.func,
            &Flags::new(cranelift::codegen::settings::builder()),
        ) {
            println!("codegen error");

            let mut s = String::new();

            s.clear();

            codegen::write_function(&mut s, &self.ctx.func).unwrap();
            println!("CLIF:\n{}", s);

            panic!("errors: {:#?}", e);
        }
    }
}

struct Handler<'a> {
    builder: FunctionBuilder<'a>,
    data: DataDescription,
    module: &'a mut ObjectModule,
    functions: HashMap<&'a str, FuncRef>,
}
impl<'a> Handler<'a> {
    fn translate_literal(&mut self, literal: &'a Literal) -> Value {
        match literal {
            Literal::Int(n) => self.builder.ins().iconst(types::I64, *n),
            Literal::Float(n) => self.builder.ins().f64const(*n),
            Literal::Str(string) => {
                let mut s = string.clone();
                s.push('\0');
                let boxed_s = s.clone().into_bytes().into_boxed_slice();
                let id = self.module.declare_anonymous_data(false, false).unwrap();

                self.data.define(boxed_s);
                self.module.define_data(id, &self.data).unwrap();
                self.data.clear();

                let value = self.module.declare_data_in_func(id, &mut self.builder.func);
                // Return the pointer
                self.builder.ins().global_value(types::I64, value)
            }
            Literal::Unit => self.builder.ins().iconst(types::I64, 0),

            _ => todo!(),
        }
    }
    fn translate_expr(&mut self, expr: &'a Mir) -> Value {
        match expr {
            Mir::Lit(lit) => self.translate_literal(lit),
            Mir::Call(name, argss) => {
                let mut args = Vec::new();
                for a in argss {
                    args.push(self.translate_expr(a));
                }
                let f = self.functions.get(name.as_str()).unwrap();
                let call = self.builder.ins().call(*f, args.as_slice());
                println!("{:?}", call);
                self.translate_literal(&Literal::Unit)
            }
            Mir::ExternFunction {
                name,
                params,
                return_type,
            } => {
                let mut sig = self.module.make_signature();
                for p in params {
                    sig.params.push(AbiParam::new(self.convert_type(&p)))
                }
                if return_type != &mir::Type::Unit {
                    sig.returns
                        .push(AbiParam::new(self.convert_type(&return_type)));
                }
                let callee = self
                    .module
                    .declare_function(&name, Linkage::Import, &sig)
                    .unwrap();
                let local_callee = self.module.declare_func_in_func(callee, self.builder.func);
                self.functions.insert(&name, local_callee);
                self.translate_literal(&Literal::Unit)
            }
            _ => todo!(),
        }
    }

    fn convert_type(&self, ty: &mir::Type) -> Type {
        use mir::Type::*;
        match ty {
            I64 | Str => types::I64,
            Unit => types::I64,
            I32 => types::I32,
            F64 => types::F64,
            F32 => types::F32,
        }
    }
}
