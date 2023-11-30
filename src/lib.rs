use rslua::{
    ast::{Assignable, BinOp, Block, Expr, IfStat, Suffix},
    lexer::{Lexer, LexerConfig},
    parser::Parser,
};

pub struct StScriptWriter {
    output: String,
    temp_var_counter: i32,
}

const BUILTINS: [&'static str; 3] = ["echo", "round", "input"];

impl StScriptWriter {
    pub fn run_code(lua_code: String) -> String {
        let mut lexer = Lexer::default();
        lexer.set_config(LexerConfig {
            use_origin_string: false,
            reserve_comments: false,
        });
        let mut lexer = Lexer::default();
        lexer.set_config(LexerConfig {
            use_origin_string: false,
            reserve_comments: false,
        });
        StScriptWriter::new()
            .run(
                &Parser::default()
                    .run(lexer.run(&lua_code).unwrap())
                    .unwrap(),
                false,
            )
            .to_string()
    }

    pub fn new() -> Self {
        StScriptWriter {
            output: String::new(),
            temp_var_counter: 0,
        }
    }

    pub fn run(&mut self, block: &Block, nested: bool) -> &str {
        self.output.clear();
        for (i, stmt) in block.stats.iter().enumerate() {
            match stmt {
                rslua::ast::Stat::IfStat(s) if !nested => self.if_statement(s),
                rslua::ast::Stat::WhileStat(_) => unimplemented!("while"),
                rslua::ast::Stat::DoBlock(_) => unimplemented!("do"),
                rslua::ast::Stat::ForStat(_) => unimplemented!("for"),
                rslua::ast::Stat::RepeatStat(_) => unimplemented!("repeat"),
                rslua::ast::Stat::FuncStat(_) => unimplemented!("func"),
                rslua::ast::Stat::LocalStat(_) => unimplemented!("local"),
                rslua::ast::Stat::LabelStat(_) => unimplemented!("label"),
                rslua::ast::Stat::RetStat(_) => unimplemented!("ret"),
                rslua::ast::Stat::BreakStat(_) => {
                    self.output.push_str("/break ");
                }
                rslua::ast::Stat::GotoStat(_) => unimplemented!("goto"),
                rslua::ast::Stat::AssignStat(s) => self.assign_statment(s),
                rslua::ast::Stat::CallStat(s) => self.call_statement(&s.call),
                _ => unreachable!(),
            }
            if i != block.stats.len() - 1 {
                if !nested {
                    self.output.push_str("| ");
                } else {
                    self.output.push_str(r" \| ");
                }
            }
        }
        &self.output
    }

    pub fn primitive_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Nil(_) => "\"\"".to_string(),
            Expr::True(_) => "true".to_string(),
            Expr::False(_) => "false".to_string(),
            Expr::VarArg(_) => todo!(),
            Expr::Float(f) => f.value().to_string(),
            Expr::Int(i) => i.value().to_string(),
            Expr::String(s) => s.value().to_string(),
            Expr::Name(_) => todo!(),
            Expr::ParenExpr(_) => todo!(),
            Expr::FuncBody(_) => todo!(),
            Expr::Table(_) => todo!(),
            Expr::BinExpr(b) => {
                if let BinOp::Concat(_) = b.op {
                    let mut str_builder = String::new();
                    match b.left.as_ref() {
                        Expr::String(s) => str_builder.push_str(&s.value()),
                        Expr::Name(n) => str_builder.push_str(&n.value()),
                        _ => str_builder.push_str(&format!(
                            "{{{{getvar::{}}}}}",
                            self.set_temp_var(b.left.as_ref())
                        )),
                    }
                    match b.right.as_ref() {
                        Expr::String(s) => str_builder.push_str(&s.value()),
                        Expr::Name(n) => str_builder.push_str(&n.value()),
                        _ => str_builder.push_str(&format!(
                            "{{{{getvar::{}}}}}",
                            self.set_temp_var(b.right.as_ref())
                        )),
                    }
                    return str_builder;
                };
                if let BinOp::Mul(_) = b.op {
                    let lhs = match b.left.as_ref() {
                        Expr::String(s) => s.value(),
                        Expr::Name(n) => n.value(),
                        _ => format!("{{{{getvar::{}}}}}", self.set_temp_var(b.left.as_ref())),
                    };
                    let rhs = match b.right.as_ref() {
                        Expr::String(s) => s.value(),
                        Expr::Name(n) => n.value(),
                        _ => format!("{{{{getvar::{}}}}}", self.set_temp_var(b.right.as_ref())),
                    };
                    return format!("/mul {} {}", lhs, rhs);
                };
                dbg!(b);
                todo!()
            }
            Expr::UnExpr(_) => todo!(),
            Expr::SuffixedExpr(s) => {
                dbg!(s);
                self.suffixed_call(s)
            }
        }
    }

    pub fn suffixed_call(&mut self, se: &rslua::ast::SuffixedExpr) -> String {
        let mut str_builder = String::new();
        let name = if let Expr::Name(s) = se.primary.as_ref() {
            s.value()
        } else {
            unimplemented!()
        };
        if BUILTINS.contains(&name.as_str()) {
            str_builder.push_str(&format!("/{} ", name));
            str_builder.push_str(
                match se.suffixes.first().unwrap() {
                    Suffix::FuncArgs(a) => self.call_args(a),
                    _ => unimplemented!(),
                }
                .as_str(),
            );
        } else {
            str_builder.push_str(&format!("/run {} ", name));
        }
        str_builder
    }

    pub fn call_statement(&mut self, stmt: &Assignable) {
        match stmt {
            Assignable::Name(_) => todo!(),
            Assignable::SuffixedExpr(se) => {
                let call = self.suffixed_call(se);
                self.output.push_str(&call);
            }
        }
    }

    pub fn call_args(&mut self, args: &rslua::ast::FuncArgs) -> String {
        match args {
            rslua::ast::FuncArgs::Exprs(_, es, _) => es
                .exprs
                .iter()
                .map(|e| {
                    if let Expr::BinExpr(_) = e {
                        let name = self.set_temp_var(e);
                        format!("{{{{getvar::{}}}}}", name)
                    } else {
                        self.primitive_expr(e)
                    }
                })
                .collect::<Vec<String>>()
                .join(" "),
            rslua::ast::FuncArgs::Table(_) => unimplemented!(),
            rslua::ast::FuncArgs::String(s) => s.value().to_string(),
        }
    }

    pub fn if_statement(&mut self, stmt: &IfStat) {
        let cond = &stmt.cond_blocks.first().unwrap().cond;
        let (lhs, op, rhs) = match cond {
            Expr::BinExpr(expr) => (&expr.left, &expr.op, &expr.right),
            _ => todo!(),
        };
        let rule = match op {
            rslua::ast::BinOp::Eq(_) => "eq",
            rslua::ast::BinOp::Ne(_) => todo!(),
            rslua::ast::BinOp::Lt(_) => todo!(),
            rslua::ast::BinOp::Gt(_) => todo!(),
            rslua::ast::BinOp::Le(_) => todo!(),
            rslua::ast::BinOp::Ge(_) => todo!(),
            _ => todo!(),
        };
        let block = &stmt.cond_blocks.first().unwrap().block;
        let else_block = &stmt.else_block;
        let lhs = self.set_temp_var(lhs);
        let rhs = self.set_temp_var(rhs);

        self.output.push_str(&format!(
            "/if left={{{{getvar::{}}}}} right={{{{getvar::{}}}}} rule={} ",
            lhs, rhs, rule,
        ));
        let mut writer = StScriptWriter::new();
        if let Some(else_block) = else_block {
            self.output
                .push_str(&format!("else=\"{}\" ", writer.run(else_block, true)));
        }
        self.output
            .push_str(&format!("\"{}\"", writer.run(block, true)));
    }

    pub fn set_temp_var(&mut self, value: &Expr) -> String {
        let name = format!("temp_{}", self.temp_var_counter);
        self.temp_var_counter += 1;
        let val = match value {
            Expr::Nil(_) => "\"\"".to_string(),
            Expr::True(_) => "true".to_string(),
            Expr::False(_) => "false".to_string(),
            Expr::VarArg(_) => todo!(),
            Expr::Float(t) => t.value().to_string(),
            Expr::Int(t) => t.value().to_string(),
            Expr::String(t) => t.value(),
            Expr::Name(t) => t.value(),
            Expr::ParenExpr(_) => todo!(),
            Expr::FuncBody(_) => todo!(),
            Expr::Table(_) => todo!(),
            Expr::BinExpr(_) => {
                let val = self.primitive_expr(value);
                self.output
                    .push_str(&format!("{} | setvar key={} | ", val, name));
                return name;
            }
            Expr::UnExpr(_) => todo!(),
            Expr::SuffixedExpr(t) => {
                // assume this is a function call
                // do /function xyz | /setvar key={}
                let call = self.suffixed_call(t);

                self.output
                    .push_str(&format!("{} | /setvar key={} | ", call, name));
                return name;
            }
        };
        self.output
            .push_str(&format!("/setvar key={} {} | ", name, val));
        name
    }

    fn assign_statment(&mut self, s: &rslua::ast::AssignStat) {
        let name = if let Assignable::Name(n) = s.left.assignables.first().unwrap() {
            n.value()
        } else {
            unimplemented!()
        };
        let value = self.primitive_expr(s.right.exprs.first().unwrap());
        self.output
            .push_str(&format!("/setvar key={} {} ", name, value));
    }
}

#[cfg(test)]
mod tests {
    use crate::StScriptWriter;

    macro_rules! testdata {
        ($name:expr) => {
            std::fs::read_to_string(concat!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/testdata/",
                $name
            )))
            .unwrap()
        };
    }

    #[test]
    fn parse_circle_area() {
        let lua_code = testdata!("circle_area.lua");
        insta::assert_snapshot!(StScriptWriter::run_code(lua_code));
    }

    #[test]
    fn parse_factorial() {
        let lua_code = testdata!("factorial.lua");
        insta::assert_snapshot!(StScriptWriter::run_code(lua_code));
    }

    #[test]
    fn parse_if() {
        let lua_code = testdata!("if.lua");
        insta::assert_snapshot!(StScriptWriter::run_code(lua_code));
    }
}
