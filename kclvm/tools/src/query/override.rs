use std::collections::{HashMap, HashSet};

use anyhow::{anyhow, Result};

use kclvm_ast::walker::MutSelfMutWalker;
use kclvm_ast::{ast, walk_if_mut};
use kclvm_parser::parse_expr;

use crate::printer::print_ast_module;

use super::util::{invalid_spec_error, split_field_path};

/// Import statement column offset always start with 1.
/// todo: The (1-based) column offset needs to be constrained by specifications.
const IMPORT_STMT_COLUMN_OFFSET: u64 = 1;

/// Apply overrides on the AST program with the override specifications.
///
/// Please note that this a low level internal API used by compiler itself,
/// The parameters of the method are all compiler internal concepts such as
/// AST, etc.
pub fn apply_overrides(
    prog: &mut ast::Program,
    overrides: &[ast::OverrideSpec],
    import_paths: &[String],
) -> Result<()> {
    for o in overrides {
        let pkgpath = if o.pkgpath.is_empty() {
            &prog.main
        } else {
            &o.pkgpath
        };
        if let Some(modules) = prog.pkgs.get_mut(pkgpath) {
            for m in modules.iter_mut() {
                if apply_override_on_module(m, o, import_paths)? {
                    let code_str = print_ast_module(m);
                    std::fs::write(&m.filename, &code_str)?
                }
            }
        }
    }
    Ok(())
}

/// Apply overrides on the AST module with the override specifications.
///
/// Please note that this a low level internal API used by compiler itself,
/// The parameters of the method are all compiler internal concepts such as
/// AST, etc.
pub fn apply_override_on_module(
    m: &mut ast::Module,
    o: &ast::OverrideSpec,
    import_paths: &[String],
) -> Result<bool> {
    if !import_paths.is_empty() {
        let mut exist_import_set: HashSet<String> = HashSet::new();
        for stmt in &m.body {
            if let ast::Stmt::Import(import_stmt) = &stmt.node {
                exist_import_set.insert(import_stmt.path.to_string());
            }
        }
        for (i, path) in import_paths.iter().enumerate() {
            let line: u64 = i as u64 + 1;
            if exist_import_set.contains(path) {
                continue;
            }
            let name = path
                .split('.')
                .last()
                .ok_or_else(|| anyhow!("Invalid import path {}", path))?;
            let import_node = ast::ImportStmt {
                path: path.to_string(),
                rawpath: "".to_string(),
                name: name.to_string(),
                asname: None,
            };
            let import_stmt = Box::new(ast::Node::new(
                ast::Stmt::Import(import_node),
                m.filename.clone(),
                line,
                IMPORT_STMT_COLUMN_OFFSET,
                line,
                ("import ".len() + path.len()) as u64,
            ));
            m.body.insert((line - 1) as usize, import_stmt)
        }
    }
    let ss = o.field_path.split('.').collect::<Vec<&str>>();
    if ss.len() > 1 {
        let target_id = ss[0];
        let field = ss[1..].join(".");
        let value = &o.field_value;
        let key = ast::Identifier {
            names: field.split('.').map(|s| s.to_string()).collect(),
            ctx: ast::ExprContext::Store,
            pkgpath: "".to_string(),
        };
        // When there is a multi-target assignment statement of the form `a = b = Config {}`,
        // it needs to be transformed into the following form first to prevent the configuration
        // from being incorrectly modified.
        // ```kcl
        // a = Config {}
        // b = Config {}
        // ```
        fix_multi_assign(m);
        let val = build_expr_from_string(value);
        let mut transformer = OverrideTransformer {
            target_id: target_id.to_string(),
            field_path: field,
            override_key: key,
            override_value: val,
            override_target_count: 0,
            has_override: false,
            action: o.action.clone(),
        };
        transformer.walk_module(m);
        return Ok(transformer.has_override);
    }
    Ok(false)
}

/// Parse override spec string to override structure.
///
/// parse_override_spec("alice.age=10") -> ast::OverrideSpec {
///     pkgpath: "".to_string(),
///     field_path: "alice.age".to_string(),
///     field_value: "10".to_string(),
///     action: ast::OverrideAction::CreateOrUpdate,
/// }
pub(crate) fn parse_override_spec(spec: &str) -> Result<ast::OverrideSpec> {
    if spec.contains('=') {
        // Create or update the override value.
        let split_values = spec.splitn(2, '=').collect::<Vec<&str>>();
        let path = split_values
            .get(0)
            .ok_or_else(|| invalid_spec_error(spec))?;
        let field_value = split_values
            .get(1)
            .ok_or_else(|| invalid_spec_error(spec))?;
        let (pkgpath, field_path) = split_field_path(path)?;
        Ok(ast::OverrideSpec {
            pkgpath,
            field_path,
            field_value: field_value.to_string(),
            action: ast::OverrideAction::CreateOrUpdate,
        })
    } else if let Some(stripped_spec) = spec.strip_suffix('-') {
        // Delete the override value.
        let (pkgpath, field_path) = split_field_path(stripped_spec)?;
        Ok(ast::OverrideSpec {
            pkgpath,
            field_path,
            field_value: "".to_string(),
            action: ast::OverrideAction::Delete,
        })
    } else {
        Err(invalid_spec_error(spec))
    }
}

/// Build a expression from string.
fn build_expr_from_string(value: &str) -> ast::NodeRef<ast::Expr> {
    if value.is_empty() {
        Box::new(ast::Node::dummy_node(ast::Expr::StringLit(
            ast::StringLit {
                is_long_string: false,
                raw_value: "\"\"".to_string(),
                value: "".to_string(),
            },
        )))
    } else {
        let expr = parse_expr(value);
        match &expr.node {
            ast::Expr::Identifier(_) | ast::Expr::Binary(_) | ast::Expr::If(_) => Box::new(
                ast::Node::dummy_node(ast::Expr::StringLit(ast::StringLit {
                    is_long_string: false,
                    raw_value: format!("{:?}", value),
                    value: value.to_string(),
                })),
            ),
            _ => expr,
        }
    }
}

/// Transform AST and fix multi assign statement.
fn fix_multi_assign(m: &mut ast::Module) {
    let mut transformer = MultiAssignTransformer::default();
    transformer.walk_module(m);
    for (offset, (index, assign_stmt)) in transformer.multi_assign_mapping.iter().enumerate() {
        let insert_index = index + offset;
        let pos = match m.body.get(insert_index) {
            Some(stmt) => stmt.pos().clone(),
            None => bug!("AST module body index {} out of bound", insert_index),
        };
        m.body.insert(
            insert_index,
            Box::new(ast::Node::node_with_pos(
                ast::Stmt::Assign(assign_stmt.clone()),
                pos,
            )),
        );
    }
}

/// MultiAssignTransformer is used to transform AST Module and split top level
/// multiple target assign statement to multiple assign statements
///
/// Before
///
/// ```kcl
/// a = b = Config {}
/// ```
///
/// After
/// ```kcl
/// a = Config {}
/// b = Config {}
/// ```
#[derive(Debug, Default)]
struct MultiAssignTransformer {
    pub multi_assign_mapping: HashMap<usize, ast::AssignStmt>,
    pub index: usize,
}

impl<'ctx> MutSelfMutWalker<'ctx> for MultiAssignTransformer {
    fn walk_assign_stmt(&mut self, assign_stmt: &'ctx mut ast::AssignStmt) {
        self.index += 1;
        if assign_stmt.targets.len() <= 1 {
            return;
        }
        for target in &assign_stmt.targets[1..] {
            let mut new_assign_stmt = assign_stmt.clone();
            new_assign_stmt.targets = vec![target.clone()];
            self.multi_assign_mapping
                .insert(self.index, new_assign_stmt);
        }
        assign_stmt.targets = vec![assign_stmt.targets[0].clone()];
    }
    fn walk_if_stmt(&mut self, _: &'ctx mut ast::IfStmt) {
        // Do not fix AssignStmt in IfStmt
    }
    fn walk_schema_stmt(&mut self, _: &'ctx mut ast::SchemaStmt) {
        // Do not fix AssignStmt in SchemaStmt
    }
    fn walk_lambda_expr(&mut self, _: &'ctx mut ast::LambdaExpr) {
        // Do not fix AssignStmt in LambdaExpr
    }
}

/// OverrideTransformer is used to walk AST and transform it with the override values.
struct OverrideTransformer {
    pub target_id: String,
    pub field_path: String,
    pub override_key: ast::Identifier,
    pub override_value: ast::NodeRef<ast::Expr>,
    pub override_target_count: usize,
    pub has_override: bool,
    pub action: ast::OverrideAction,
}

impl<'ctx> MutSelfMutWalker<'ctx> for OverrideTransformer {
    fn walk_unification_stmt(&mut self, unification_stmt: &'ctx mut ast::UnificationStmt) {
        let name = match unification_stmt.target.node.names.get(0) {
            Some(name) => name,
            None => bug!(
                "Invalid AST unification target names {:?}",
                unification_stmt.target.node.names
            ),
        };
        if name != &self.target_id {
            return;
        }
        self.override_target_count = 1;
        self.has_override = true;
        self.walk_schema_expr(&mut unification_stmt.value.node);
    }

    fn walk_assign_stmt(&mut self, assign_stmt: &'ctx mut ast::AssignStmt) {
        if let ast::Expr::Schema(_) = &assign_stmt.value.node {
            self.override_target_count = 0;
            for target in &assign_stmt.targets {
                if target.node.names.len() != 1 {
                    continue;
                }
                if target.node.names[0] != self.target_id {
                    continue;
                }
                self.override_target_count += 1;
            }
            if self.override_target_count == 0 {
                return;
            }
            self.has_override = true;
            self.walk_expr(&mut assign_stmt.value.node);
        }
    }

    fn walk_schema_expr(&mut self, schema_expr: &'ctx mut ast::SchemaExpr) {
        if self.override_target_count == 0 {
            return;
        }
        if !self.lookup_schema_config_and_replace(schema_expr) {
            // Not exist and append an override value when the action is CREATE_OR_UPDATE
            if let ast::OverrideAction::CreateOrUpdate = self.action {
                if let ast::Expr::Config(config_expr) = &mut schema_expr.config.node {
                    config_expr
                        .items
                        .push(Box::new(ast::Node::dummy_node(ast::ConfigEntry {
                            key: Some(Box::new(ast::Node::dummy_node(ast::Expr::Identifier(
                                self.override_key.clone(),
                            )))),
                            value: self.override_value.clone(),
                            operation: ast::ConfigEntryOperation::Override,
                            insert_index: -1,
                        })));
                }
            }
        }
        self.override_target_count = 0;
    }

    fn walk_config_expr(&mut self, config_expr: &'ctx mut ast::ConfigExpr) {
        for config_entry in config_expr.items.iter_mut() {
            walk_if_mut!(self, walk_expr, config_entry.node.key);
            self.walk_expr(&mut config_entry.node.value.node);
        }
    }

    fn walk_if_stmt(&mut self, _: &'ctx mut ast::IfStmt) {
        // Do not override AssignStmt in IfStmt
    }
    fn walk_schema_stmt(&mut self, _: &'ctx mut ast::SchemaStmt) {
        // Do not override AssignStmt in SchemaStmt
    }
    fn walk_lambda_expr(&mut self, _: &'ctx mut ast::LambdaExpr) {
        // Do not override AssignStmt in LambdaExpr
    }
}

impl OverrideTransformer {
    /// Get all field paths from AST nodes including schema and config.
    #[inline]
    fn get_field_paths(&mut self, expr: &mut ast::NodeRef<ast::Expr>) -> Vec<String> {
        match &mut expr.node {
            ast::Expr::Schema(schema_expr) => self.get_schema_config_field_paths(schema_expr),
            ast::Expr::Config(config_expr) => self.get_config_field_paths(config_expr),
            _ => vec![],
        }
    }

    /// Get all field paths from a schema AST node.
    fn get_schema_config_field_paths(&mut self, schema_expr: &mut ast::SchemaExpr) -> Vec<String> {
        if let ast::Expr::Config(config_expr) = &mut schema_expr.config.node {
            self.get_config_field_paths(config_expr)
        } else {
            vec![]
        }
    }

    /// Get all field paths from a config AST node.
    fn get_config_field_paths(&mut self, config: &mut ast::ConfigExpr) -> Vec<String> {
        let mut paths = vec![];
        for entry in config.items.iter_mut() {
            let mut entry_paths = self.get_entry_paths(&mut entry.node);
            paths.append(&mut entry_paths);
        }
        paths
    }

    /// Get all field paths from a config entry.
    fn get_entry_paths(&mut self, entry: &mut ast::ConfigEntry) -> Vec<String> {
        let mut paths = vec![];
        let path = self.get_path_from_key(&entry.key);
        if !path.is_empty() {
            paths.push(path.clone());
            let value_paths = self.get_field_paths(&mut entry.value);
            if !value_paths.is_empty() {
                paths.append(
                    &mut value_paths
                        .iter()
                        .map(|value_path| format!("{}.{}", path, value_path))
                        .collect::<Vec<String>>(),
                );
            }
        }
        paths
    }

    /// Get config key path from the AST key node.
    #[inline]
    fn get_path_from_key(&mut self, key: &Option<ast::NodeRef<ast::Expr>>) -> String {
        match key {
            Some(key) => match &key.node {
                ast::Expr::Identifier(identifier) => identifier.get_name(),
                ast::Expr::StringLit(string_lit) => string_lit.value.clone(),
                _ => "".to_string(),
            },
            None => "".to_string(),
        }
    }
}

/// OverrideNode is used to place different AST configuration syntax structures.
#[derive(Debug)]
enum OverrideNode<'a> {
    Schema(&'a mut ast::SchemaExpr),
    Config(&'a mut ast::ConfigExpr),
}

impl OverrideTransformer {
    /// Lookup schema config all fields and replace if it is matched with the override spec,
    /// return whether is found a replaced one.
    fn lookup_schema_config_and_replace(&mut self, schema_expr: &mut ast::SchemaExpr) -> bool {
        let paths = self.get_schema_config_field_paths(schema_expr);
        match paths.iter().position(|r| r == &self.field_path) {
            Some(pos) => {
                let mut config = OverrideNode::Schema(schema_expr);
                self.replace_node_with_path(&mut config, &paths[pos]);
                true
            }
            None => false,
        }
    }

    /// Replace AST node with path including schema_expr and config_expr.
    fn replace_node_with_path(&mut self, config: &mut OverrideNode, path: &str) {
        // Do not replace empty path parts on the config expression.
        if path.is_empty() {
            return;
        }
        // Split a path into multiple parts. `a.b.c` -> ["a", "b", "c"]
        let parts = path.split('.').collect::<Vec<&str>>();
        for i in 0..parts.len() {
            match config {
                OverrideNode::Schema(schema_expr) => {
                    if let ast::Expr::Config(config_expr) = &mut schema_expr.config.node {
                        self.replace_config_expr_with_path_parts(config_expr, &parts, i);
                    }
                }
                OverrideNode::Config(config_expr) => {
                    self.replace_config_expr_with_path_parts(config_expr, &parts, i);
                }
            }
        }
    }

    /// Replace AST config expr with one part of path.
    fn replace_config_expr_with_path_parts(
        &mut self,
        config_expr: &mut ast::ConfigExpr,
        parts: &[&str],
        start_index: usize,
    ) {
        // Do not replace empty path parts and out of index parts on the config expression.
        if parts.is_empty() || start_index >= parts.len() {
            return;
        }
        let part = parts[start_index];
        let mut delete_index_set = HashSet::new();
        for (i, item) in config_expr.items.iter_mut().enumerate() {
            let path = self.get_path_from_key(&item.node.key);
            if path == part {
                match self.action {
                    ast::OverrideAction::CreateOrUpdate => {
                        // When the last part of the path is successfully recursively matched, 
                        // it indicates that the original value that needs to be overwritten 
                        // is successfully found, and the new value is used to overwrite it.
                        if start_index == 0 && parts.len() == 1 {
                            self.override_value.set_pos(item.pos());
                            // Override the node value.
                            item.node.value = self.override_value.clone();
                        }
                        // Replace value recursively using the path composed by subsequent parts.
                        let path = &parts[start_index + 1..].join(".");
                        match &mut item.node.value.node {
                            ast::Expr::Schema(schema_expr) => {
                                let mut config = OverrideNode::Schema(schema_expr);
                                self.replace_node_with_path(&mut config, path);
                            }
                            ast::Expr::Config(config_expr) => {
                                let mut config = OverrideNode::Config(config_expr);
                                self.replace_node_with_path(&mut config, path);
                            }
                            _ => {}
                        }
                    }
                    ast::OverrideAction::Delete => {
                        delete_index_set.insert(i);
                    }
                }
            }
        }
        if !delete_index_set.is_empty() {
            let items: Vec<(usize, &ast::NodeRef<ast::ConfigEntry>)> = config_expr
                .items
                .iter()
                .enumerate()
                .filter(|(i, _)| !delete_index_set.contains(i))
                .collect();
            config_expr.items = items
                .iter()
                .map(|(_, item)| <&ast::NodeRef<ast::ConfigEntry>>::clone(item).clone())
                .collect();
        }
    }
}
