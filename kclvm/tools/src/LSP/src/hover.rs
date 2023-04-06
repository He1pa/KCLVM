use indexmap::IndexSet;
use kclvm_ast::ast::Program;
use kclvm_error::Position as KCLPos;
use kclvm_sema::resolver::scope::ProgramScope;
use lsp_types::{Hover, HoverContents, MarkedString};

use crate::goto_def::find_definition_objs;

/// Returns a short text describing element at position.
/// Specifically, the doc for schema and schema attr(todo)
pub(crate) fn hover(
    program: &Program,
    kcl_pos: &KCLPos,
    prog_scope: &ProgramScope,
) -> Option<lsp_types::Hover> {
    match program.pos_to_stmt(kcl_pos) {
        Some(node) => match node.node {
            _ => {
                let objs = find_definition_objs(node, kcl_pos, prog_scope);
                let docs: IndexSet<String> = objs
                    .iter()
                    .filter(|obj| obj.ty.is_schema())
                    .map(|obj| obj.ty.into_schema_type().doc.clone())
                    .collect();
                docs_to_hover(docs)
            }
        },
        None => None,
    }
}

// Convert docs to Hover. This function will convert to
// None, Scalar or Array according to the number of positions
fn docs_to_hover(docs: IndexSet<String>) -> Option<lsp_types::Hover> {
    match docs.len() {
        0 => None,
        1 => Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(docs[0].clone())),
            range: None,
        }),
        _ => Some(Hover {
            contents: HoverContents::Array(
                docs.iter()
                    .map(|doc| MarkedString::String(doc.clone()))
                    .collect(),
            ),
            range: None,
        }),
    }
}
