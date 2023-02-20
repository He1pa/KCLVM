use kclvm_error::Diagnostic as KCLDiagnostic;
use kclvm_error::Level;
use kclvm_error::Message;
use kclvm_error::Position as KCLPos;
use tower_lsp::lsp_types::*;

use kclvm_sema::resolver::scope::Scope;
use kclvm_sema::resolver::scope::ScopeObject;

#[derive(Debug)]
pub struct ImCompleteSemanticToken {
    pub start: KCLPos,
    /// The scope object end position.
    pub end: KCLPos,
    /// The scope object kind.
    pub kind: u32,
    pub length: u32,
}

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::STRUCT,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::PROPERTY,
];

/// Convert pos format
/// The position in lsp protocol is different with position in ast node whose line number is 1 based.
pub fn kcl_pos_to_lsp_pos(pos: &KCLPos) -> Position {
    Position {
        line: pos.line as u32 - 1,
        character: pos.column.unwrap_or(0) as u32,
    }
}

/// Convert KCL Message to LSP Diagnostic
fn kcl_msg_to_lsp_diags(msg: &Message, severity: DiagnosticSeverity) -> Diagnostic {
    let kcl_pos = msg.pos.clone();
    let start_position = kcl_pos_to_lsp_pos(&kcl_pos);
    let end_position = kcl_pos_to_lsp_pos(&kcl_pos);
    Diagnostic {
        range: Range::new(start_position, end_position),
        severity: Some(severity),
        code: None,
        code_description: None,
        source: None,
        message: msg.message.clone(),
        related_information: None,
        tags: None,
        data: None,
    }
}

fn kcl_err_level_to_severity(level: Level) -> DiagnosticSeverity {
    match level {
        Level::Error => DiagnosticSeverity::ERROR,
        Level::Warning => DiagnosticSeverity::WARNING,
        Level::Note => DiagnosticSeverity::HINT,
    }
}

/// Convert KCL Diagnostic to LSP Diagnostics.
/// Because the diagnostic of KCL contains multiple messages, and each messages corresponds to a diagnostic of LSP, the return value is a vec
pub fn kcl_diag_to_lsp_diags(diag: &KCLDiagnostic, file_name: &str) -> Vec<Diagnostic> {
    diag.messages
        .iter()
        .filter(|msg| msg.pos.filename == file_name)
        .map(|msg| kcl_msg_to_lsp_diags(msg, kcl_err_level_to_severity(diag.level)))
        .collect()
}

pub fn kcl_scopeobj_to_lsp_imcomplete_sema_token(
    obj: &ScopeObject,
) -> Option<ImCompleteSemanticToken> {
    match obj.kind {
        kclvm_sema::resolver::scope::ScopeObjectKind::Variable => Some(ImCompleteSemanticToken {
            start: obj.start.clone(),
            end: obj.end.clone(),
            kind: LEGEND_TYPE
                .iter()
                .position(|item| item == &SemanticTokenType::VARIABLE)
                .unwrap() as u32,
            length: obj.name.len() as u32,
        }),
        kclvm_sema::resolver::scope::ScopeObjectKind::Attribute => Some(ImCompleteSemanticToken {
            start: obj.start.clone(),
            end: obj.end.clone(),
            kind: LEGEND_TYPE
                .iter()
                .position(|item| item == &SemanticTokenType::PROPERTY)
                .unwrap() as u32,
            length: obj.name.len() as u32,
        }),
        kclvm_sema::resolver::scope::ScopeObjectKind::Definition => {
            let mut start_pos = obj.start.clone();
            start_pos.column =  Some(start_pos.column.unwrap_or(0) + "schema ".len() as u64  );
            Some(ImCompleteSemanticToken {
            start: start_pos,
            end: obj.end.clone(),
            kind: LEGEND_TYPE
                .iter()
                .position(|item| item == &SemanticTokenType::STRUCT)
                .unwrap() as u32,
            length: obj.name.len() as u32,
        })},
        kclvm_sema::resolver::scope::ScopeObjectKind::Parameter => Some(ImCompleteSemanticToken {
            start: obj.start.clone(),
            end: obj.end.clone(),
            kind: LEGEND_TYPE
                .iter()
                .position(|item| item == &SemanticTokenType::PARAMETER)
                .unwrap() as u32,
            length: obj.name.len() as u32,
        }),
        kclvm_sema::resolver::scope::ScopeObjectKind::TypeAlias => None,
        kclvm_sema::resolver::scope::ScopeObjectKind::Module => None,
    }
}

pub fn get_scope_imcomplete_sema_token(scope: &Scope, file_name: &str) -> Vec<ImCompleteSemanticToken>{
    let mut tokens = vec![];
    for (_, obj) in &scope.elems {
        let obj = obj.borrow();
        if obj.start.filename == file_name{
            if let Some(token) = kcl_scopeobj_to_lsp_imcomplete_sema_token(&obj){
                tokens.push(token);
            }
        }
    }
    for child in &scope.children{
        let child = child.borrow();
        tokens.append(&mut get_scope_imcomplete_sema_token(&child, file_name));
    }
    tokens
}
