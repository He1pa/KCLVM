use std::{collections::HashMap, time::Instant};

use anyhow::Ok;
use crossbeam_channel::Sender;
use ra_ap_vfs::VfsPath;
use lsp_types::{CodeAction, CodeActionKind, CodeActionOrCommand, TextEdit};

use crate::{
    completion::completion,
    db::AnalysisDatabase,
    dispatcher::RequestDispatcher,
    document_symbol::document_symbol,
    from_lsp::{self, file_path_from_url, kcl_pos},
    goto_def::goto_definition,
    hover, quick_fix,
    state::{log_message, LanguageServerSnapshot, LanguageServerState, Task},
};

impl LanguageServerState {
    /// Handles a language server protocol request
    pub(super) fn on_request(
        &mut self,
        request: lsp_server::Request,
        request_received: Instant,
    ) -> anyhow::Result<()> {
        log_message(format!("on request {:?}", request), &self.task_sender)?;
        self.register_request(&request, request_received);

        // If a shutdown was requested earlier, immediately respond with an error
        if self.shutdown_requested {
            self.respond(lsp_server::Response::new_err(
                request.id,
                lsp_server::ErrorCode::InvalidRequest as i32,
                "shutdown was requested".to_owned(),
            ))?;
            return Ok(());
        }

        // Dispatch the event based on the type of event
        RequestDispatcher::new(self, request)
            .on_sync::<lsp_types::request::Shutdown>(|state, _request| {
                state.shutdown_requested = true;
                Ok(())
            })?
            .on::<lsp_types::request::GotoDefinition>(handle_goto_definition)?
            .on::<lsp_types::request::Completion>(handle_completion)?
            .on::<lsp_types::request::HoverRequest>(handle_hover)?
            .on::<lsp_types::request::DocumentSymbolRequest>(handle_document_symbol)?
            .on::<lsp_types::request::CodeActionRequest>(handle_code_action)?
            .finish();

        Ok(())
    }
}

impl LanguageServerSnapshot {
    pub(crate) fn get_db(&self, path: &VfsPath) -> anyhow::Result<&AnalysisDatabase> {
        match self.vfs.read().file_id(path) {
            Some(id) => match self.db.get(&id) {
                Some(db) => Ok(db),
                None => Err(anyhow::anyhow!(format!(
                    "Path {path} AnalysisDatabase not found"
                ))),
            },
            None => Err(anyhow::anyhow!(format!("Path {path} fileId not found"))),
        }
    }
}

/// Called when a `GotoDefinition` request was received.
pub(crate) fn handle_code_action(
    _snap: LanguageServerSnapshot,
    params: lsp_types::CodeActionParams,
    _sender: Sender<Task>,
) -> anyhow::Result<Option<lsp_types::CodeActionResponse>> {
    let mut code_actions: Vec<lsp_types::CodeActionOrCommand> = vec![];
    code_actions.extend(quick_fix::quick_fix(params));
    Ok(Some(code_actions))
}

/// Called when a `GotoDefinition` request was received.
pub(crate) fn handle_goto_definition(
    snapshot: LanguageServerSnapshot,
    params: lsp_types::GotoDefinitionParams,
    sender: Sender<Task>,
) -> anyhow::Result<Option<lsp_types::GotoDefinitionResponse>> {
    let file = file_path_from_url(&params.text_document_position_params.text_document.uri)?;
    let path = from_lsp::abs_path(&params.text_document_position_params.text_document.uri)?;
    let db = snapshot.get_db(&path.into())?;
    let kcl_pos = kcl_pos(&file, params.text_document_position_params.position);
    let res = goto_definition(&db.prog, &kcl_pos, &db.scope);
    if res.is_none() {
        log_message("Definition item not found".to_string(), &sender)?;
    }
    Ok(res)
}

/// Called when a `Completion` request was received.
pub(crate) fn handle_completion(
    snapshot: LanguageServerSnapshot,
    params: lsp_types::CompletionParams,
    sender: Sender<Task>,
) -> anyhow::Result<Option<lsp_types::CompletionResponse>> {
    let file = file_path_from_url(&params.text_document_position.text_document.uri)?;
    let path = from_lsp::abs_path(&params.text_document_position.text_document.uri)?;
    let db = snapshot.get_db(&path.into())?;
    let kcl_pos = kcl_pos(&file, params.text_document_position.position);
    let completion_trigger_character = params
        .context
        .and_then(|ctx| ctx.trigger_character)
        .and_then(|s| s.chars().next());
    let res = completion(completion_trigger_character, &db.prog, &kcl_pos, &db.scope);
    if res.is_none() {
        log_message("Completion item not found".to_string(), &sender)?;
    }
    Ok(res)
}

/// Called when a `Completion` request was received.
pub(crate) fn handle_hover(
    snapshot: LanguageServerSnapshot,
    params: lsp_types::HoverParams,
    sender: Sender<Task>,
) -> anyhow::Result<Option<lsp_types::Hover>> {
    let file = file_path_from_url(&params.text_document_position_params.text_document.uri)?;
    let path = from_lsp::abs_path(&params.text_document_position_params.text_document.uri)?;
    let db = snapshot.get_db(&path.into())?;
    let kcl_pos = kcl_pos(&file, params.text_document_position_params.position);
    let res = hover::hover(&db.prog, &kcl_pos, &db.scope);
    if res.is_none() {
        log_message("Hover definition not found".to_string(), &sender)?;
    }
    Ok(res)
}

/// Called when a `GotoDefinition` request was received.
pub(crate) fn handle_document_symbol(
    snapshot: LanguageServerSnapshot,
    params: lsp_types::DocumentSymbolParams,
    sender: Sender<Task>,
) -> anyhow::Result<Option<lsp_types::DocumentSymbolResponse>> {
    let file = file_path_from_url(&params.text_document.uri)?;
    let path = from_lsp::abs_path(&params.text_document.uri)?;
    let db = snapshot.get_db(&path.into())?;
    let res = document_symbol(&file, &db.prog, &db.scope);
    if res.is_none() {
        log_message(format!("File {file} Document symbol not found"), &sender)?;
    }
    Ok(res)
}
