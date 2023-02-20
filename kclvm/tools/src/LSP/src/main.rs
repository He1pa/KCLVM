use chrono::{Local, TimeZone};
use indexmap::IndexSet;
use kclvm_tools::lint::lint_files;
use kclvm_tools::util::lsp::kcl_diag_to_lsp_diags;
use kclvm_parser::load_program;
use kclvm_sema::resolver::resolve_program;
use kclvm_tools::util::lsp::{LEGEND_TYPE, get_scope_imcomplete_sema_token};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use kclvm_error::Diagnostic as KCLDiagnostic;

#[derive(Debug)]
struct Backend {
    client: Client,
}

struct TextDocumentItem {
    uri: Url,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("KCL".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.clone().into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(false),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
        })
        .await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
        })
        .await;
        self.client
            .log_message(MessageType::INFO, "file changed!")
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
        })
        .await;

        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let filename = params.text_document.uri.path();
        self.client
            .log_message(
                MessageType::INFO,
                format!("semantic_token_full filename:{}", filename),
            )
            .await;

        let mut program = load_program(&[filename], None).unwrap();
        let prog_scope = resolve_program(&mut program);
        
        let mut imcompletesemantictokens = vec![];
        let scope_map = prog_scope.scope_map.clone();
        for (_, scope) in scope_map.iter() {
            let s = scope.borrow();
            imcompletesemantictokens.append(&mut get_scope_imcomplete_sema_token(&s, filename));
        }


        imcompletesemantictokens.sort_by(|a, b| {
            if a.start.line == b.start.line {
                a.start
                    .column
                    .unwrap_or(0)
                    .cmp(&b.start.column.unwrap_or(0))
            } else {
                a.start.line.cmp(&b.start.line)
            }
        });

        let mut pre_line = 0;
        let mut pre_start = 0;

        let semantic_tokens: Vec<SemanticToken> = imcompletesemantictokens
            .iter()
            .map(|obj| {
                let line = obj.start.line - 1;
                let start = obj.start.column.unwrap_or(0);

                let delta_line: u32 = (line - pre_line) as u32;
                let delta_start: u32 = (if delta_line == 0 {
                    start - pre_start
                } else {
                    start
                }) as u32;
                let length = obj.length;
                let ret = SemanticToken {
                    delta_line,
                    delta_start,
                    length,
                    token_type: obj.kind,
                    token_modifiers_bitset: 0,
                };
                pre_line = line;
                pre_start = start;
                ret
            })
            .collect();

        return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })));
    }
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Get request: {} ",
                    Local
                        .timestamp_millis_opt(Local::now().timestamp_millis())
                        .unwrap()
                ),
            )
            .await;
        let uri = params.uri.clone();
        let file_name = uri.path();
        self.client
            .log_message(MessageType::INFO, "on change")
            .await;

        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Start lint: {} ",
                    Local
                        .timestamp_millis_opt(Local::now().timestamp_millis())
                        .unwrap()
                ),
            )
            .await;

        let (errors, warnings) = lint_files(&[file_name], None);

        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "End lint: {} ",
                    Local
                        .timestamp_millis_opt(Local::now().timestamp_millis())
                        .unwrap()
                ),
            )
            .await;
        let diags: IndexSet<KCLDiagnostic> = errors
            .iter()
            .chain(warnings.iter())
            .cloned()
            .collect::<IndexSet<KCLDiagnostic>>();

        let diagnostics = diags
            .iter()
            .map(|diag| kcl_diag_to_lsp_diags(diag, file_name))
            .flatten()
            .collect::<Vec<Diagnostic>>();

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, None)
            .await;
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Response to client: {} ",
                    Local
                        .timestamp_millis_opt(Local::now().timestamp_millis())
                        .unwrap()
                ),
            )
            .await;
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend { client });

    Server::new(stdin, stdout, socket).serve(service).await;
}
