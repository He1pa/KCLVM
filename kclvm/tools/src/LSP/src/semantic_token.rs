use std::vec;

use kclvm_error::Position;
use kclvm_sema::core::{global_state::GlobalState, symbol::SymbolKind};
use lsp_types::{SemanticToken, SemanticTokenType, SemanticTokens, SemanticTokensResult};

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRUCT,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::TYPE,
    SemanticTokenType::MACRO,
];

pub(crate) struct KCLSemanticToken {
    pub start: Position,
    pub kind: u32,
    pub length: u32,
}

pub(crate) fn semantic_tokens_full(file: &str, gs: &GlobalState) -> Option<SemanticTokensResult> {
    let mut kcl_tokens: Vec<KCLSemanticToken> = vec![];
    let sema_db = gs.get_sema_db();
    if let Some(file_sema) = sema_db.get_file_sema(&file.to_string()) {
        let symbols = file_sema.get_symbols();
        for symbol_ref in symbols {
            if let Some(symbol) = gs.get_symbols().get_symbol(*symbol_ref) {
                let (start, end) = symbol.get_range();
                let kind = match symbol_ref.get_kind() {
                    SymbolKind::Schema => type_index(SemanticTokenType::STRUCT),
                    SymbolKind::Attribute => type_index(SemanticTokenType::PROPERTY),
                    SymbolKind::Package => type_index(SemanticTokenType::NAMESPACE),
                    SymbolKind::TypeAlias => type_index(SemanticTokenType::TYPE),
                    SymbolKind::Value | SymbolKind::Unresolved => {
                        type_index(SemanticTokenType::VARIABLE)
                    }
                    SymbolKind::Rule => type_index(SemanticTokenType::MACRO),
                };
                kcl_tokens.push(KCLSemanticToken {
                    start: start.clone(),
                    kind,
                    length: if start.line == end.line {
                        (end.column.unwrap_or(0) - start.column.unwrap_or(0)) as u32
                    } else {
                        symbol.get_name().len() as u32
                    },
                });
            }
        }
    }

    Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: kcl_semantic_tokens_to_semantic_tokens(&mut kcl_tokens),
    }))
}

pub(crate) fn type_index(ty: SemanticTokenType) -> u32 {
    LEGEND_TYPE.iter().position(|it| *it == ty).unwrap() as u32
}

pub(crate) fn kcl_semantic_tokens_to_semantic_tokens(
    tokens: &mut Vec<KCLSemanticToken>,
) -> Vec<SemanticToken> {
    tokens.sort_by(|a, b| {
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

    let semantic_tokens: Vec<SemanticToken> = tokens
        .iter()
        .map(|obj| {
            // ref: https://github.com/microsoft/vscode-extension-samples/blob/5ae1f7787122812dcc84e37427ca90af5ee09f14/semantic-tokens-sample/vscode.proposed.d.ts#L71
            // A file can contain many tokens, perhaps even hundreds of thousands of tokens. Therefore, to improve
            // the memory consumption around describing semantic tokens, we have decided to avoid allocating an object
            // for each token and we represent tokens from a file as an array of integers. Furthermore, the position
            // of each token is expressed relative to the token before it because most tokens remain stable relative to
            // each other when edits are made in a file.
            let line = obj.start.line - 1;
            let start = obj.start.column.unwrap_or(0);

            let delta_line: u32 = (line - pre_line) as u32;
            let delta_start: u32 =
                (if delta_line == 0 {
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
    semantic_tokens
}

#[cfg(test)]
mod tests {
    use crate::tests::compile_test_file;
    use proc_macro_crate::bench_test;

    use super::semantic_tokens_full;

    #[test]
    #[bench_test]
    fn semantic_tokens_full_test() {
        let (file, _, _, _, gs) = compile_test_file("src/test_data/sema_token.k");
        let expected = [
            (0, 5, 3, 4),
            (1, 7, 7, 1),
            (1, 4, 4, 2),
            (2, 0, 2, 0),
            (0, 4, 7, 0),
            (0, 10, 7, 0),
            (1, 4, 4, 0),
            (2, 0, 1, 0),
            (0, 3, 3, 0),
        ];
        let res = semantic_tokens_full(&file, &gs);
        if let Some(tokens) = res {
            match &tokens {
                lsp_types::SemanticTokensResult::Tokens(tokens) => {
                    let get: Vec<(u32, u32, u32, u32)> = tokens
                        .data
                        .iter()
                        .map(|token| {
                            (
                                token.delta_line as u32,
                                token.delta_start as u32,
                                token.length as u32,
                                token.token_type as u32,
                            )
                        })
                        .collect();
                    assert_eq!(get, expected);
                }
                lsp_types::SemanticTokensResult::Partial(_) => {
                    panic!("test failed")
                }
            }
        }
    }
}