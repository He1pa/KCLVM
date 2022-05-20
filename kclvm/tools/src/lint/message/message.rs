use kclvm_error::Position;

#[derive(Clone, PartialEq)]
pub struct Message {
    pub msg_id: String,
    pub msg: String,
    pub source_code: String,
    pub pos: Position,
    pub arguments: Vec<String>,
}

impl Message {
    pub fn new(
        msg_id: String,
        msg: String,
        source_code: String,
        pos: Position,
        arguments: Vec<String>,
    ) -> Self {
        Self {
            msg_id,
            msg,
            source_code,
            pos,
            arguments,
        }
    }
}

impl std::fmt::Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let column = match self.pos.column {
            Some(column) => column as usize,
            None => 1,
        };
        write!(
            f,
            "{}:{}:{}: {}: {}\n{}\n{}^",
            self.pos.filename,
            self.pos.line,
            column,
            self.msg_id,
            self.msg,
            self.source_code,
            " ".repeat(column - 1)
        )
    }

    // let s = format!("{}:{}:{}:{}:{}\n{}\n{}\n^",
    // msg.file, msg.pos.0, msg.pos.1, msg.msg_id, msg.msg,
    // msg.source_code,
    // " ".repeat(msg.pos.1 as usize - 1));
    // s
}

#[derive(Clone)]
pub struct MSG {
    pub id: String,
    pub short_info: String,
    pub long_info: String,
    pub sarif_info: String,
}
