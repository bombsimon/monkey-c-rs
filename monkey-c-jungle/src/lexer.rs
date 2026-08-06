use crate::token;

/// Byte scanner over jungle source. Pull tokens with [`Lexer::next_token`] until it yields
/// [`token::Type::Eof`].
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, position: 0 }
    }

    /// Scan the next token with its `[start, end)` byte span.
    pub fn next_token(&mut self) -> (usize, token::Type, usize) {
        self.skip_separators();

        let start = self.position;
        let Some(byte) = self.peek() else {
            return (start, token::Type::Eof, start);
        };

        // A comment runs past its own line break, so its span ends before the scanner does and
        // can't come from `self.position` like every other token's.
        if byte == b'#' {
            let (token, end) = self.read_comment();

            return (start, token, end);
        }

        let token = match byte {
            b'\n' => {
                self.position += 1;
                token::Type::Newline
            }
            b'=' => self.single(token::Type::Assign),
            b';' => self.single(token::Type::Semicolon),
            b'[' => self.single(token::Type::LBracket),
            b']' => self.single(token::Type::RBracket),
            b'"' => self.read_quoted(),
            b'$' if self.peek_at(1) == Some(b'(') => self.read_deref(),
            _ => token::Type::Word(self.read_word().to_string()),
        };

        (start, token, self.position)
    }

    fn peek(&self) -> Option<u8> {
        self.peek_at(0)
    }

    fn peek_at(&self, offset: usize) -> Option<u8> {
        self.input.as_bytes().get(self.position + offset).copied()
    }

    fn single(&mut self, token: token::Type) -> token::Type {
        self.position += 1;
        token
    }

    /// Skip what separates two tokens: blanks, and the line break a `\` hides. Swallowing the
    /// latter here means a [`token::Type::Newline`] always terminates an instruction.
    ///
    /// The `\` only counts when it starts a token. `monkeyc` treats it as an ordinary value
    /// character otherwise, so `source\` at the end of a line is the value `source\`, not `source`
    /// continued — see [`Lexer::read_word`].
    fn skip_separators(&mut self) {
        while let Some(byte) = self.peek() {
            match byte {
                b'\\' if matches!(self.peek_at(1), Some(b'\n' | b'\r')) => {
                    self.position += 1;
                    self.skip_line_break();
                }
                b'\n' => return,
                _ if byte.is_ascii_whitespace() => self.position += 1,
                _ => return,
            }
        }
    }

    fn skip_line_break(&mut self) {
        if self.peek() == Some(b'\r') {
            self.position += 1;
        }

        if self.peek() == Some(b'\n') {
            self.position += 1;
        }
    }

    /// Read a comment and the line break `monkeyc` folds into it, returning the end of the text so
    /// the span covers the comment alone.
    fn read_comment(&mut self) -> (token::Type, usize) {
        self.position += 1; // consume `#`
        let start = self.position;
        self.advance_to_line_break();

        let text = self.input[start..self.position].to_string();
        let end = self.position;
        self.skip_line_break();

        (token::Type::Comment(text), end)
    }

    /// Read a `"…"` value. A `#` ends the scan like a line break does, since `monkeyc` starts a
    /// comment on one even inside quotes and leaves the string unclosed.
    fn read_quoted(&mut self) -> token::Type {
        let opening = self.position;
        self.position += 1; // consume `"`
        let start = self.position;

        while let Some(byte) = self.peek() {
            match byte {
                b'"' => {
                    let text = self.input[start..self.position].to_string();
                    self.position += 1; // consume the closing `"`

                    return token::Type::QuotedWord(text);
                }
                b'\n' | b'\r' | b'#' => break,
                _ => self.position += 1,
            }
        }

        token::Type::UnclosedString(self.input[opening..self.position].to_string())
    }

    fn read_deref(&mut self) -> token::Type {
        let opening = self.position;
        self.position += 2; // consume `$(`
        let start = self.position;

        while let Some(byte) = self.peek() {
            match byte {
                b')' => {
                    let name = self.input[start..self.position].to_string();
                    self.position += 1; // consume the closing `)`

                    return token::Type::Deref(name);
                }
                b'\n' | b'\r' => break,
                _ => self.position += 1,
            }
        }

        token::Type::UnclosedDeref(self.input[opening..self.position].to_string())
    }

    /// Read a run of raw value text, which is what keeps paths like `.\**.mc` and device names
    /// like `round-360x360` in one piece.
    ///
    /// A trailing `\` belongs to the word, so `source\` at the end of a line is a value and the
    /// line break after it still terminates. `monkeyc` agrees: it rejects `a\` + break + `b`.
    fn read_word(&mut self) -> &'a str {
        let start = self.position;

        while let Some(byte) = self.peek() {
            let starts_deref = byte == b'$' && self.peek_at(1) == Some(b'(');

            if starts_deref || !is_word_byte(byte) {
                break;
            }

            self.position += 1;
        }

        &self.input[start..self.position]
    }

    fn advance_to_line_break(&mut self) {
        while let Some(byte) = self.peek() {
            if byte == b'\n' || byte == b'\r' {
                break;
            }

            self.position += 1;
        }
    }
}

/// Whether `byte` can appear in an unquoted word. Non-ASCII bytes qualify so UTF-8 sequences stay
/// intact, and `)` qualifies because a dereference is lexed whole and never leaves a stray one.
fn is_word_byte(byte: u8) -> bool {
    !byte.is_ascii_whitespace() && !matches!(byte, b'=' | b';' | b'[' | b']' | b'"' | b'#')
}
