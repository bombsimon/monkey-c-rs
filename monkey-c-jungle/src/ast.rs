use monkey_c_parser::ast::Span;

/// Span for nodes built programmatically rather than parsed.
pub(crate) const SYNTHETIC: Span = Span { start: 0, end: 0 };

/// A parsed jungle file, as entries in source order.
///
/// Comments and blank lines are entries too, which is what lets a file be edited and written back
/// without losing the notes around the instructions. Parse with [`JungleFile::parse`] or
/// `str::parse`, edit through [`JungleFile::set`] and friends, render with `to_string()`.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct JungleFile {
    pub entries: Vec<Entry>,
}

/// A single line of a jungle file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Entry {
    Assignment(Assignment),
    Comment(Comment),
    BlankLine,
}

/// A `qualifier[.property] = value` build instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment {
    pub target: QualifiedName,
    pub values: Vec<Value>,
    pub span: Span,
}

/// A dotted name like `base`, `fenix5.sourcePath` or `fenix5.lang.eng` — both the target of an
/// instruction and the subject of a `$(…)` dereference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QualifiedName {
    pub segments: Vec<String>,
    pub span: Span,
}

/// One `;`-separated item of a value list, with the comments written against it. See the [crate
/// docs](crate) for why comments belong to a value rather than to the instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    pub kind: ValueKind,
    pub comments: Vec<Comment>,
    pub span: Span,
}

/// Either text — the adjacent literals and `$(…)` dereferences that make it up, concatenated — or
/// a `[…]` group, which pulls several build instructions out of one barrel project.
///
/// The distinction that matters: `$(base.sourcePath);source` is two values, while
/// `$(base.sourcePath)/source` is one value of two parts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueKind {
    Literal(Vec<LiteralPart>),
    Group(Vec<Value>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralPart {
    Text(Text),
    Reference(QualifiedName),
}

/// Raw text inside a value. `quoted` remembers quoting the author didn't strictly need, so it
/// survives a round-trip; text with whitespace is quoted on the way out either way.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Text {
    pub text: String,
    pub quoted: bool,
    pub span: Span,
}

/// A `#` comment. `text` is everything after the `#` verbatim, the author's spacing included, so
/// it comes back out untouched — build one with [`Comment::new`], which handles that spacing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Comment {
    pub text: String,
    pub span: Span,
}

impl JungleFile {
    /// Parse jungle source text.
    pub fn parse(source: &str) -> Result<Self, crate::parser::ParserError> {
        crate::parser::Parser::new(source).parse()
    }

    pub fn assignments(&self) -> impl DoubleEndedIterator<Item = &Assignment> {
        self.entries.iter().filter_map(|entry| match entry {
            Entry::Assignment(assignment) => Some(assignment),
            _ => None,
        })
    }

    pub fn assignments_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Assignment> {
        self.entries.iter_mut().filter_map(|entry| match entry {
            Entry::Assignment(assignment) => Some(assignment),
            _ => None,
        })
    }

    /// Find the instruction targeting a dotted name like `fenix5.sourcePath`.
    ///
    /// A file may assign the same target more than once, and the later one wins, so this returns
    /// the last match.
    pub fn get(&self, target: &str) -> Option<&Assignment> {
        let name = QualifiedName::new(target);

        self.assignments().rev().find(|a| a.target.matches(&name))
    }

    pub fn get_mut(&mut self, target: &str) -> Option<&mut Assignment> {
        let name = QualifiedName::new(target);

        self.assignments_mut()
            .rev()
            .find(|a| a.target.matches(&name))
    }

    /// Point `target` at `values`, replacing the last instruction that assigns it or appending one
    /// at the end of the file.
    pub fn set(&mut self, target: &str, values: impl IntoIterator<Item = Value>) {
        let values = values.into_iter().collect();

        if let Some(assignment) = self.get_mut(target) {
            assignment.values = values;

            return;
        }

        self.entries
            .push(Entry::Assignment(Assignment::new(target, values)));
    }

    /// Append a comment on a line of its own.
    pub fn push_comment(&mut self, text: impl Into<String>) {
        self.entries.push(Entry::Comment(Comment::new(text)));
    }

    /// Append a blank line, to group instructions into sections.
    pub fn push_blank_line(&mut self) {
        self.entries.push(Entry::BlankLine);
    }

    /// Drop every instruction assigning `target` and report how many were removed.
    pub fn remove(&mut self, target: &str) -> usize {
        let name = QualifiedName::new(target);
        let before = self.entries.len();

        self.entries.retain(|entry| match entry {
            Entry::Assignment(assignment) => !assignment.target.matches(&name),
            _ => true,
        });

        before - self.entries.len()
    }
}

impl std::str::FromStr for JungleFile {
    type Err = crate::parser::ParserError;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        Self::parse(source)
    }
}

impl Assignment {
    pub fn new(target: &str, values: impl IntoIterator<Item = Value>) -> Self {
        Self {
            target: QualifiedName::new(target),
            values: values.into_iter().collect(),
            span: SYNTHETIC,
        }
    }
}

impl Comment {
    /// Build a comment from its content, spacing it off the `#` — `Comment::new("Sources")` prints
    /// as `# Sources`.
    ///
    /// Text that already starts with whitespace is left alone, so this is idempotent and a caller
    /// who wants tighter or wider spacing can still ask for it.
    pub fn new(text: impl Into<String>) -> Self {
        let mut text = text.into();

        if !text.is_empty() && !text.starts_with(char::is_whitespace) {
            text.insert(0, ' ');
        }

        Self {
            text,
            span: SYNTHETIC,
        }
    }
}

impl QualifiedName {
    /// Build a name from a dotted string, e.g. `fenix5.lang.eng`.
    pub fn new(name: &str) -> Self {
        Self {
            segments: name.split('.').map(str::to_string).collect(),
            span: SYNTHETIC,
        }
    }

    /// Whether two names refer to the same thing, ignoring spans so a constructed name matches a
    /// parsed one.
    pub fn matches(&self, other: &Self) -> bool {
        self.segments == other.segments
    }

    /// The qualifier the name applies to, e.g. `fenix5` in `fenix5.lang.eng`.
    pub fn qualifier(&self) -> &str {
        self.segments.first().map_or("", String::as_str)
    }

    /// The property path below the qualifier, e.g. `["lang", "eng"]` in `fenix5.lang.eng`.
    pub fn property(&self) -> &[String] {
        self.segments.get(1..).unwrap_or_default()
    }
}

impl Value {
    /// A value made of literal text, e.g. `resources-round-360x360`.
    pub fn text(text: impl Into<String>) -> Self {
        Self::of(ValueKind::Literal(vec![LiteralPart::Text(Text {
            text: text.into(),
            quoted: false,
            span: SYNTHETIC,
        })]))
    }

    /// A value that dereferences another qualifier, e.g. `$(base.sourcePath)`.
    pub fn reference(target: &str) -> Self {
        Self::of(ValueKind::Literal(vec![LiteralPart::Reference(
            QualifiedName::new(target),
        )]))
    }

    /// A bracketed group of values, e.g. `[round.jungle;rect.jungle]`.
    pub fn group(values: impl IntoIterator<Item = Value>) -> Self {
        Self::of(ValueKind::Group(values.into_iter().collect()))
    }

    /// Note this value with a comment, printed after it on the same line.
    pub fn with_comment(mut self, text: impl Into<String>) -> Self {
        self.comments.push(Comment::new(text));

        self
    }

    fn of(kind: ValueKind) -> Self {
        Self {
            kind,
            comments: Vec::new(),
            span: SYNTHETIC,
        }
    }
}
