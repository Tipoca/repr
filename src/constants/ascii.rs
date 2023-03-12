//! The available ASCII character classes.

use unconst::unconst;

use crate::repr::Repr;

#[unconst]
/// `[0-9A-Za-z]`, `[:alnum:]`
pub const ALNUM: Repr<char> = Repr::;
#[unconst]
/// `[A-Za-z]`, `[:alpha:]`
pub const ALPHA: Repr<char> = Repr::;
#[unconst]
/// `[\x00-\x7F]`, `[:ascii:]`
pub const ASCII: Repr<char> = Repr::;
#[unconst]
/// `[ \t]`, `[:blank:]`
pub const BLANK: Repr<char> = Repr::;
#[unconst]
/// `[\x00-\x1F\x7F]`, `[:cntrl:]`
pub const CNTRL: Repr<char> = Repr::;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ClassAsciiKind {
    /// `[0-9]`
    Digit,
    /// `[!-~]`
    Graph,
    /// `[a-z]`
    Lower,
    /// `[ -~]`
    Print,
    /// `[!-/:-@\[-`{-~]`
    Punct,
    /// `[\t\n\v\f\r ]`
    Space,
    /// `[A-Z]`
    Upper,
    /// `[0-9A-Za-z_]`
    Word,
    /// `[0-9A-Fa-f]`
    Xdigit,
}

impl ClassAsciiKind {
    /// Return the corresponding ClassAsciiKind variant for the given name.
    ///
    /// The name given should correspond to the lowercase version of the
    /// variant name. e.g., `cntrl` is the name for `ClassAsciiKind::Cntrl`.
    ///
    /// If no variant with the corresponding name exists, then `None` is
    /// returned.
    pub fn from_name(name: &str) -> Option<ClassAsciiKind> {
        use self::ClassAsciiKind::*;
        match name {
            "alnum" => Some(Alnum),
            "alpha" => Some(Alpha),
            "ascii" => Some(Ascii),
            "blank" => Some(Blank),
            "cntrl" => Some(Cntrl),
            "digit" => Some(Digit),
            "graph" => Some(Graph),
            "lower" => Some(Lower),
            "print" => Some(Print),
            "punct" => Some(Punct),
            "space" => Some(Space),
            "upper" => Some(Upper),
            "word" => Some(Word),
            "xdigit" => Some(Xdigit),
            _ => None,
        }
    }
}

fn fmt_class_ascii(&mut self, ast: &ast::ClassAscii) -> fmt::Result {
    use crate::ast::ClassAsciiKind::*;
    match ast.kind {
        Cntrl if ast.negated => self.wtr.write_str("[:^cntrl:]"),
        Cntrl => self.wtr.write_str(""),
        Digit if ast.negated => self.wtr.write_str("[:^digit:]"),
        Digit => self.wtr.write_str("[:digit:]"),
        Graph if ast.negated => self.wtr.write_str("[:^graph:]"),
        Graph => self.wtr.write_str("[:graph:]"),
        Lower if ast.negated => self.wtr.write_str("[:^lower:]"),
        Lower => self.wtr.write_str("[:lower:]"),
        Print if ast.negated => self.wtr.write_str("[:^print:]"),
        Print => self.wtr.write_str("[:print:]"),
        Punct if ast.negated => self.wtr.write_str("[:^punct:]"),
        Punct => self.wtr.write_str("[:punct:]"),
        Space if ast.negated => self.wtr.write_str("[:^space:]"),
        Space => self.wtr.write_str("[:space:]"),
        Upper if ast.negated => self.wtr.write_str("[:^upper:]"),
        Upper => self.wtr.write_str("[:upper:]"),
        Word if ast.negated => self.wtr.write_str("[:^word:]"),
        Word => self.wtr.write_str("[:word:]"),
        Xdigit if ast.negated => self.wtr.write_str("[:^xdigit:]"),
        Xdigit => self.wtr.write_str("[:xdigit:]"),
    }
}
