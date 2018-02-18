extern crate byteorder;
extern crate pom;
extern crate rand;
extern crate ring;

use std::collections::{BTreeMap, BTreeSet};

mod parser {
    use pom::char_class::{alphanum, digit};
    use pom::parser::*;
    use pom::{self, Parser};
    use std::ops::Range;
    use std::fmt;

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Spaced<T> {
        pub inner: T,
        pub matched: Range<usize>,
        pub trailer: String,
    }

    impl fmt::Display for Spaced<String> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}{}", self.inner, self.trailer)
        }
    }

    impl fmt::Display for Spaced<char> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}{}", self.inner, self.trailer)
        }
    }

    impl fmt::Display for Spaced<()> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.trailer)
        }
    }

    pub type S<T> = Spaced<T>;

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Token {
        Ident(S<String>),
        Number(S<String>),
        Punctuation(S<String>),
        Tree { start: S<char>, inner: Vec<Token>, end: S<char> },
    }

    impl fmt::Display for Token {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            use self::Token::*;
            match *self {
                Ident(ref s) | Number(ref s) | Punctuation(ref s) =>
                    write!(f, "{}", s),
                Tree { ref start, ref inner, ref end } => {
                    write!(f, "{}", start)?;
                    for n in inner {
                        write!(f, "{}", n)?;
                    }
                    write!(f, "{}", end)
                }
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Node {
        Preprocessor { directive: S<String>, value: S<String> },
        Define { directive: S<String>, name: S<String>, value: Vec<Token> },
        Token(Token),
    }

    impl fmt::Display for Node {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            use self::Node::*;
            match *self {
                Preprocessor { ref directive, ref value } =>
                    write!(f, "{}{}", directive, value),
                Define { ref directive, ref name, ref value } => {
                    write!(f, "{}{}", directive, name)?;
                    for t in value {
                        write!(f, "{}", t)?;
                    }
                    write!(f, "\n")
                },
                Token(ref t) =>
                    write!(f, "{}", t),
            }
        }
    }

    #[derive(Debug)]
    pub struct File(pub S<()>, pub Vec<Node>);

    impl fmt::Display for File {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.0)?;
            for n in &self.1 {
                write!(f, "{}", n)?;
            }
            Ok(())
        }
    }

    fn utf8(v: Vec<u8>) -> String {
        String::from_utf8(v).unwrap()
    }

    fn concat2(a: Parser<u8, Vec<u8>>, b: Parser<u8, Vec<u8>>) -> Parser<u8, Vec<u8>> {
        (a + b).map(|(mut va, vb)| {
            va.extend(vb);
            va
        })
    }

    fn concat3(a: Parser<u8, Vec<u8>>, b: Parser<u8, Vec<u8>>, c: Parser<u8, Vec<u8>>) -> Parser<u8, Vec<u8>> {
        (a + b + c).map(|((mut va, vb), vc)| {
            va.extend(vb);
            va.extend(vc);
            va
        })
    }

    fn flatten_vecs<T>(vv: Vec<Vec<T>>) -> Vec<T> {
        vv.into_iter().flat_map(|x| x).collect()
    }

    fn space() -> Parser<u8, String> {
        let end_comment = || seq(b"*/");
        ( one_of(b" \t\r\n").repeat(1..)
          | (concat2(seq(b"//"), none_of(b"\n").repeat(0..)))
          | (concat3(seq(b"/*"),
                     (!end_comment() * take(1)).repeat(0..).map(flatten_vecs),
                     end_comment()))
        ).repeat(0..).map(flatten_vecs).map(utf8)
    }

    fn spaced<T>(subparser: Parser<u8, T>) -> Parser<u8, Spaced<T>>
        where T: 'static,
    {
        (empty().pos() + subparser + space() + empty().pos()).map(|(((start, inner), trailer), end)| {
            Spaced { inner, trailer, matched: Range { start, end } }
        })
    }

    fn spaced_string(subparser: Parser<u8, Vec<u8>>) -> Parser<u8, Spaced<String>> {
        spaced(subparser.map(utf8))
    }

    fn to_eol() -> Parser<u8, String> {
        (none_of(b"\n").repeat(1..) | seq(b"\\\n")).repeat(0..).map(flatten_vecs).map(utf8)
    }

    fn slice_to_seqs(s: &'static [&'static [u8]]) -> Parser<u8, Vec<u8>> {
        let mut seqs = s.into_iter().map(|&b| seq::<u8, [u8]>(b));
        let first_seq = seqs.next().unwrap();
        seqs.fold(first_seq, |a, b| a | b)
    }

    fn preprocessor_define() -> Parser<u8, Node> {
        (spaced_string(seq(b"#define")) + ident() + spaced(to_eol()))
            .convert::<_, pom::Error, _>(|((directive, name), spaced_value)| {
                let mut input = spaced_value.inner;
                input.push_str(&spaced_value.trailer);
                let mut data_input = pom::DataInput::new(input.as_bytes());
                let value = token().repeat(0..).parse(&mut data_input)?;
                Ok(Node::Define { directive, name, value })
            })
    }

    fn preprocessor_line() -> Parser<u8, Node> {
        let directive = spaced((sym(b'#') + slice_to_seqs(&[
            b"if", b"else", b"elif", b"endif", b"ifdef", b"ifdef",
            b"undef", b"pragma", b"version", b"error", b"extension", b"line",
        ])).map(|(b, mut v)| {
            v.insert(0, b);
            utf8(v)
        }));
        (directive + spaced(to_eol()))
            .map(|(directive,  value)| Node::Preprocessor { directive, value })
    }

    fn ident() -> Parser<u8, S<String>> {
        spaced_string(!is_a(digit) * ((is_a(alphanum) | sym(b'_'))).repeat(1..))
    }

    fn number() -> Parser<u8, S<String>> {
        spaced_string((is_a(digit) | one_of(b"+-.eE")).repeat(1..))
    }

    fn punctuation() -> Parser<u8, S<String>> {
        spaced_string(slice_to_seqs(&[
            b",", b";", b"==", b"=", b"<=", b"<", b">=", b">",
            b"-=", b"+=", b"*=", b"/=",
            b"-", b"+", b"*", b"/",
        ]))
    }

    fn tree(start: u8, end: u8) -> Parser<u8, Token> {
        (spaced(sym(start).map(Into::into))
         + call(token).repeat(0..)
         + spaced(sym(end).map(Into::into))
        ).map(|((start, inner), end)| Token::Tree { start, inner, end })
    }

    fn token() -> Parser<u8, Token> {
        ident().map(Token::Ident)
            | number().map(Token::Number)
            | punctuation().map(Token::Punctuation)
            | tree(b'(', b')')
            | tree(b'[', b']')
            | tree(b'{', b'}')
    }

    fn node() -> Parser<u8, Node> {
        preprocessor_line()
            | preprocessor_define()
            | token().map(Node::Token)
    }

    fn file() -> Parser<u8, File> {
        (spaced(empty()) + node().repeat(0..)).map(|(s, v)| File(s, v))
    }

    pub fn parse_string(input: &str) -> Result<File, pom::Error> {
        let mut input = pom::DataInput::new(input.as_bytes());
        file().parse(&mut input)
    }
}

fn condense_token(t: &mut parser::Token, next: Option<&parser::Token>) {
    use parser::Token::*;
    match (t, next) {
        (&mut Ident(ref mut s), Some(&Ident(..))) => {
            s.trailer.clear();
            s.trailer.push(' ');
        },
        (&mut Ident(ref mut s), _) |
        (&mut Number(ref mut s), _) |
        (&mut Punctuation(ref mut s), _) => {
            s.trailer.clear();
        },
        (&mut Tree { ref mut start, ref mut inner, ref mut end }, _) => {
            start.trailer.clear();
            end.trailer.clear();
            condense_tokens(inner);
        },
    }
}

fn split_half_mut<'a, T>(s: &'a mut [T], i: usize) -> (&'a mut T, Option<&'a T>) {
    let (lhs, rhs) = s.split_at_mut(i + 1);
    (&mut lhs[i], rhs.first())
}

fn condense_tokens(ts: &mut [parser::Token]) {
    if ts.is_empty() {
        return;
    }
    for e in 0..ts.len() {
        let (cur, next) = split_half_mut(ts, e);
        condense_token(cur, next);
    }
}

fn default_reserved() -> BTreeSet<String> {
    let mut ret = BTreeSet::new();
    ret.extend([
        "attribute", "const", "bool", "float", "int", "break", "continue",
        "do", "else", "for", "if", "discard", "return", "in", "out", "inout",
        "uniform", "varying", "sampler2D", "samplerCube", "struct", "void",
        "while", "highp", "mediump", "lowp", "true", "false",
    ].iter().map(|&s| s.into()));
    for n in &[2, 3, 4] {
        ret.insert(format!("mat{}", n));
        ret.insert(format!("vec{}", n));
        for k in "bi".chars() {
            ret.insert(format!("{}vec{}", k, n));
        }
    }
    ret
}

struct NameScrambler {
    base: ring::digest::Context,
    reserved: BTreeSet<String>,
    unscrambled: BTreeMap<String, String>,
}

impl NameScrambler {
    fn new(input: &[u8]) -> Self {
        let mut base = ring::digest::Context::new(&ring::digest::SHA256);
        base.update(input);
        NameScrambler {
            base,
            reserved: default_reserved(),
            unscrambled: Default::default(),
        }
    }

    fn build_rng(&mut self, name: &str) -> rand::ChaChaRng {
        use byteorder::{ByteOrder, LE};
        let mut ctx = self.base.clone();
        ctx.update(name.as_bytes());
        let digest = ctx.finish();
        let digest_bytes = digest.as_ref();
        let mut digest_u32 = vec![0u32; digest_bytes.len() / 4];
        LE::read_u32_into(&digest_bytes, &mut digest_u32);
        <rand::ChaChaRng as rand::SeedableRng<_>>::from_seed(&digest_u32)
    }

    fn scrambled(&mut self, name: &str) -> String {
        if self.reserved.contains(name) {
            return name.into();
        }
        use rand::Rng;
        let mut rng = self.build_rng(name);
        let length = rng.gen_range(4, 8);
        let out: String = (0..length)
            .filter_map(|_| rng.choose(&['a', 'e', 'o', 'u']).cloned())
            .collect();
        match self.unscrambled.insert(out.clone(), name.into()) {
            Some(ref s) if s != name => panic!("{:?} derived from {:?} and {:?}", out, name, s),
            _ => (),
        }
        out
    }

    fn scramble_tokens(&mut self, ts: &mut [parser::Token]) {
        for t in ts {
            self.scramble_token(t);
        }
    }

    fn scramble_token(&mut self, t: &mut parser::Token) {
        use parser::Token::*;
        match *t {
            Ident(ref mut s) => {
                s.inner = self.scrambled(&s.inner);
            },
            Tree { ref mut inner, .. } => {
                self.scramble_tokens(inner);
            },
            _ => (),
        }
    }
}

fn main() {
    use std::io::{Read, stdin};
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();
    let mut f = parser::parse_string(&input).unwrap();
    let mut scrambler = NameScrambler::new(input.as_bytes());
    println!("parsed: {:#?}", f);
    f.0.trailer.clear();
    for e in 0..f.1.len() {
        let (cur, next) = split_half_mut(&mut f.1[..], e);
        match *cur {
            parser::Node::Preprocessor { ref mut value, .. } => {
                value.trailer.clear();
                value.trailer.push('\n');
            },
            parser::Node::Define { ref mut name, ref mut value, .. } => {
                name.trailer.clear();
                name.trailer.push(' ');
                name.inner = scrambler.scrambled(&name.inner);
                condense_tokens(value);
                scrambler.scramble_tokens(value);
            },
            parser::Node::Token(ref mut tok) => {
                let next_tok = next.and_then(|n| match *n {
                    parser::Node::Token(ref t) => Some(t),
                    _ => None,
                });
                condense_token(tok, next_tok);
                scrambler.scramble_token(tok);
            },
        }
    }
    for (scrambled, unscrambled) in &scrambler.unscrambled {
        println!("{}={}", scrambled, unscrambled);
    }
    println!("{}", f);
}
