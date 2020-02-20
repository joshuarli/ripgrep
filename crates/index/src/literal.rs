// BREADCRUMBS: Move this file aside and just work on porting the ngram
// extractor. The code below should be polished and moved into regex-syntax
// however.

use std::cmp;
use std::mem;

use bstr::{BString, ByteSlice, ByteVec};
use regex_syntax::hir::{self, Hir, HirKind};

#[derive(Clone, Debug)]
pub enum GramQuery {
    Literal(BString),
    And(Vec<GramQuery>),
    Or(Vec<GramQuery>),
}

impl GramQuery {
    fn nothing() -> GramQuery {
        GramQuery::Or(vec![])
    }

    fn everything() -> GramQuery {
        GramQuery::And(vec![])
    }

    fn or(mut queries: Vec<GramQuery>) -> GramQuery {
        if queries.is_empty() || queries.len() > 1 {
            return GramQuery::Or(queries);
        }
        queries.pop().unwrap()
    }

    fn and(mut queries: Vec<GramQuery>) -> GramQuery {
        if queries.is_empty() || queries.len() > 1 {
            return GramQuery::And(queries);
        }
        queries.pop().unwrap()
    }

    fn from_set(set: LiteralSet) -> GramQuery {
        let lits: Vec<_> =
            set.lits.into_iter().map(GramQuery::Literal).collect();
        if lits.is_empty() {
            return GramQuery::everything();
        }
        GramQuery::or(lits)
    }

    fn union(&mut self, q2: GramQuery) {
        use self::GramQuery::*;

        let mut q1 = mem::replace(self, GramQuery::nothing());
        match (q1, q2) {
            (Literal(lit1), Literal(lit2)) => {
                *self = Or(vec![Literal(lit1), Literal(lit2)]);
            }
            (Literal(lit1), And(qs2)) => {
                *self = Or(vec![Literal(lit1), And(qs2)]);
            }
            (Literal(lit1), Or(mut qs2)) => {
                qs2.push(Literal(lit1));
                *self = Or(qs2);
            }
            (And(qs1), q2) => {
                *self = Or(vec![And(qs1), q2]);
            }
            (Or(mut qs1), q2) => {
                qs1.push(q2);
                *self = Or(qs1);
            }
        }
    }

    fn intersect(&mut self, q2: GramQuery) {
        use self::GramQuery::*;

        let mut q1 = mem::replace(self, GramQuery::nothing());
        match (q1, q2) {
            (Literal(lit1), Literal(lit2)) => {
                *self = And(vec![Literal(lit1), Literal(lit2)]);
            }
            (Literal(lit1), And(mut qs2)) => {
                qs2.push(Literal(lit1));
                *self = And(qs2);
            }
            (Literal(lit1), Or(qs2)) => {
                *self = And(vec![Literal(lit1), And(qs2)]);
            }
            (And(mut qs1), q2) => {
                qs1.push(q2);
                *self = And(qs1);
            }
            (Or(qs1), q2) => {
                *self = And(vec![Or(qs1), q2]);
            }
        }
    }

    fn and_ngrams(&mut self, size: usize, set: &LiteralSet) {
        if set.min_len() < size {
            return;
        }
        let mut qor = GramQuery::nothing();
        for lit in &set.lits {
            if lit.len() < size {
                continue;
            }

            let mut set = LiteralSet::new();
            set.extend(ngrams(size, lit).map(BString::from));
            qor.union(GramQuery::from_set(set));
        }
        self.intersect(qor);
    }
}

#[derive(Clone, Debug)]
pub struct Analysis {
    query: GramQuery,
    literals: Literals,
}

#[derive(Clone, Debug)]
pub enum Literals {
    Exact(LiteralSet),
    Inexact(Inexact),
}

#[derive(Clone, Debug)]
struct Inexact {
    prefix: LiteralSet,
    suffix: LiteralSet,
}

impl Analysis {
    fn exact(set: LiteralSet) -> Analysis {
        Analysis {
            query: GramQuery::everything(),
            literals: Literals::Exact(set),
        }
    }

    fn anything() -> Analysis {
        Analysis {
            query: GramQuery::everything(),
            literals: Literals::Inexact(Inexact {
                prefix: LiteralSet::new(),
                suffix: LiteralSet::new(),
            }),
        }
    }

    fn exact_one(string: BString) -> Analysis {
        Analysis {
            query: GramQuery::everything(),
            literals: Literals::Exact(LiteralSet::single(string)),
        }
    }

    fn empty_string() -> Analysis {
        Analysis::exact(LiteralSet::single(BString::from("")))
    }

    fn max_len(&self) -> usize {
        match self.literals {
            Literals::Exact(ref set) => set.len(),
            Literals::Inexact(ref inex) => {
                cmp::max(inex.prefix.len(), inex.suffix.len())
            }
        }
    }

    fn is_exact(&self) -> bool {
        match self.literals {
            Literals::Exact(..) => true,
            _ => false,
        }
    }

    fn make_inexact(&mut self) -> &mut Inexact {
        let exact = match self.literals {
            Literals::Inexact(ref mut inex) => return inex,
            Literals::Exact(ref mut exact) => {
                mem::replace(exact, LiteralSet::new())
            }
        };
        self.literals = Literals::Inexact(Inexact {
            prefix: exact.clone(),
            suffix: exact,
        });
        match self.literals {
            Literals::Inexact(ref mut inex) => inex,
            _ => unreachable!(),
        }
    }

    fn save_exact(&mut self) {
        match self.literals {
            Literals::Inexact(..) => {}
            Literals::Exact(ref set) => {
                self.query.and_ngrams(3, set);
            }
        }
    }

    fn union(&mut self, mut o: Analysis) {
        if self.is_exact() && !o.is_exact() {
            self.save_exact();
        } else if !self.is_exact() && o.is_exact() {
            o.save_exact();
        }
        match o.literals {
            Literals::Exact(set2) => match self.literals {
                Literals::Exact(ref mut set1) => {
                    set1.union(set2);
                }
                Literals::Inexact(ref mut inex1) => {
                    inex1.prefix.union(set2.clone());
                    inex1.suffix.union(set2);
                }
            },
            Literals::Inexact(inex2) => {
                let inex1 = self.make_inexact();
                inex1.prefix.union(inex2.prefix);
                inex1.suffix.union(inex2.suffix);
            }
        }
        self.query.union(o.query);
    }

    fn concat(&mut self, o: Analysis) {
        self.query.intersect(o.query);
        match o.literals {
            Literals::Exact(set2) => match self.literals {
                Literals::Exact(ref mut set1) => {
                    set1.cross(set2);
                }
                Literals::Inexact(ref mut inex1) => {
                    if inex1.prefix.has_empty() {
                        inex1.prefix.union(set2.clone());
                    }
                    inex1.suffix.cross(set2);
                }
            },
            Literals::Inexact(inex2) => {
                let was_exact = self.is_exact();
                let inex1 = self.make_inexact();

                if was_exact {
                    inex1.prefix.cross(inex2.prefix);
                } else if inex1.prefix.has_empty() {
                    inex1.prefix.union(inex2.prefix);
                }

                let old_suffix = mem::replace(&mut inex1.suffix, inex2.suffix);
                if inex1.suffix.has_empty() {
                    inex1.suffix.union(old_suffix);
                }
            }
        }
    }

    // BREADCRUMBS: This is hard/awkward to implement with our current
    // representation. Get rid of enum and just use exact/prefix/suffix.
    fn simplify(&mut self, force: bool) {
        // if let Some(ref
    }
}

#[derive(Clone, Debug)]
struct LiteralSet {
    lits: Vec<BString>,
}

impl LiteralSet {
    fn new() -> LiteralSet {
        LiteralSet { lits: vec![] }
    }

    fn single(lit: BString) -> LiteralSet {
        LiteralSet { lits: vec![lit] }
    }

    fn clear(&mut self) {
        self.lits.clear();
    }

    fn extend<I: IntoIterator<Item = BString>>(&mut self, it: I) {
        self.lits.extend(it);
        self.canonicalize();
    }

    fn canonicalize(&mut self) {
        self.lits.sort();
        self.lits.dedup();
    }

    fn union(&mut self, o: LiteralSet) {
        self.lits.extend(o.lits);
        self.canonicalize();
    }

    fn cross(&mut self, o: LiteralSet) {
        if o.is_empty() || o.has_only_empty() {
            return;
        }
        if self.is_empty() || self.has_only_empty() {
            *self = o;
            return;
        }

        let orig = mem::replace(&mut self.lits, vec![]);
        for selflit in &orig {
            for olit in &o.lits {
                let mut newlit = selflit.clone();
                newlit.push_str(olit);
                self.lits.push(newlit);
            }
        }
    }

    fn is_empty(&self) -> bool {
        self.lits.is_empty()
    }

    fn len(&self) -> usize {
        self.lits.len()
    }

    fn min_len(&self) -> usize {
        self.lits.iter().map(|x| x.len()).min().unwrap_or(0)
    }

    fn has_empty(&self) -> bool {
        self.lits.get(0).map_or(false, |x| x.is_empty())
    }

    fn has_only_empty(&self) -> bool {
        self.len() == 1 && self.has_empty()
    }
}

#[derive(Clone, Debug)]
pub struct AnalysisBuilder {
    ngram_size: usize,
    limit_len: usize,
    limit_class: usize,
}

impl AnalysisBuilder {
    pub fn new() -> AnalysisBuilder {
        AnalysisBuilder { ngram_size: 3, limit_len: 250, limit_class: 10 }
    }

    pub fn ngram_size(&mut self, size: usize) -> &mut AnalysisBuilder {
        self.ngram_size = size;
        self
    }

    pub fn limit_len(&mut self, len: usize) -> &mut AnalysisBuilder {
        self.limit_len = len;
        self
    }

    pub fn limit_class(&mut self, len: usize) -> &mut AnalysisBuilder {
        self.limit_class = len;
        self
    }

    pub fn build(&self, exp: &Hir) -> Analysis {
        match *exp.kind() {
            HirKind::Empty | HirKind::Anchor(_) | HirKind::WordBoundary(_) => {
                Analysis::empty_string()
            }
            HirKind::Literal(hir::Literal::Unicode(ch)) => {
                let mut lit = BString::from(vec![]);
                lit.push_char(ch);
                Analysis::exact_one(lit)
            }
            HirKind::Literal(hir::Literal::Byte(b)) => {
                let mut lit = BString::from(vec![]);
                lit.push_byte(b);
                Analysis::exact_one(lit)
            }
            HirKind::Class(hir::Class::Unicode(ref cls)) => {
                if class_over_limit_unicode(cls, self.limit_class) {
                    return Analysis::anything();
                }

                let mut set = LiteralSet::new();
                for r in cls.iter() {
                    for cp in (r.start() as u32)..=(r.end() as u32) {
                        let ch = match std::char::from_u32(cp) {
                            None => continue,
                            Some(ch) => ch,
                        };
                        set.lits.push(BString::from(ch.to_string()));
                    }
                }
                set.canonicalize();
                Analysis::exact(set)
            }
            HirKind::Class(hir::Class::Bytes(ref cls)) => {
                if class_over_limit_bytes(cls, self.limit_class) {
                    return Analysis::anything();
                }

                let mut set = LiteralSet::new();
                for r in cls.iter() {
                    for b in r.start()..=r.end() {
                        set.lits.push(BString::from(vec![b]));
                    }
                }
                set.canonicalize();
                Analysis::exact(set)
            }
            HirKind::Group(ref group) => self.build(&group.hir),
            HirKind::Repetition(ref rep) => {
                if rep.is_match_empty() {
                    Analysis::anything()
                } else {
                    let mut ana = self.build(&rep.hir);
                    ana.make_inexact();
                    ana
                }
            }
            HirKind::Alternation(ref exps) => {
                let mut ana = self.build(&exps[0]);
                for e in exps.iter().skip(1) {
                    ana.union(self.build(e));
                }
                ana
            }
            HirKind::Concat(ref exps) => {
                let mut exps = combine_literals(exps);
                let mut ana = Analysis::exact(LiteralSet::new());
                for e in exps {
                    let next = self.build_literal_or_hir(e);
                    if ana.max_len() + ana.max_len() > self.limit_len {
                        ana.concat(Analysis::anything());
                    } else {
                        ana.concat(next);
                    }
                }
                ana
            }
        }
    }

    fn build_literal_or_hir(&self, or: LiteralOrHir) -> Analysis {
        match or {
            LiteralOrHir::Literal(string) => Analysis::exact_one(string),
            LiteralOrHir::Other(exp) => self.build(exp),
        }
    }
}

impl Default for AnalysisBuilder {
    fn default() -> AnalysisBuilder {
        AnalysisBuilder::new()
    }
}

fn class_over_limit_unicode(cls: &hir::ClassUnicode, limit: usize) -> bool {
    let mut count = 0;
    for r in cls.iter() {
        if count > limit {
            return true;
        }
        count += (r.end() as u32 - r.start() as u32) as usize;
    }
    count > limit
}

fn class_over_limit_bytes(cls: &hir::ClassBytes, limit: usize) -> bool {
    let mut count = 0;
    for r in cls.iter() {
        if count > limit {
            return true;
        }
        count += (r.end() - r.start()) as usize;
    }
    count > limit
}

#[derive(Debug)]
enum LiteralOrHir<'a> {
    Literal(BString),
    // Guaranteed to never contain a HirKind::Literal.
    Other(&'a Hir),
}

fn combine_literals(concat: &[Hir]) -> Vec<LiteralOrHir> {
    let mut combined = vec![];
    let mut lit = BString::from(vec![]);
    for exp in concat {
        match *exp.kind() {
            HirKind::Literal(hir::Literal::Unicode(ch)) => {
                lit.push_char(ch);
            }
            HirKind::Literal(hir::Literal::Byte(b)) => {
                lit.push_byte(b);
            }
            _ => {
                if !lit.is_empty() {
                    combined.push(LiteralOrHir::Literal(lit));
                    lit = BString::from(vec![]);
                }
                combined.push(LiteralOrHir::Other(exp));
            }
        }
    }
    if !lit.is_empty() {
        combined.push(LiteralOrHir::Literal(lit));
    }
    combined
}

/// Returns all ngrams of the given size in a sliding window fashion over the
/// given literal. If the literal is smaller than the given size, then the
/// entire literal is returned as an ngram. (An empty literal always results in
/// a single empty string returned.)
fn ngrams<'b, B: 'b + AsRef<[u8]> + ?Sized>(
    size: usize,
    lit: &'b B,
) -> impl Iterator<Item = &'b [u8]> {
    let lit = lit.as_ref();
    let size = cmp::min(size, lit.len());
    let end = lit.len() - size;
    (0..=end).map(move |i| &lit[i..i + size])
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex_syntax::ParserBuilder;

    fn parse(pattern: &str) -> Hir {
        ParserBuilder::new()
            .allow_invalid_utf8(true)
            .build()
            .parse(pattern)
            .unwrap()
    }

    fn analysis(pattern: &str) -> Analysis {
        let re = parse(pattern);
        let mut b = AnalysisBuilder::new();
        // b.limit_len(3);
        b.build(&re)
    }

    #[test]
    fn iter_ngrams() {
        let get = |size, lit| -> Vec<BString> {
            ngrams(size, lit).map(BString::from).collect()
        };

        assert_eq!(get(3, "foobar"), vec!["foo", "oob", "oba", "bar"]);
        assert_eq!(get(3, "fooba"), vec!["foo", "oob", "oba"]);
        assert_eq!(get(3, "foob"), vec!["foo", "oob"]);
        assert_eq!(get(3, "foo"), vec!["foo"]);
        assert_eq!(get(3, "fo"), vec!["fo"]);
        assert_eq!(get(3, "f"), vec!["f"]);
        assert_eq!(get(3, ""), vec![""]);

        assert_eq!(get(1, "ab"), vec!["a", "b"]);
        assert_eq!(get(1, "a"), vec!["a"]);
        assert_eq!(get(1, ""), vec![""]);

        assert_eq!(get(0, "ab"), vec!["", "", ""]);
        assert_eq!(get(0, "a"), vec!["", ""]);
        assert_eq!(get(0, ""), vec![""]);
    }

    #[test]
    fn scratch() {
        // dbg!(analysis(r"a|b|c"));
        // dbg!(analysis(r"[2-6]"));
        // dbg!(analysis(r"abcQ+def(QQ)+xyz"));
        // dbg!(analysis(r".abc(XYZ)+"));
        // dbg!(analysis(r".(a)(yz)"));
        // dbg!(analysis(r"abc.def.ghi"));
        // dbg!(analysis(r"ZZZ+(foo|bar|baz)(a|b)"));
        dbg!(analysis(r"aND|caN|Ha[DS]|WaS"));
        // dbg!(analysis(r"\|[^|][^|]*\|"));
        // dbg!(analysis(r"a[act]ggtaaa|tttacc[agt]t"));
        // dbg!(analysis(r">[^\n]*\n|\n"));
    }
}
