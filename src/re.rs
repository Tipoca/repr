// Don't ever compile Save instructions for regex sets because
// they are never used. They are also never used in DFA programs
// because DFAs can't handle captures.
        
use alloc::vec;

use unconst::unconst;

use crate::{Partition, Context};
use crate::exec::Exec;
use crate::options::Options;
use crate::repr::{Repr, Integral};

/// Escapes all regular expression meta characters in `text`.
///
/// The string returned may be safely used as a literal in a regular
/// expression.
pub fn escape<I: Integral>(context: &Context<I>) -> String {
    regex_syntax::escape(context)
}

#[derive(Clone)]
pub struct Regex<I: Integral>(Exec<I>);

/// Core regular expression methods.
impl<I: Integral> Regex<I> {
    /// Returns the start and end byte range of the leftmost-first match in
    /// `text`. If no match exists, then `None` is returned.
    ///
    /// Note that this should only be used if you want to discover the position
    /// of the match. Testing the existence of a match is faster if you use
    /// `is_match`.
    ///
    /// # Example
    ///
    /// Find the start and end location of the first word with exactly 13
    /// Unicode word characters:
    ///
    /// ```rust
    /// # use regex::Regex;
    /// # fn main() {
    /// let text = "I categorically deny having triskaidekaphobia.";
    /// let mat = Regex::new(r"\b\w{13}\b").unwrap().find(text).unwrap();
    /// assert_eq!(mat.start(), 2);
    /// assert_eq!(mat.end(), 15);
    /// # }
    /// ```
    pub fn find<'t>(&self, text: &'t str) -> Option<Match<'t>> {
        self.find_at(text, 0)
    }

    /// Returns an iterator for each successive non-overlapping match in
    /// `text`, returning the start and end byte indices with respect to
    /// `text`.
    ///
    /// # Example
    ///
    /// Find the start and end location of every word with exactly 13 Unicode
    /// word characters:
    ///
    /// ```rust
    /// # use regex::Regex;
    /// # fn main() {
    /// let text = "Retroactively relinquishing remunerations is reprehensible.";
    /// for mat in Regex::new(r"\b\w{13}\b").unwrap().find_iter(text) {
    ///     println!("{:?}", mat);
    /// }
    /// # }
    /// ```
    pub fn find_iter<'r, 't>(&'r self, text: &'t str) -> Partition<'r, 't, I> {
        Partition(self.0.searcher().find_iter(text))
    }
}

/// Advanced or "lower level" search methods.
impl<I: Integral> Regex<I> {


    /// Returns the same as find, but starts the search at the given
    /// offset.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    pub fn find_at<'t>(&self, text: &'t str, start: usize)
        -> Option<Match<'t>>
    {
        self.0
            .searcher()
            .find_at(text, start)
            .map(|(s, e)| Match::new(text, s, e))
    }
}

/// Match multiple (possibly overlapping) regular expressions in a single scan.
///
/// A regex set corresponds to the union of two or more regular expressions.
/// That is, a regex set will match text where at least one of its
/// constituent regular expressions matches. A regex set as its formulated here
/// provides a touch more power: it will also report *which* regular
/// expressions in the set match. Indeed, this is the key difference between
/// regex sets and a single `Regex` with many alternates, since only one
/// alternate can match at a time.
///
/// For example, consider regular expressions to match email addresses and
/// domains: `[a-z]+@[a-z]+\.(com|org|net)` and `[a-z]+\.(com|org|net)`. If a
/// regex set is constructed from those regexes, then searching the text
/// `foo@example.com` will report both regexes as matching. Of course, one
/// could accomplish this by compiling each regex on its own and doing two
/// searches over the text. The key advantage of using a regex set is that it
/// will report the matching regexes using a *single pass through the text*.
/// If one has hundreds or thousands of regexes to match repeatedly (like a URL
/// router for a complex web application or a user agent matcher), then a regex
/// set can realize huge performance gains.
///
/// # Example
///
/// This shows how the above two regexes (for matching email addresses and
/// domains) might work:
///
/// ```rust
/// # use regex::RegexSet;
/// let set = RegexSet::new(&[
///     r"[a-z]+@[a-z]+\.(com|org|net)",
///     r"[a-z]+\.(com|org|net)",
/// ]).unwrap();
///
/// // Ask whether any regexes in the set match.
/// assert!(set.is_match("foo@example.com"));
///
/// // Identify which regexes in the set match.
/// let matches: Vec<_> = set.matches("foo@example.com").into_iter().collect();
/// assert_eq!(vec![0, 1], matches);
///
/// // Try again, but with text that only matches one of the regexes.
/// let matches: Vec<_> = set.matches("example.com").into_iter().collect();
/// assert_eq!(vec![1], matches);
///
/// // Try again, but with text that doesn't match any regex in the set.
/// let matches: Vec<_> = set.matches("example").into_iter().collect();
/// assert!(matches.is_empty());
/// ```
///
/// Note that it would be possible to adapt the above example to using `Regex`
/// with an expression like:
///
/// ```text
/// (?P<email>[a-z]+@(?P<email_domain>[a-z]+[.](com|org|net)))|(?P<domain>[a-z]+[.](com|org|net))
/// ```
///
/// After a match, one could then inspect the capture groups to figure out
/// which alternates matched. The problem is that it is hard to make this
/// approach scale when there are many regexes since the overlap between each
/// alternate isn't always obvious to reason about.
///
/// # Limitations
///
/// Regex sets are limited to answering the following two questions:
///
/// 1. Does any regex in the set match?
/// 2. If so, which regexes in the set match?
///
/// As with the main [`Regex`][crate::Regex] type, it is cheaper to ask (1)
/// instead of (2) since the matching engines can stop after the first match
/// is found.
///
/// You cannot directly extract [`Match`][crate::Match] or
/// [`Captures`][crate::Captures] objects from a regex set. If you need these
/// operations, the recommended approach is to compile each pattern in the set
/// independently and scan the exact same input a second time with those
/// independently compiled patterns:
///
/// ```rust
/// use regex::{Regex, RegexSet};
///
/// let patterns = ["foo", "bar"];
/// // Both patterns will match different ranges of this string.
/// let text = "barfoo";
///
/// // Compile a set matching any of our patterns.
/// let set = RegexSet::new(&patterns).unwrap();
/// // Compile each pattern independently.
/// let regexes: Vec<_> = set.patterns().iter()
///     .map(|pat| Regex::new(pat).unwrap())
///     .collect();
///
/// // Match against the whole set first and identify the individual
/// // matching patterns.
/// let matches: Vec<&str> = set.matches(text).into_iter()
///     // Dereference the match index to get the corresponding
///     // compiled pattern.
///     .map(|match_idx| &regexes[match_idx])
///     // To get match locations or any other info, we then have to search
///     // the exact same text again, using our separately-compiled pattern.
///     .map(|pat| pat.find(text).unwrap().as_str())
///     .collect();
///
/// // Matches arrive in the order the constituent patterns were declared,
/// // not the order they appear in the input.
/// assert_eq!(vec!["foo", "bar"], matches);
/// ```
///
/// # Performance
///
/// A `RegexSet` has the same performance characteristics as `Regex`. Namely,
/// search takes `O(mn)` time, where `m` is proportional to the size of the
/// regex set and `n` is proportional to the length of the search text.
#[derive(Clone)]
pub struct RegexSet<I: Integral>(Exec<I>);

impl<I: Integral> RegexSet<I> {
    /// Create a new regex set with the given regular expressions.
    ///
    /// This takes an iterator of `S`, where `S` is something that can produce
    /// a `&str`. If any of the strings in the iterator are not valid regular
    /// expressions, then an error is returned.
    ///
    /// # Example
    ///
    /// Create a new regex set from an iterator of strings:
    ///
    /// ```rust
    /// # use regex::RegexSet;
    /// let set = RegexSet::new(&[r"\w+", r"\d+"]).unwrap();
    /// assert!(set.is_match("foo"));
    /// ```
    pub fn new(repr: Repr<I>) -> Exec<I> {
        Options::new(repr).build()
    }

    /// Returns the set of regular expressions that match in the given text.
    ///
    /// The set returned contains the index of each regular expression that
    /// matches in the given text. The index is in correspondence with the
    /// order of regular expressions given to `RegexSet`'s constructor.
    ///
    /// The set can also be used to iterate over the matched indices.
    ///
    /// Note that as with searches using `Regex`, the expression is unanchored
    /// by default. That is, if the regex does not start with `^` or `\A`, or
    /// end with `$` or `\z`, then it is permitted to match anywhere in the
    /// text.
    ///
    /// # Example
    ///
    /// Tests which regular expressions match the given text:
    ///
    /// ```rust
    /// # use regex::RegexSet;
    /// let set = RegexSet::new(&[
    ///     r"\w+",
    ///     r"\d+",
    ///     r"\pL+",
    ///     r"foo",
    ///     r"bar",
    ///     r"barfoo",
    ///     r"foobar",
    /// ]).unwrap();
    /// let matches: Vec<_> = set.matches("foobar").into_iter().collect();
    /// assert_eq!(matches, vec![0, 2, 3, 4, 6]);
    ///
    /// // You can also test whether a particular regex matched:
    /// let matches = set.matches("foobar");
    /// assert!(!matches.matched(5));
    /// assert!(matches.matched(6));
    /// ```
    pub fn matches(&self, context: &Context<I>) -> SetMatches {
        let mut matches = vec![false; self.0.regex_strings().len()];
        let any = self.read_matches_at(&mut matches, context, 0);
        SetMatches {
            matched_any: any,
            matches: matches,
        }
    }

    /// Returns the same as matches, but starts the search at the given
    /// offset and stores the matches into the slice given.
    ///
    /// The significance of the starting point is that it takes the surrounding
    /// context into consideration. For example, the `\A` anchor can only
    /// match when `start == 0`.
    ///
    /// `matches` must have a length that is at least the number of regexes
    /// in this set.
    ///
    /// This method returns true if and only if at least one member of
    /// `matches` is true after executing the set against `text`.
    #[doc(hidden)]
    pub fn read_matches_at(
        &self,
        matches: &mut [bool],
        context: &Context<I>,
        start: usize,
    ) -> bool {
        self.0.searcher().many_matches_at(matches, context, start)
    }
}
