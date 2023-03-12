use std::fmt;
use std::iter::FusedIterator;

/// `RegularExpression` describes types that can implement regex searching.
///
/// This trait is my attempt at reducing code duplication and to standardize
/// the internal API. Specific duplication that is avoided are the `find`
/// and `capture` iterators, which are slightly tricky.
///
/// It's not clear whether this trait is worth it, and it also isn't
/// clear whether it's useful as a public trait or not. Methods like
/// `next_after_empty` reak of bad design, but the rest of the methods seem
/// somewhat reasonable. One particular thing this trait would expose would be
/// the ability to start the search of a regex anywhere in a haystack, which
/// isn't possible in the current public API.
pub trait RegularExpression: Sized {
    /// The type of the haystack.
    type Text: ?Sized + fmt::Debug;

    /// Returns the location of the shortest match.
    fn shortest_match_at(
        &self,
        text: &Self::Text,
        start: usize,
    ) -> Option<usize>;

    /// Returns whether the regex matches the text given.
    fn is_match_at(&self, text: &Self::Text, start: usize) -> bool;

    /// Returns the leftmost-first match location if one exists.
    fn find_at(
        &self,
        text: &Self::Text,
        start: usize,
    ) -> Option<(usize, usize)>;

    /// Returns an iterator over all non-overlapping successive leftmost-first
    /// matches.
    fn find_iter(self, text: &Self::Text) -> Matches<'_, Self> {
        Matches { re: self, text, last_end: 0, last_match: None }
    }

    /// Returns an iterator over all non-overlapping successive leftmost-first
    /// matches with captures.
    fn captures_iter(self, text: &Self::Text) -> CaptureMatches<'_, Self> {
        CaptureMatches(self.find_iter(text))
    }
}

/// An iterator over all non-overlapping successive leftmost-first matches.
#[derive(Debug)]
pub struct Matches<'t, R>
    where
        R: RegularExpression,
        R::Text: 't,
{
    re: R,
    text: &'t R::Text,
    last_end: usize,
    last_match: Option<usize>,
}

impl<'t, R> Matches<'t, R>
where
    R: RegularExpression,
    R::Text: 't,
{
    /// Return the text being searched.
    pub fn text(&self) -> &'t R::Text {
        self.text
    }

    /// Return the underlying regex.
    pub fn regex(&self) -> &R {
        &self.re
    }
}

impl<'t, R> Iterator for Matches<'t, R>
where
    R: RegularExpression,
    R::Text: 't + AsRef<[u8]>,
{
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
        if self.last_end > self.text.as_ref().len() {
            return None;
        }
        let (s, e) = match self.re.find_at(self.text, self.last_end) {
            None => return None,
            Some((s, e)) => (s, e),
        };
        if s == e {
            // This is an empty match. To ensure we make progress, start
            // the next search at the smallest possible starting position
            // of the next match following this one.
            self.last_end = e + 1;
            // Don't accept empty matches immediately following a match.
            // Just move on to the next match.
            if Some(e) == self.last_match {
                return self.next();
            }
        } else {
            self.last_end = e;
        }
        self.last_match = Some(e);
        Some((s, e))
    }
}

impl<'t, R> FusedIterator for Matches<'t, R>
where
    R: RegularExpression,
    R::Text: 't + AsRef<[u8]>,
{
}

/// An iterator over all non-overlapping successive leftmost-first matches with
/// captures.
#[derive(Debug)]
pub struct CaptureMatches<'t, R>(Matches<'t, R>)
where
    R: RegularExpression,
    R::Text: 't;

impl<'t, R> CaptureMatches<'t, R>
where
    R: RegularExpression,
    R::Text: 't,
{
    /// Return the text being searched.
    pub fn text(&self) -> &'t R::Text {
        self.0.text()
    }

    /// Return the underlying regex.
    pub fn regex(&self) -> &R {
        self.0.regex()
    }
}
