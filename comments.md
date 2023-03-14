# exec.rs::is_match

Returns true if and only if one of the regexes in this set matches
the text given.

This method should be preferred if you only need to test whether any
of the regexes in the set should match, but don't care about *which*
regexes matched. This is because the underlying matching engine will
quit immediately after seeing the first match instead of continuing to
find all matches.

Note that as with searches using `Regex`, the expression is unanchored
by default. That is, if the regex does not start with `^` or `\A`, or
end with `$` or `\z`, then it is permitted to match anywhere in the
text.

## Example

Tests whether a set matches some text:

```rust
# use regex::RegexSet;
let set = RegexSet::new(&[r"\w+", r"\d+"]).unwrap();
assert!(set.is_match("foo"));
assert!(!set.is_match("☃"));
```

Test if some text contains at least one word with exactly 13
Unicode word characters:

```rust
# use regex::Regex;
# fn main() {
let text = "I categorically deny having triskaidekaphobia.";
assert!(Regex::new(r"\b\w{13}\b").unwrap().is_match(text));
# }
```

---

# exec.rs::is_match_at

Returns the same as is_match, but starts the search at the given
offset.

For single regular expressions, this is equivalent to calling
`shortest_match(...).is_some()`.

The significance of the starting point is that it takes the surrounding
context into consideration. For example, the `\A` anchor can only
match when `start == 0`.

---

# exec.rs::shortest_match

Returns the end location of a match in the text given.

This method may have the same performance characteristics as
`is_match`, except it provides an end location for a match. In
particular, the location returned *may be shorter* than the proper end
of the leftmost-first match.

## Example

Typically, `a+` would match the entire first sequence of `a` in some
text, but `shortest_match` can give up as soon as it sees the first
`a`.

```rust
# use regex::Regex;
# fn main() {
let text = "aaaaa";
let pos = Regex::new(r"a+").unwrap().shortest_match(text);
assert_eq!(pos, Some(1));
# }
```

---

# exec.rs::shortest_match_at

Returns the end of a match location, possibly occurring before the
end location of the correct leftmost-first match.

Returns the same as shortest_match, but starts the search at the given
offset.

The significance of the starting point is that it takes the surrounding
context into consideration. For example, the `\A` anchor can only
match when `start == 0`.

---

# exec.rs::find

Returns the start and end byte range of the leftmost-first match in
`text`. If no match exists, then `None` is returned.

Note that this should only be used if you want to discover the position
of the match. Testing the existence of a match is faster if you use
`is_match`.

## Example

Find the start and end location of the first word with exactly 13
Unicode word characters:

```rust
# use regex::Regex;
# fn main() {
let text = "I categorically deny having triskaidekaphobia.";
let mat = Regex::new(r"\b\w{13}\b").unwrap().find(text).unwrap();
assert_eq!(mat.start(), 2);
assert_eq!(mat.end(), 15);
# }
```

---

# exec.rs::find_at

Finds the start and end location of the leftmost-first match, starting
at the given location.

Returns the same as find, but starts the search at the given
offset.

The significance of the starting point is that it takes the surrounding
context into consideration. For example, the `\A` anchor can only
match when `start == 0`.

---

# exec.rs::find_iter

Returns an iterator for each successive non-overlapping match in
`text`, returning the start and end byte indices with respect to
`text`.

## Example

Find the start and end location of every word with exactly 13 Unicode
word characters:

```rust
# use regex::Regex;
# fn main() {
let text = "Retroactively relinquishing remunerations is reprehensible.";
for mat in Regex::new(r"\b\w{13}\b").unwrap().find_iter(text) {
    println!("{:?}", mat);
}
# }
```

---

# exec.rs::matches

Returns the set of regular expressions that match in the given text.

The set returned contains the index of each regular expression that
matches in the given text. The index is in correspondence with the
order of regular expressions given to `RegexSet`'s constructor.

The set can also be used to iterate over the matched indices.

Note that as with searches using `Regex`, the expression is unanchored
by default. That is, if the regex does not start with `^` or `\A`, or
end with `$` or `\z`, then it is permitted to match anywhere in the
text.

## Example

Tests which regular expressions match the given text:

```rust
# use regex::RegexSet;
let set = RegexSet::new(&[
    r"\w+",
    r"\d+",
    r"\pL+",
    r"foo",
    r"bar",
    r"barfoo",
    r"foobar",
]).unwrap();
let matches: Vec<_> = set.matches("foobar").into_iter().collect();
assert_eq!(matches, vec![0, 2, 3, 4, 6]);

// You can also test whether a particular regex matched:
let matches = set.matches("foobar");
assert!(!matches.matched(5));
assert!(matches.matched(6));
```

---

## exec.rs::read_matches_at

Returns the same as matches, but starts the search at the given
offset and stores the matches into the slice given.

The significance of the starting point is that it takes the surrounding
context into consideration. For example, the `\A` anchor can only
match when `start == 0`.

`matches` must have a length that is at least the number of regexes
in this set.

This method returns true if and only if at least one member of
`matches` is true after executing the set against `text`.

---

# exec.rs::many_matches_at

Finds which regular expressions match the given text.

`matches` should have length equal to the number of regexes being
searched.

This is only useful when one wants to know which regexes in a set
match some text.

---

- Repr is Add
- Literals are partial

---

If our set of prefixes is complete, then we can use it to find a match in lieu of a regex engine. This doesn't quite work well in the presence of multiple regexes, so only do it when there's one.

TODO(burntsushi): Also, don't try to match literals if the regex is partially anchored. We could technically do it, but we'd need to create two sets of literals: all of them and then the subset that aren't anchored. We would then only search for all of them when at the beginning of the input and use the subset in all other cases.

---

\Note that when compiling 2 or more regular expressions, capture groups
are completely unsupported. (This means both `find` and `captures`
won't work.)

---

A compiled regular expression for matching Unicode strings.

It is represented as either a sequence of bytecode instructions (dynamic)
or as a specialized Rust function (native). It can be used to search, split
or replace text. All searching is done with an implicit `.*?` at the
beginning and end of an expression. To force an expression to match the
whole string (or a prefix or a suffix), you must use an anchor like `^` or
`$` (or `\A` and `\z`).

While this crate will handle Unicode strings (whether in the regular
expression or in the search text), all positions returned are **byte
indices**. Every byte index is guaranteed to be at a Unicode code point
boundary.

The lifetimes `'r` and `'t` in this crate correspond to the lifetime of a
compiled regular expression and text to search, respectively.

The only methods that allocate new strings are the string replacement
methods. All other methods (searching and splitting) return borrowed
pointers into the string given.

# Examples

Find the location of a US phone number:

```rust
# use regex::Regex;
let re = Regex::new("[0-9]{3}-[0-9]{3}-[0-9]{4}").unwrap();
let mat = re.find("phone: 111-222-3333").unwrap();
assert_eq!((mat.start(), mat.end()), (7, 19));
```

# Using the `std::str::pattern` methods with `Regex`

> **Note**: This section requires that this crate is compiled with the
> `pattern` Cargo feature enabled, which **requires nightly Rust**.

Since `Regex` implements `Pattern`, you can use regexes with methods
defined on `&str`. For example, `is_match`, `find`, `find_iter`
and `split` can be replaced with `str::contains`, `str::find`,
`str::match_indices` and `str::split`.

Here are some examples:

```rust,ignore
# use regex::Regex;
let re = Regex::new(r"\d+").unwrap();
let haystack = "a111b222c";

assert!(haystack.contains(&re));
assert_eq!(haystack.find(&re), Some(1));
assert_eq!(haystack.match_indices(&re).collect::<Vec<_>>(),
           vec![(1, "111"), (5, "222")]);
assert_eq!(haystack.split(&re).collect::<Vec<_>>(), vec!["a", "b", "c"]);
```

Match multiple (possibly overlapping) regular expressions in a single scan.

A regex set corresponds to the union of two or more regular expressions.
That is, a regex set will match text where at least one of its
constituent regular expressions matches. A regex set as its formulated here
provides a touch more power: it will also report *which* regular
expressions in the set match. Indeed, this is the key difference between
regex sets and a single `Regex` with many alternates, since only one
alternate can match at a time.

For example, consider regular expressions to match email addresses and
domains: `[a-z]+@[a-z]+\.(com|org|net)` and `[a-z]+\.(com|org|net)`. If a
regex set is constructed from those regexes, then searching the text
`foo@example.com` will report both regexes as matching. Of course, one
could accomplish this by compiling each regex on its own and doing two
searches over the text. The key advantage of using a regex set is that it
will report the matching regexes using a *single pass through the text*.
If one has hundreds or thousands of regexes to match repeatedly (like a URL
router for a complex web application or a user agent matcher), then a regex
set can realize huge performance gains.

# Example

This shows how the above two regexes (for matching email addresses and
domains) might work:

```rust
# use regex::RegexSet;
let set = RegexSet::new(&[
    r"[a-z]+@[a-z]+\.(com|org|net)",
    r"[a-z]+\.(com|org|net)",
]).unwrap();

// Ask whether any regexes in the set match.
assert!(set.is_match("foo@example.com"));

// Identify which regexes in the set match.
let matches: Vec<_> = set.matches("foo@example.com").into_iter().collect();
assert_eq!(vec![0, 1], matches);

// Try again, but with text that only matches one of the regexes.
let matches: Vec<_> = set.matches("example.com").into_iter().collect();
assert_eq!(vec![1], matches);

// Try again, but with text that doesn't match any regex in the set.
let matches: Vec<_> = set.matches("example").into_iter().collect();
assert!(matches.is_empty());
```

Note that it would be possible to adapt the above example to using `Regex`
with an expression like:

```text
(?P<email>[a-z]+@(?P<email_domain>[a-z]+[.](com|org|net)))|(?P<domain>[a-z]+[.](com|org|net))
```

After a match, one could then inspect the capture groups to figure out
which alternates matched. The problem is that it is hard to make this
approach scale when there are many regexes since the overlap between each
alternate isn't always obvious to reason about.

# Limitations

Regex sets are limited to answering the following two questions:

1. Does any regex in the set match?
2. If so, which regexes in the set match?

As with the main [`Regex`][crate::Regex] type, it is cheaper to ask (1)
instead of (2) since the matching engines can stop after the first match
is found.

You cannot directly extract [`Match`][crate::Match] or
[`Captures`][crate::Captures] objects from a regex set. If you need these
operations, the recommended approach is to compile each pattern in the set
independently and scan the exact same input a second time with those
independently compiled patterns:

```rust
use regex::{Regex, RegexSet};

let patterns = ["foo", "bar"];
// Both patterns will match different ranges of this string.
let text = "barfoo";

// Compile a set matching any of our patterns.
let set = RegexSet::new(&patterns).unwrap();
// Compile each pattern independently.
let regexes: Vec<_> = set.patterns().iter()
    .map(|pat| Regex::new(pat).unwrap())
    .collect();

// Match against the whole set first and identify the individual
// matching patterns.
let matches: Vec<&str> = set.matches(text).into_iter()
    // Dereference the match index to get the corresponding
    // compiled pattern.
    .map(|match_idx| &regexes[match_idx])
    // To get match locations or any other info, we then have to search
    // the exact same text again, using our separately-compiled pattern.
    .map(|pat| pat.find(text).unwrap().as_str())
    .collect();

// Matches arrive in the order the constituent patterns were declared,
// not the order they appear in the input.
assert_eq!(vec!["foo", "bar"], matches);
```

# Performance

A `RegexSet` has the same performance characteristics as `Regex`. Namely,
search takes `O(mn)` time, where `m` is proportional to the size of the
regex set and `n` is proportional to the length of the search text.
