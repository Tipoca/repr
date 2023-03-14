# Usage

```rust
use repr::{Repr, constants::DIGIT};
let re = DIGIT.rep(4) * '-' * DIGIT.rep(2) * '-' * DIGIT.rep(2);
assert!(re.is_match("2014-01-01"));
```
