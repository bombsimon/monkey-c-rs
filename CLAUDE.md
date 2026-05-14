# Project Guidelines

## Architecture

- monkey-c-parser contains parser/lexer code to donvert Monkey C code to AST
- monkey-c-formatter contains document IR and code to write AST back to code

## Styling

- Import statements should always go at the top
- Avoid using import aliases as much as possible
- Avoid importing ambigous types - keep path if it helps clarity (e.g. don't
  import `token::Type` - always refer to `token::Type` with the `token` part
- Use semantic newlines to make code airy and easy to read
  - `return` statements should preceed with a newline
  - Final statement/returning values should be separated by a newline
  - A newline should be added after all block (`{}`)
- Keep happy path to the left and use early returns when possible. Prefer `if
let .. else {}` pattern where sensible

## Code

- Prefer easy to read/easy to follow code over pre-mature optimizations
- Always run `cargo fmt --all` to format all code
- Always use `cargo clippy --all-features --tests` and address all warnings
- Always run `cargo doc` to ensure proper documentation.

## Testing

- Prefer small units of tests over larger integration tests

## Documentation

- Only document WHY some things are made, not WHAT they do. Code should tell
  that
- Describe types and methods at high level to avoid risk of comments getting out
  of date or out of sync with what the code does
- Avoid code separation comments, just group logical units. If it grows to big,
  split over separate files or modules.
