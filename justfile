_default:
  @just --list

clippy:
    cargo clippy --workspace --all-features --tests -- -D warnings

doc:
    RUSTDOCFLAGS="-D warnings" cargo doc --workspace --all-features --keep-going

docs:
    mdbook build docs
    lychee --offline --include-fragments --index-files index.html --exclude-path docs/book/404.html --no-progress docs/book

format:
    cargo fmt --all

test:
    cargo test --workspace --all-features
