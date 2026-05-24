_default:
  @just --list

clippy:
    cargo clippy --workspace --all-features --tests -- -D warnings

doc:
    RUSTDOCFLAGS="-D warnings" cargo doc --workspace --all-features --keep-going

format:
    cargo fmt --all

test:
    cargo test --workspace --all-features
