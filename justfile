crates := "monkey-c-formatter monkey-c-parser"

_default:
  @just --list

clippy: (_cmd 'cargo clippy --all-features --tests')

doc: (_cmd 'RUSTDOCFLAGS="-D warnings" cargo doc --all-features --workspace --keep-going')

format: (_cmd 'cargo fmt --all')

test: (_cmd 'cargo test --all-features')

_cmd *args='':
    #!/bin/sh
    for f in {{crates}}
    do
      cd $f
      {{args}}
      cd -
    done

