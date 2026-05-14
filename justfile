crates := "monkey-c-formatter monkey-c-parser"

default:
  @just --list

clippy: (_cmd 'cargo clippy --all-features --tests')

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

