# monkey-c-coverage

Test coverage for Monkey C, a platform with no native coverage support.
Instead of compiler instrumentation, the source is rewritten before
compilation:

1. `instrument` parses each file with `monkey-c-parser` and splices a
   `Cov.hit(N);` call directly after the opening brace of every function
   body, writing the rewritten copies, a generated `Cov.mc` runtime, and a
   `coverage-manifest.tsv` (id, line, file, function) into an output
   directory. Untouched code stays byte-identical — the same byte-range
   patching strategy `monkey-c-linter --fix` uses.
2. Compile the instrumented directory with `monkeyc --unit-test` and run the
   tests with `monkeydo … -t`, capturing the console output. The `Cov.mc`
   runtime prints `COVHIT <id>` the first time each function executes.
3. `report` joins the captured log against the manifest and prints per-file
   function coverage with the names of every function that never ran.

```sh
monkey-c-coverage instrument --out build/coverage/source source/*.mc
monkeyc -f build/coverage/coverage.jungle -d <device> -o cov.prg -y key --unit-test
monkeydo cov.prg <device> -t | tee run.log
monkey-c-coverage report \
    --manifest build/coverage/source/coverage-manifest.tsv \
    --log run.log \
    --exclude-suffix Test.mc
```

The jungle for the instrumented build points `base.sourcePath` at the
output directory (paths are resolved relative to the jungle file):

```
project.manifest = ../../manifest.xml
base.sourcePath = source
base.resourcePath = ../../resources
```

Granularity is function-level: it answers "which functions never execute
under the test suite", including functions reached indirectly, which static
reference scanning cannot see. Statement and branch coverage would follow
the same design by also splicing at `BlockStmt` boundaries.

The simulator may reinitialize module state between unit tests, so the
first-hit deduplication in `Cov.mc` restarts per test and ids can repeat in
the log; `report` deduplicates while joining.
