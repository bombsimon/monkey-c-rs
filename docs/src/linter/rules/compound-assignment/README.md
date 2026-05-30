# `compound-assignment`

Flags `<lvalue> = <lvalue> <op> <expr>` patterns that can be written with a
compound assignment operator or `++` / `--`.

## Rationale

A self-referential assignment carries a small amount of duplication: the
target appears twice on the same line, and a reader has to confirm that the
left and right occurrences are the same identifier before they can read the
expression as "update `x`". The compound form removes that duplication and
makes intent immediately obvious.

## What triggers

The rule reports any assignment whose left- and right-hand sides reference
the same writable location:

```text
<target> = <target> <op> <expr>
```

`<target>` may be an identifier (`x`), a member access (`obj.x`), or an
index access (`arr[i]`) — and may nest, so `obj.a.b` and `grid[row][col]`
both qualify. The two occurrences must be structurally identical, and
`<op>` must have a compound form:

| Binary op | Compound | Special case for literal `1` |
| --------- | -------- | ---------------------------- |
| `+`       | `+=`     | `x++`                        |
| `-`       | `-=`     | `x--`                        |
| `*`       | `*=`     | —                            |
| `/`       | `/=`     | —                            |
| `%`       | `%=`     | —                            |
| `&`       | `&=`     | —                            |
| `\|`      | `\|=`    | —                            |
| `^`       | `^=`     | —                            |
| `<<`      | `<<=`    | —                            |
| `>>`      | `>>=`    | —                            |

## What does not trigger

The rule skips any case where the rewrite could change observable behavior
or where the two sides aren't actually the same location:

- Targets whose receiver or index isn't side-effect-free —
  `arr[next()] = arr[next()] + 1` would call `next()` once after the
  rewrite instead of twice, and `arr[i++] = arr[i++] + 1` similarly
  changes how many times `i` gets bumped.
- Commutative variants where the target appears on the right —
  `x = 1 + x` is semantically `x += 1`, but the binary's left operand
  isn't `x`, so the rule leaves it alone.
- Mismatched targets — `obj.x = obj2.x + 1` or `obj.x = obj.y + 1`.
- Already-compound assignments — `x += 1`, `x *= n`, etc.
- Operators without a compound form — `==`, `<`, `&&`, …

## Example

Before:

```monkey-c
function f() {
    x = x + 1;
    x = x - 1;
    x = x + 3;
    x = x * n;
    obj.x = obj.x + 1;
    arr[i] = arr[i] * 2;

    for (i = 0; i < 10; i = i + 1) {
        doStuff();
    }
}
```

After `--fix`:

```monkey-c
function f() {
    x++;
    x--;
    x += 3;
    x *= n;
    obj.x++;
    arr[i] *= 2;

    for (i = 0; i < 10; i++) {
        doStuff();
    }
}
```

## Fix

The fix replaces the entire assignment expression with the compound form.
The right-hand side is copied verbatim from the source, so any inline
comments and whitespace inside the RHS are preserved.
