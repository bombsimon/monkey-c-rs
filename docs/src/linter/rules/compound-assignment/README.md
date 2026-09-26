# `compound-assignment`

Flags an assignment like `x = x + n` that can use a compound operator like
`x += n`, or `x++` and `x--`.

## Why

In `x = x + 1` the reader has to check that both sides really are the same `x`
before they can read it as "increase `x`". `x++` says that directly.

## What it flags

An assignment where the right side starts with the same target as the left,
followed by an operator that has a compound form. The target can be a variable,
a member like `obj.x` or an index like `arr[i]`, and they can be nested.

| Written as      | Becomes     |
| --------------- | ----------- |
| `x = x + 1`     | `x++`       |
| `x = x - 1`     | `x--`       |
| `x = x + n`     | `x += n`    |
| `x = x - n`     | `x -= n`    |
| `x = x * n`     | `x *= n`    |
| `x = x / n`     | `x /= n`    |
| `x = x % n`     | `x %= n`    |
| `x = x & n`     | `x &= n`    |
| `x = x \| n`    | `x \|= n`   |
| `x = x ^ n`     | `x ^= n`    |
| `x = x << n`    | `x <<= n`   |
| `x = x >> n`    | `x >>= n`   |

## What it leaves alone

- Targets where rewriting changes how often something runs, like
  `arr[next()] = arr[next()] + 1`.
- The target on the right of the operator, like `x = 1 + x`.
- Different targets on each side, like `obj.x = obj.y + 1`.

## Example

```monkey-c
// Before
x = x + 1;
x = x * n;
obj.count = obj.count + 3;
arr[i] = arr[i] * 2;

for (i = 0; i < 10; i = i + 1) {
    process(i);
}

// After
x++;
x *= n;
obj.count += 3;
arr[i] *= 2;

for (i = 0; i < 10; i++) {
    process(i);
}
```
