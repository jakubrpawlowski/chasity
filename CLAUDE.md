# Style

- Prefer `>>` composition over `|>` pipes; use `|>` only when `>>` doesn't fit
- Prefer partial application over lambdas (`List.map (f x)` not
  `List.map (fun a -> f x a)`)
- Point-free with `>>` when the whole function is a composition chain
- No unnecessary `let x = ... in` when `x` is used once — pipe it
- `open Fun_ext` in modules that use `>>`
