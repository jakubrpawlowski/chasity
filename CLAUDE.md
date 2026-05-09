# Style

- Prefer `>>` composition over `|>` pipes; use `|>` only when `>>` doesn't fit
- Prefer partial application over lambdas (`List.map (f x)` not
  `List.map (fun a -> f x a)`)
- Point-free with `>>` when the whole function is a composition chain
- No unnecessary `let x = ... in` when `x` is used once — pipe it
- Pipe data into `List.map`/`List.filter_map`/`List.fold_left`/etc — never pass
  as last positional arg (`data |> List.map f` not `List.map f data`)
- Consistent names for the same concept across a module
- `open Fun_ext` in modules that use `>>`
