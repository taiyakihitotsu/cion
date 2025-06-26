## setup
```terminal
sh scripts/init.sh
```

## format
```terminal
npm run format:smith
```

## commit
Your commit message **must include one of the following tags**: `[add]` `[update]` `[fix]` `[refactor]` `[chore]` `[doc]` `[test]`.

`husky` hooks will run automatically on commit.

## test
- `npm run check`: to type check the `src` dir.
- `npm run check:test`: to test the `test` dir.
- `npm run check:smith`: to check if the `src` dir is formatted.

> Important: Use `Cion.Lisp` to test record type. Because union types don't preserve record order, the same meaning S-expressions may appear in different orders.  To keep a consistent order, you need to use map or similar techniques in `Cion.Lisp`.

## debug
Evaluation error must extend `{error: string, sexpr: unknown[]}`. They will eventually be thrown as an `UnparseError` during AST conversion back to S-exprs strings. If you want to dump evaluation state for debugging, wrap the value like this:
```typescript
{error: '', sexpr: THEvalue}
```

Since sexpr is an array, you can also dump the environment `env` here.

## FAQ & more
Feel free to open issues casually. Issues with the `[bug]` tag will be prioritized.



# Specification
## Error Case
To simplify error handling, some functions return `nil` instead of throwing exceptions, which differs from standard Clojure behavior.

Functions that do this include:

- getter fns like `first`
- `div` (only when div by zero).

I've leave that `update` / `update-in` throw errors directly instead of returning `nil` or themselves.  Their errors occur in very different ways, so throwing makes fixing the S-expression easier.

(They may also be updated in the future to return `nil` instead of errors.)

## Numeric
- Supports rational numbers (Q). Real numbers (R) are not supported.
- Rational numbers with negative denominators are represented as `-n/m` if one of `n` or `m` is negative.
- `0` is represented of `0/1`.
- Any number with a denominator of `0` returns nil during casting and reduction.

AST: 
```typescript
['prim', string]` | `['prim', [string, string]]
```

## string
All string expressions are wrapped in double quotes `("")`. To get the raw string content, remove the first and last double quotes.
To concatenate two strings, concatenate their contents first, then wrap the result again in `""`.

AST: 
```typescript
['prim', string]
```

## keyword
Keywords are expressed with a leading colon like `:key`. When used in TypeScript, remove the leading colon.

AST: 
```typescript
['key', string]
```

## `fn` and `let` AST
`fn` expressions are internally translated into nested `let` expressions. When there are multiple arguments, the `let` expressions are nested accordingly. So if `fn` part has over one args, firstly it is converted to multiarg `let` AST, then the form is separated into the one arg `let` AST.

Note that the Type `Fn` does not match builtin-functions. To accept both, use the union type `Fn | ['sym', BuiltinsFn].`

## regex
In Cion Lisp, the `#` prefix for regex (e.g., `#''`) is not required. Within character classes `([ or [^)`, unescaped hyphens `(-)` are read literally unless written as ranges like `a-z`. 

The escape character is `\\` (double backslash), not `\`, following TypeScript rules.
