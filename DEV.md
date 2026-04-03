This document is intended for:
- people who want to understand the internal implementation
- potential contributors

It assumes familiarity with TypeScript advanced types and basic Lisp concepts.

# Development
## Workflow
### Setup
1. **Initialize**: `sh init.sh`
2. **Format**: `pnpm format:smith`
3. **Lint & Type Check**: 
  - `pnpm check`: Validates the `src` directory.
  - `pnpm check:test`: Validates the `test` directory.
  - `pnpm check:smith`: Verifies formatting compliance.
### Commit Convention
Commit messages must be prefixed with one of the following tags:  
`[add]` | `[update]` | `[fix]` | `[refactor]` | `[chore]` | `[doc]` | `[test]`
### Testing Standards
**CRITICAL**: When testing map object types, do not compare raw union types directly.
Since TypeScript's union types do not guarantee property order, S-expressions representing the same map may appear in different orders, causing flaky tests.

**Solution**: Use `Cion.Lisp` to evaluate and compare values. Map-like structures should be validated through `Cion.Lisp`'s internal canonicalization to ensure consistent ordering.
## Debugging & Error Handling
### Error Structure
All evaluation errors must extend:  
`{error: string, sexpr: unknown[]}`

These are eventually re-thrown as `UnparseError` during AST-to-String conversion.  
### State Dumping
`Eval` is defined in `src/index.ts` ([here]( https://github.com/taiyakihitotsu/cion/blob/main/src/index.ts#L2169 )).  
To inspect the internal state or environment (`env`) during evaluation, wrap the target value, at a branch in `Eval` which you want to watch:
```typescript
// Debug dump wrapper
{error: '', sexpr: The_Value_To_Dump}
```

Since `sexpr` is an array, it is a safe place to dump the current `env` for deep inspection.
## Handling Recursion Limits (TS2589)
```terminal
TS2589: Type instantiation is excessively deep and possibly infinite.
```
`Cion` heavily uses type-function and recurtion. You can read these eregant articles:

- [How to workaround the max recursion depth in TypeScript]( https://www.esveo.com/en/blog/how-to-workaround-the-max-recursion-depth-in-typescript/ )
- [Into the Chamber of Secrets, Break through the limits of TypeScript]( https://herringtondarkholme.github.io/2023/04/30/typescript-magic/ )

If you struggle with TS2589 error, check `Rec` defined in [src/util.ts]( https://github.com/taiyakihitotsu/cion/blob/main/src/util.ts ).  

- Rewrite a type function you want to fix TS2589, as returning `{r: Result}` instead of just `Result` type in **every branch**.
- Decompose the result nested by `:r` via `Rec`.

This pattern has been already used in [`SParser`]( https://github.com/taiyakihitotsu/cion/blob/doc/src/compiler.ts ) and [`regex-compiler`]( https://github.com/taiyakihitotsu/cion/blob/doc/src/regex-compiler.ts ). Check them for details and a nuance.
## Core Design Principles
### Architecture Overview
string-literal -- via [`SParser`]( https://github.com/taiyakihitotsu/cion/blob/main/src/compiler.ts ) --> tokens -- via [`SCompiler`]( https://github.com/taiyakihitotsu/cion/blob/main/src/compiler.ts ) --> AST -- via [`Eval`]( https://github.com/taiyakihitotsu/cion/blob/main/src/index.ts ) --> AST -- via [`Unparser`]( https://github.com/taiyakihitotsu/cion/blob/main/src/compiler.ts ) --> string-literal
### Evaluation
- **Cion.Lisp**: Always returns a string literal that is valid as an input to another `Cion.Lisp` call.
- **Cion.RawLisp**: Always returns an AST tuple that is valid as an input to another `Cion.RawLisp` call.

Except in error cases for both patterns, because error handler are map objects.

We can assign results of evaluation to variable `A` to re-use the next arguments.

```typescript
type A = Cion.Lisp<...>
type B = Cion.Lisp<`${A}`>
```

**NOTE**: The return type of `Cion.Lisp` is always a literal string (as above). In other words, results cannot be used directly for type-checking against TypeScript's primitive `number` or `boolean` types.

# Specification
## AST
`string` here means internal-16-bit-string, e.g. `"0000000000001010"`.
Rational numbers are composed by 2 bit-string element tuples. 
### Values
| Type | Lisp Syntax | AST Representation | Notes |
| :--- | :--- | :--- | :--- |
| **Number** | `42`, `2/3` | `['prim', string]`, `['prim', [string, string]]` | Rational (Q) only. Denominator 0 returns `nil`. |
| **String** | `'hello'` | `['prim', string]` | Wrapped in quotes `''`. |
| **Keyword** | `:key` | `['key', string]` | Leading colon is removed in the AST string. |
| **Vector** | `[1 2]`, `[]` | `['vec', ['prim', string], ['prim', string]]`, `['vec']` | No need to separete with `,`. |
| **Map** | `{:a 1 :b 2}` | `['map', [['key', 'a'], ['prim', string]], [['key', 'b'], ['prim', string]]]` | No need to separate with `,`. |
| **Fn** | `(fn [a] (inc a))` | `['fn', [['sym', 'a']], [['sym', 'inc'], ['sym', 'a']]]` | Body part required. |
| **`nil`** | `nil` | `['prim', 'nil']` | primitive. |
| **built-in** | `inc` | `['sym', 'inc']` | See also Apply Fn sector. |

They are the first classes for `Cion`.  
### Syntax Macro
| Type | Lisp Syntax | AST Representation | Notes |
| :--- | :--- | :--- | :--- |
| **`if`** | `(if true 0 1)` | `['if', ['prim', true], ['prim', string], ['prim', string]]` | Else branch required. |
| **`let`** | `(let [a 0] (inc a))` | `['let', [['sym', 'a'], ['prim', string]], [['sym', 'inc'], ['sym', 'a']]]` | Body part required. |

As the same as TypeScript's type-system, `Cion` S-expressions return S-expression, since type function must not have side-effect.

Internal arguments AST of `let` and `fn` are the common structure. Both are acceptable to be an empty vector.

`fn` expressions are internally translated into nested `let` expressions.  
When there are multiple arguments, the `let` expressions are nested accordingly.  
So if `fn` part has over one args, firstly it is converted to multiarg `let` AST, then the form is separated into the one arg `let` AST.

Note that the Type `Fn` does not match builtin-functions. To accept both, use the union type `Fn | ['sym', BuiltinsFn].`

**Note for TS users**: `if` and `let` syntax also return a value.  
**Note for Clojure users**: Threading macros (`->`, `->>`, `some->`, `some->>`) are implemented as fns in Cion, both are macros in Clojure though.
### Function Application
| Type | Lisp Syntax | AST Representation | Notes |
| :--- | :--- | :--- | :--- |
| **Apply** | `(inc 0)` | `[['sym', 'inc'], ['prim', string]]` | Possible to empty call `(f)`. |

Fn is a first class, so you can write `((f) 0)` if `f` returns unary fn `g`.

## Error Case
To simplify error handling, some functions return `nil` instead of throwing exceptions, which differs from standard Clojure behavior.

Fns that do this include:

- Getter fns like `first`
- `div` (only when div by zero).

I've leave that `update` / `update-in` throw errors directly instead of returning `nil` or themselves.  Their errors occur in very different ways, so throwing makes fixing the S-expression easier.

(They may also be updated in the future to return `nil` instead of errors.)

## Primitive Spec
### number
- Supports rational numbers (Q). Real numbers (R) are not supported.
- Rational numbers with negative denominators are represented as `-n/m` if one of `n` or `m` is negative.
- `0` is represented of `0/1`.
- Any number with a denominator of `0` returns `nil` during casting and reduction.

All numerical calculations are performed using bit-strings. Integer number will be translated to rational number first if calculating with rational vs integer.

[bit.ts]( https://github.com/taiyakihitotsu/cion/blob/main/src/bit.ts ) | [decimal.ts]( https://github.com/taiyakihitotsu/cion/blob/doc/src/bit.ts ) | [peano.ts]( https://github.com/taiyakihitotsu/cion/blob/main/src/bit.ts ) | [ratio.ts]( https://github.com/taiyakihitotsu/cion/blob/main/src/bit.ts )

### string
All string expressions are wrapped in quotes `('')`.  
To get the raw string content, remove the first and last quotes.    
To concatenate two strings, concatenate their contents first, then wrap the result again in `""`.

### regex
Cion's regex engine is **non-backtracking**.

- Once a pattern is evaluated, it does not revisit previous states
- This guarantees deterministic behavior and avoids exponential runtime
- Some patterns that rely on backtracking (as in JavaScript) may not behave the same

This is a deliberate design decision and should not be changed.

In Cion Lisp, the `#` prefix for regex (e.g., `#''`) is not required. Within character classes `([ or [^)`, unescaped hyphens `(-)` are read literally unless written as ranges like `a-z`. 

The escape character is `\\` (double backslash), not `\`, following TypeScript literal rules.

Group-match is implemented but Group-catch isn't.

[regex.ts]( https://github.com/taiyakihitotsu/cion/blob/main/src/regex.ts ) | [regex-eval.ts]( https://github.com/taiyakihitotsu/cion/blob/main/src/regex-eval.ts ) | [regex-compiler.ts]( https://github.com/taiyakihitotsu/cion/blob/main/src/regex-compiler.ts ) | [regex-const.ts]( https://github.com/taiyakihitotsu/cion/blob/main/src/regex-const.ts ) | [strutil.ts]( https://github.com/taiyakihitotsu/cion/blob/main/src/strutil.ts )


# FAQ & more
Feel free to open issues casually. Issues with the `[bug]` tag will be prioritized.
