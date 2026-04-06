# 🧪 Project Status: Beta
Cion is currently in Beta and undergoing a major internal refactoring 🛠️.  
Expect frequent updates and some "experimental" internal code!

# Cion
[![npm](https://img.shields.io/npm/v/@taiyakihitotsu/cion)](https://www.npmjs.com/package/@taiyakihitotsu/cion) ![license](https://img.shields.io/npm/l/@taiyakihitotsu/cion) [![build](https://github.com/taiyakihitotsu/cion/actions/workflows/node.js.yml/badge.svg)](https://github.com/taiyakihitotsu/cion/actions)

Cion is a full-featured, **type-level Lisp interpreter** inspired by Clojure.  
Cion lets you execute Lisp code at compile time using TypeScript's type system.

```typescript
type Result = Cion.Lisp<`(+ 1 2)`> // "3"
```

Try it:  
[playground]( https://www.typescriptlang.org/play/?#code/JYWwDg9gTgLgBDAnmApnAwsCA7OAzKCEOAcgAEYBDYRSga2AAtgYIYBnAVwHoBjLbCQBQQ7gCoxcAMqgwAGzSUoLRiBQxgvOGO5DeOdvCmcQAfQCsALgwCAdABlg7MAB4SACgDUcAExwAzACUJAB8cAC8pObC3NxwFOwAtCgAHqi8MMlQhFB6BkYmFqYo2dDWmDgOTq4e3n5BoRGkAGzCohJwACLAAG5OAnDaukioXb2mfpEV2FXObu5xABxwACzBIaJxgwB6APxwAEQ+B0IjaN09poum-k3TszULcACMzXDN65uDcHuHi9z+E5nMaXbDAOR3OyOOYeOJ+AAMn1i31+BzBchO7UkADEQJQwENTsg0PYILxKHJseCYCUmh48NTae48LgANopAC6cHcYRSvkCAuEwNJ5LkAFl8XT3HiCcy2ZzuZI+T4BcE4ETRgA1FC8UzNK6QyrQ1wAA3ciRCYVZz18AVWXIAJABvEUUqlyGlQAC+cGdrvF+K9gRNG2RO32B1Zb0WHMx4kkpIgBIA7ip8LhvAp4DoNSSyW7cJF6WyoHAFe5gHhuSFIvCy4E4KXmdzvKXng3zWWXqrgrm4ImwKYVoaZsaXGas3BWVW-fnKdgvVzmzagsHQ1sfhGVidkQlkmkdZkSjk8thDP2IEmh8VSlBrAOh3T-G143AAEooADmqUJwI-35SUxKAAIy0KYoWqcd3CgFBEgZbAABNSFZShEgALw5TwSFIEDeGeHxnzXL5w0OEhcJIHc4j3VJ0iPW9T3Pf9UiA0CbxyawmMA3C6WEIA ) | [source]( https://github.com/taiyakihitotsu/cion/blob/main/test/playground.ts )
## Install
**pnpm** - `pnpm add -D @taiyakihitotsu/cion`  
**npm** - `npm install -D @taiyakihitotsu/cion`  
## Key Features
- **Zero Runtime Overhead**: Computations are performed at compile-time and vanish in production.
- **Turing-Complete**: Supports recursion, lexical scoping (`let`), and higher-order functions (`fn`), looping via `let` + `fn` or `reduce`.
- **Compile-time Regex**: Features a built-in type-level regex engine (`re-find`).
- **Standard Library**: 90+ built-in type-level functions including `map`, `filter`, `reduce`, and thread macros (`->`, `->>`).
## For Developers
If you want to understand the internal implementation, see [DEV.md]( https://github.com/taiyakihitotsu/cion/blob/main/DEV.md ).
## How to Use
```typescript
import type Cion from '@taiyakihitotsu/cion'

const test: Cion.Lisp<`(inc 3)`> = '4'
```

## Overview
Cion follows [clojure](https://clojure.org/guides/learn/clojure)'s standard syntax. You can use [builtins](#builtins), vector ```[]```, and map ```{}```.

**Note**
 - Lists `()` and sets `#{}` are not supported. Use vector instead.

(If you want to do a type-check of this doc, see [test/test-in-doc.ts](https://github.com/taiyakihitotsu/cion/blob/main/test/test-in-doc.ts))

### Basic
```typescript
const test_in_doc3: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= 'in' (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x)))) first :message))`> = "'message1'"
```

### Arithmetic
```typescript
const test_in_doc_a0: Cion.Lisp<`(+ 2 3)`> = '5'
const test_in_doc_a1: Cion.Lisp<`(/ 2 3)`> = '2/3'
const test_in_doc_a2: Cion.Lisp<`(/ 2 0)`> = 'nil'
// @ts-expect-error: Type '"0"' is not assignable to type '"nil"'.
const test_in_doc_a2_err: Cion.Lisp<`(/ 2 0)`> = '0'
const test_in_doc_a3: Cion.Lisp<`(+ 2 (- 1 4))`> = '-1'
// @ts-expect-error: Type '"0"' is not assignable to type '"-1"'.
const test_in_doc_a3_err: Cion.Lisp<`(+ 2 (- 1 4))`> = '0'
```

- Division by zero returns `nil`.
- The value range is from `-32767` to `32767`. Numbers are internally handled as 16-bit signed integers. The minimum, `-32768`, is excluded currently. 

### Logical operation
```typescript
const test_in_doc_log0: Cion.Lisp<`(> 3 2 1)`> = 'true'
// @ts-expect-error: Type '"true"' is not assignable to type '"false"'.
const test_in_doc_log0_err: Cion.Lisp<`(> 3 3 1)`> = 'true'
```

### get
```typescript
const test_in_doc_fst0: Cion.Lisp<`(first [1 2 3])`> = '1'
const test_in_doc_fst1: Cion.Lisp<`(first [])`> = 'nil'
// @ts-expect-error: Type '"[]"' is not assignable to type '"nil"'.
const test_in_doc_fst1_err: Cion.Lisp<`(first [])`> = '[]'
const test_in_doc_get0: Cion.Lisp<`(get [1 2 3] 0)`> = '1'
const test_in_doc_get1: Cion.Lisp<`(get [1 2 3] 4)`> = 'nil'
// @ts-expect-error: Type '"[]"' is not assignable to type '"nil"'.
const test_in_doc_get1_err: Cion.Lisp<`(get [1 2 3] 4)`> = '[]'
```

 - Accessing an out-of-bounds index returns `nil`.

### if, let, fn
```typescript
const test_in_doc_if0: Cion.Lisp<`(if true 1 2)`> = '1'
// @ts-expect-error: Type '"2"' is not assignable to type '"1"'.
const test_in_doc_if0_err: Cion.Lisp<`(if true 1 2)`> = '2'

const test_in_doc_let0: Cion.Lisp<`(let [a 2] (+ a 4))`> = '6'
// @ts-expect-error: Type '"5"' is not assignable to type '"6"'.
const test_in_doc_let0_err: Cion.Lisp<`(let [a 2] (+ a 4))`> = '5'

const test_in_doc_fn0: Cion.Lisp<`((fn [x y] (+ x y)) 2 3)`> = '5'
// @ts-expect-error: Type '"6"' is not assignable to type '"5"'.
const test_in_doc_fn0_err: Cion.Lisp<`((fn [x y] (+ x y)) 2 3)`> = '6'
```

- Two-component `if` forms (`(if true true-branch)`) are not implemented.
- Destructuring is not implemented.
- Empty function bodies are not supported.

### loop
```typescript
type Loop_Return_4 = `(let [f (fn [r x] (if (>= 0 x) r (f (+ r 1) (- x 1))))] (f 1 3))`
const test_in_doc_loop0: Cion.Lisp<Loop_Return_4> = '4'
// @ts-expect-error: Type '"3"' is not assignable to type '"4"'.
const test_in_doc_loop0_err: Cion.Lisp<Loop_Return_4> = '3'
```

- Use recursion like above. loop and recur are not implemented.

### def, defn
```def``` and ```defn``` is not implemented, but ```type``` ```typeof``` can be an alternative.

```typescript
const test_in_doc_def0: Cion.Lisp<`(+ ${typeof test_in_doc_loop0} 5)`> = '9'
const test_in_doc_def1: Cion.Lisp<`(fn [i] (+ 1 i))`> = '(fn [i] (+ 1 i))'
const test_in_doc_def2: Cion.Lisp<`(${typeof test_in_doc_def1} ${typeof test_in_doc_def0})`> = '10'

type test_in_doc_def3 = `(+ 2 2)`
type test_in_doc_def4 = Cion.Lisp<`(+ ${test_in_doc_def3} 5)`>
type test_in_doc_def5 = '(fn [i] (+ 1 i))'
type test_in_doc_def6 = Cion.Lisp<`(${test_in_doc_def5} ${test_in_doc_def4})`>
const test_in_doc_def7: test_in_doc_def6 = '10'
```

### re-find
```typescript
const test_refind1 : Cion.Lisp<`(re-find '[a-z]*' 'aaa')`> = `'aaa'`
type email = `'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'`
const test_refind : Cion.Lisp<`(re-find ${email} 'zzz.zzz@testmailreg.com')`> = `'zzz.zzz@testmailreg.com'`
```
- `#` of `#'regex'` is unnecessary.


## Builtins
The specs of some fns **don't** follow Clojure.  
See [spec/spec.ts](https://github.com/taiyakihitotsu/cion/blob/main/spec/spec.ts).

### Core
- if, fn, let, nil
### Arithmetic
- +, -, *, /, mod, rem, `*`trunc, floor, inc, dec, abs, min, max
### String & Regex
- str, re-find, split, subs, `*`subs-all, replace, join
### Logic
- and, or, not, >, <, = (eq), >=, <=
### Predicates
- `*`prim?, any?, number?, string?, vector?, map?, fn?, ifn?, int?, `*`nat? (= nat-int?), ratio?, pos?, neg?, pos-int?, neg-int?, odd?, even?, zero?, keyword?, empty?, boolean?, type, every?, some, nil?, some?
### Collections
- map, filter, remove, reduce, zipmap, apply
- conj, concat, interleave, reverse, range, repeat, drop, take, keys, count
### Map operations
- assoc, assoc-in, update, update-in
### Accessors
- first, second, `*`third, last, rest, butlast, get, get-in
### Threading macros
- ->, ->>, some->, some->>

And vector ```[0 1 2]``` & map ```{:x 0 :y 1 :z 2}``` syntax supported.

`*` indicates functions not available in Clojure.

# Author
taiyakihitotsu

# License
BSD 3-Clause License
