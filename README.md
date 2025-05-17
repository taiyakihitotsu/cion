# Cion
Clojure Implemented on TypeScript, at the type level.

## How to Use
(If you want to do a type-check of this doc, see src/test-in-doc.ts)

### Basic
```typescript
const test_in_doc3: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= 'in' (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x)))) first :message))`> = "'message1'"
```

### Arithmetic
```typescript
const test_in_doc_a0: Cion.Lisp<`(+ 2 3)`> = '5'
const test_in_doc_a1: Cion.Lisp<`(/ 2 3)`> = '0'
const test_in_doc_a2: Cion.Lisp<`(/ 2 0)`> = 'nil'
const test_in_doc_a3: Cion.Lisp<`(+ 2 (- 1 4))`> = '-1'
```

- R\Z is not implemented in current.
- Division by zero is not allowed, to return nil.
- The value range is from -32767 to 32767. Decimal numbers is converted to 16bit number internally though, the minimum, -32768, is excluded for convenience. 

### Logical operation
```typescript
const test_in_doc_log0: Cion.Lisp<`(> 3 2 1)`> = 'true'
```

### if, let, fn
```typescript
const test_in_doc_if0: Cion.Lisp<`(if true 1 2)`> = '1'
const test_in_doc_let0: Cion.Lisp<`(let [a 2] (+ a 4))`> = '6'
const test_in_doc_fn0: Cion.Lisp<`((fn [x y] (+ x y)) 2 3)`> = '5'
```

- if form having only two components is not implemented.
- destructuring is not implemented.

### Loop
```typescript
const test_in_doc_loop0: Cion.Lisp<`(let [f (fn [r x] (if (>= 0 x) r (f (+ r 1) (- x 1))))] (f 1 3))`> = '4'
```

- Use recursion like above. loop and recur are not implemented.

### def
```def``` is not implemented, but ```typeof``` can be an alternative.

```typescript
const test_in_doc_def0: Cion.Lisp<`(+ ${typeof test_in_doc_loop0} 5)`> = '9'
```

## Builtins
 - if, fn, let, str, eq, and, or, not, >, <, =, >=, <=, +, -, *, /, mod, map, filter, remove, reduce, conj, concat, interleave, reverse, first, last, rest, butlast, assoc, assoc-in, update, update-in, get, vec, nil, ->, ->>.
 - vector & map syntax also be supported.

# Author
taiyakihitotsu

# License
3-clause BSD license
