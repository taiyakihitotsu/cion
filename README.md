# Cion
Clojure Implemented on TypeScript, at the type level.

## How to Use
Cion has a common grammer of [clojure](https://clojure.org/guides/learn/clojure). You can pick some of [builtins](#builtins), vector ```[]```, and map ```{}```.

**NOTE**
 - list, set is not supported. Use vector instead.
 - Line break is not supported.

(If you want to do a type-check of this doc, see [test/test-in-doc.ts](https://github.com/taiyakihitotsu/cion/blob/main/test/test-in-doc.ts))

### Basic
```clojure
const test_in_doc3: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= 'in' (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x)))) first :message))`> = "'message1'"
```

### Arithmetic
```clojure
const test_in_doc_a0: Cion.Lisp<`(+ 2 3)`> = '5'
const test_in_doc_a1: Cion.Lisp<`(/ 2 3)`> = '0'
const test_in_doc_a2: Cion.Lisp<`(/ 2 0)`> = 'nil'
const test_in_doc_a3: Cion.Lisp<`(+ 2 (- 1 4))`> = '-1'
```

- R\Z is not implemented in current.
- Division by zero is not allowed, to return nil.
- The value range is from -32767 to 32767. Decimal numbers is converted to 16bit number internally though, the minimum, -32768, is excluded for convenience. 

### Logical operation
```clojure
const test_in_doc_log0: Cion.Lisp<`(> 3 2 1)`> = 'true'
```

### get
```clojure
const test_in_doc_fst0: Cion.Lisp<`(first [1 2 3])`> = '1'
const test_in_doc_fst1: Cion.Lisp<`(first [])`> = 'nil'
const test_in_doc_get0: Cion.Lisp<`(get [1 2 3] 0)`> = '1'
const test_in_doc_get1: Cion.Lisp<`(get [1 2 3] 4)`> = 'nil'
```

 - Getting empty place is not allowe, to return nil.

### if, let, fn
```clojure
const test_in_doc_if0: Cion.Lisp<`(if true 1 2)`> = '1'
const test_in_doc_let0: Cion.Lisp<`(let [a 2] (+ a 4))`> = '6'
const test_in_doc_fn0: Cion.Lisp<`((fn [x y] (+ x y)) 2 3)`> = '5'
```

- if form having only two components is not implemented.
- destructuring is not implemented.
- empty body part is not supported.

### loop
```clojure
const test_in_doc_loop0: Cion.Lisp<`(let [f (fn [r x] (if (>= 0 x) r (f (+ r 1) (- x 1))))] (f 1 3))`> = '4'
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

## Builtins
 - if, fn, let, str, eq, and, or, not, >, <, =, >=, <=, +, -, *, /, mod, map, filter, remove, reduce, conj, concat, interleave, reverse, first, last, rest, butlast, assoc, assoc-in, update, update-in, get, vec, nil, ->, ->>.
 - number?, string?, vector?, map?, fn?, ifn?, nat-int?, pos-int?, neg-int?, odd?, even?, zero?, symbol?, keyword?, coll?, empty? 
 - vector & map syntax also be supported.

# Author
taiyakihitotsu

# License
3-clause BSD license
