import type Cion from './index'

const test_in_doc0: Cion.Lisp<`(let [a 'a' f (fn [c] (str '+' c '+'))] (if (eq '+a+' (f a)) 'this_is_true' 'this_is_false'))`> = "'this_is_true'"

const test_in_doc1: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= "in" (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x))))))`> = `[{:status 'in' :message 'message1'}]`

const test_in_doc2: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= 'in' (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x)))) first))`> = `{:status 'in' :message 'message1'}`

const test_in_doc3: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= 'in' (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x)))) first :message))`> = "'message1'"

// Cion has a common grammer of Clojure.
// > https://clojure.org/guides/learn/syntax
// Some of difference is described below:

// - List is not implemented. Use vector instead.
// - Quote, Eval, etc are not implemented.
// - Use apostrophe as a string syntax.

// See Built in fns.

// --- arithmetic
const test_in_doc_a0: Cion.Lisp<`(+ 2 3)`> = '5'
const test_in_doc_a1: Cion.Lisp<`(/ 2 3)`> = '0'
const test_in_doc_a2: Cion.Lisp<`(/ 2 0)`> = 'nil'
const test_in_doc_a3: Cion.Lisp<`(+ 2 (- 1 4))`> = '-1'
// - R\Z is not implemented in current.
// - Division by zero is not allowed, to return nil.
// - The value range is from -32767 to 32767. Decimal numbers is converted to 16bit number internally though, the minimum, -32768, is excluded for convenience. 

// --- logical operation
const test_in_doc_log0: Cion.Lisp<`(> 3 2 1)`> = 'true'

// --- get
const test_in_doc_fst0: Cion.Lisp<`(first [1 2 3])`> = '1'
const test_in_doc_fst1: Cion.Lisp<`(first [])`> = 'nil'
const test_in_doc_get0: Cion.Lisp<`(get [1 2 3] 0)`> = '1'
const test_in_doc_get1: Cion.Lisp<`(get [1 2 3] 4)`> = 'nil'
// - Getting empty place is not allowe, to return nil.

// --- if form
const test_in_doc_if0: Cion.Lisp<`(if true 1 2)`> = '1'
// - if form having only two components is not implemented.

// --- let form
const test_in_doc_let0: Cion.Lisp<`(let [a 2] (+ a 4))`> = '6'
// - destructuring is not implemented.
// - empty body part is not supported.

// --- fn form
const test_in_doc_fn0: Cion.Lisp<`((fn [x y] (+ x y)) 2 3)`> = '5'
// - destructuring is not implemented.
// - empty body part is not supported.

// --- loop
const test_in_doc_loop0: Cion.Lisp<`(let [f (fn [r x] (if (>= 0 x) r (f (+ r 1) (- x 1))))] (f 1 3))`> = '4'
// - Use recursion like above. loop and recur are not implemented.

// -- def
const test_in_doc_def0: Cion.Lisp<`(+ ${typeof test_in_doc_loop0} 5)`> = '9'
const test_in_doc_def1: Cion.Lisp<`(fn [i] (+ 1 i))`> = '(fn [i] (+ 1 i))'
const test_in_doc_def2: Cion.Lisp<`(${typeof test_in_doc_def1} ${typeof test_in_doc_def0})`> = '10'
