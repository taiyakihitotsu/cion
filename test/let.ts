import type Cion from '../src/index.js'
import type { Equal } from '../src/util.js'

// let test
const test_let_0: true = {} as Equal<'3', Cion.Lisp<"(let [a {:a \"a\"}] (+ 1 2))">>

const test_letmap_0: true = {} as Equal<"['in' 'out' 'out']", Cion.Lisp<"(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= \"in\" (b a)))] (->> cv (map :status)))">>

const test_letmap_1a: true = {} as Equal<'true', Cion.Lisp<"(= 'in' (:status {:status 'in'}))">>

const test_letmap_1b: true = {} as Equal<"{:status 'in' :message 'message1'}", Cion.Lisp<"(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= 'in' (b a)))] (->> cv first))">>

const test_letmap_1c: true = {} as Equal<'true', Cion.Lisp<"(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] (= c (a b)))] (f :status {:status 'in' :message 'message1'} msg))">>

const test_letmap_1caaa: true = {} as Equal<'true', Cion.Lisp<"(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] (= c (b a)))] (f :status {:status 'in' :message 'message1'} msg))">>

const test_letmap_1caa: true = {} as Equal<"['in' 'in']", Cion.Lisp<"(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] [c (a b)])] (f :status {:status 'in' :message 'message1'} msg))">>

const test_letmap_1cab: true = {} as Equal<"[= 'in']", Cion.Lisp<"(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] [= (a b)])] (f :status {:status 'in' :message 'message1'} msg))">>

const test_letmap_1ca: true = {} as Equal<"['in' 'in']", Cion.Lisp<"[(:status {:status 'in' :message 'message1'}) 'in']">>

const test_letmap_1cb: true = {} as Equal<"['in' 'in']", Cion.Lisp<"[(:status {:status 'in' :message 'message1'}) ((fn [] 'in'))]">>

const test_letmap_1cc: true = {} as Equal<"['in' 'in']", Cion.Lisp<"(let [v 'in'] [(:status {:status 'in' :message 'message1'}) ((fn [] v))])">>

const test_letmap_1cd: true = {} as Equal<"['in' 'in']", Cion.Lisp<"(let [v 'in'] [(:status {:status 'in' :message 'message1'}) ((fn [] (first [v v])))])">>

const test_letmap_1d: true = {} as Equal<"['in']", Cion.Lisp<"(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] (= c (b a)))] [msg])">>

const test_letmap_1ce: true = {} as Equal<"['in' 'in']", Cion.Lisp<"(let [v 'in'] [(:status {:status 'in' :message 'message1'}) ((fn [] (:a {:a 'in'})))])">>


// form macro: if, let.
// --- Cion Lisp: Function (fn), Scoping, and Closures ---

// Basic fn binding and usage in let
const test_cion_fn_0: true = {} as Equal<'3', Cion.Lisp<"(let [x (fn [a b] (+ a b)) y 8] (+ 2 1))">>
const test_cion_fn_1: true = {} as Equal<'11', Cion.Lisp<"(let [x (fn [a b] (+ a b)) y 8] (+ y 2 1))">>

// Argument handling (ignoring extra arguments or scoping)
const test_cion_fn_2: true = {} as Equal<'2', Cion.Lisp<"(let [x (fn [a] (+ 1 a))] (x 1 1))">>
const test_cion_fn_3: true = {} as Equal<'2', Cion.Lisp<"(let [x (fn [a] (+ 1 a)) y 9] (x 1 1))">>
const test_cion_fn_4: true = {} as Equal<'10', Cion.Lisp<"(let [x (fn [a] (+ 1 a)) y 9] (x y 1))">>
const test_cion_fn_5: true = {} as Equal<'2', Cion.Lisp<"(let [x (fn [a b] (+ 1 a)) y 9] (x 1 1))">>
const test_cion_fn_6: true = {} as Equal<'3', Cion.Lisp<"(let [x (fn [a b] (+ b a)) y 9] (x 1 2))">>

// Immediately Invoked Function Expressions (IIFE)
const test_cion_iife_0: true = {} as Equal<'3', Cion.Lisp<"((fn [a b] (+ b a)) 1 2)">>
const test_cion_iife_1: true = {} as Equal<'4', Cion.Lisp<"(let [x ((fn [a] (+ a 1)) 1)] (* 2 x))">>
const test_cion_iife_2: true = {} as Equal<'20', Cion.Lisp<"(let [x ((fn [a b] (+ a 1 b)) 1 8)] (* 2 x))">>

// Closure and Lexical Scoping
const test_cion_scope_0: true = {} as Equal<'34', Cion.Lisp<"(let [x 8 y ((fn [a b] (+ a x b)) 1 8)] (* 2 y))">>
const test_cion_scope_1: true = {} as Equal<'34', Cion.Lisp<"(let [x (let [a 2 b 6] (+ a b)) y ((fn [a b] (+ a x b)) 1 8)] (* 2 y))">>

// Returning functions (Higher-order functions & Lexical capture)
const test_cion_closure_0: true = {} as Equal<"(fn [c] (+ 2 6 c))", Cion.Lisp<"(let [x (let [a 2 b 6] (fn [c] (+ a b c))) y 8] x)">>
const test_cion_closure_1: true = {} as Equal<'18', Cion.Lisp<"(let [x (let [a 2 b 6] (fn [c] (+ a b c)))] (x 10))">>

// Nested function calls and dependencies
const test_cion_nest_fn_0: true = {} as Equal<'10', Cion.Lisp<"(let [x (let [a 2 b 6] (fn [c] (+ a b c))) y ((fn [d] (x 2 d)) 10)] y)">>
const test_cion_nest_fn_1: true = {} as Equal<'12', Cion.Lisp<"(let [z 2 y ((fn [d] (+ z d)) 10)] y)">>
const test_cion_nest_fn_2: true = {} as Equal<'12', Cion.Lisp<"(let [z 2 y ((fn [d] (+ z d)) 10) x 10] y)">>
const test_cion_nest_fn_3: true = {} as Equal<'3', Cion.Lisp<"(let [z 2 y 3 x 10] y)">>

// Complex integration: Lexical capture with parameter passing
const test_cion_complex_0: true = {} as Equal<'10', Cion.Lisp<"(let [x (let [a 2 b 6] (fn [c] (+ a b c))) z 2 y ((fn [d] (x z d)) 10)] (x z))">>
const test_cion_complex_1: true = {} as Equal<'28', Cion.Lisp<"(let [x (let [a 2 b 6] (fn [c] (+ a b c))) z 20 y ((fn [d] (x d)) z)] y)">>
const test_cion_complex_2: true = {} as Equal<'28', Cion.Lisp<"(let [x (let [a 2 b 6] (fn [c] (+ a b c))) z 20 y ((fn [d] (x d)) z 999)] y)">>

// Shadowing
const test_shadowing_0: true = {} as Equal<'2', Cion.Lisp<"(let [x 1] (let [x 2] x))">>
const test_shadowing_1: true = {} as Equal<'10', Cion.Lisp<"(let [x 1 f (fn [x] x)] (f 10))">>
const test_shadowing_2: true = {} as Equal<'3', Cion.Lisp<"(let [x 1] (+ (let [x 2] x) x))">>

// Deeply let
const test_deep_closure_0: true = {} as Equal<'6', Cion.Lisp<"(let [a 1] (let [f (fn [b] (fn [c] (+ a b c)))] ((f 2) 3)))">>
const test_deep_closure_1: true = {} as Equal<'10', Cion.Lisp<"(let [a 1] (let [f (fn [b] (let [d 4] (fn [c] (+ a b c d))))] ((f 2) 3)))">>

// seq let
const test_sequential_let_0: true = {} as Equal<'6', Cion.Lisp<"(let [a 1 b (+ a 1) c (+ b 1)] (+ a b c))">>
const test_sequential_let_1: true = {} as Equal<'[1 2 4]', Cion.Lisp<"(let [a 1 b (* a 2) c (* b 2)] [a b c])">>

// zero args with let
const test_zero_args_0: true = {} as Equal<'10', Cion.Lisp<"(let [f (fn [] 10)] (f))">>
const test_zero_args_1: true = {} as Equal<'20', Cion.Lisp<"(let [a 10 f (fn [] (+ a a))] (f))">>
const test_zero_args_2: true = {} as Equal<'10', Cion.Lisp<"((fn [] ((fn [] 10))))">>

// map
const test_structure_resolve_0: true = {} as Equal<"{:x 1 :y 2}", Cion.Lisp<"(let [a 1 b 2] {:x a :y b})">>
const test_structure_resolve_1: true = {} as Equal<"[1 2 3]", Cion.Lisp<"(let [a 1] [a 2 3])">>
const test_structure_resolve_2: true = {} as Equal<"{:outer {:inner 10}}", Cion.Lisp<"(let [v 10] {:outer {:inner v}})">>

// app
const test_fn_application_complex_0: true = {} as Equal<'true', Cion.Lisp<"(let [is-even (fn [n] (= 0 (% n 2))) val 4] (is-even val))">>
const test_fn_application_complex_1: true = {} as Equal<'[2 4 6]', Cion.Lisp<"(let [double (fn [x] (* x 2)) items [1 2 3]] (map double items))">>
