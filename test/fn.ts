import type Cion from '../src/index.js'
import type { Equal } from '../src/util.js'

const test_fn_args_0: true = {} as Equal<'1', Cion.Lisp<"((fn [a] a) 1)">>
const test_fn_args_1: true = {} as Equal<'2', Cion.Lisp<"((fn [a b] b) 1 2)">>
const test_fn_args_2: true = {} as Equal<'3', Cion.Lisp<"((fn [a b c] (+ a b c)) 1 1 1)">>

const test_fn_scope_0: true = {} as Equal<'11', Cion.Lisp<"(let [x 10] ((fn [a] (+ a x)) 1))">>
const test_fn_scope_1: true = {} as Equal<'15', Cion.Lisp<"(let [x 5 f (fn [a] (+ a x))] (let [x 10] (f 10)))">>
const test_fn_scope_2: true = {} as Equal<'20', Cion.Lisp<"(let [f (fn [x] (fn [y] (+ x y)))] ((f 10) 10))">>

const test_fn_higher_order_0: true = {} as Equal<'2', Cion.Lisp<"(let [apply (fn [f x] (f x))] (apply (fn [a] (+ a 1)) 1))">>
const test_fn_higher_order_1: true = {} as Equal<'10', Cion.Lisp<"(let [const-fn (fn [x] (fn [] x)) get-ten (const-fn 10)] (get-ten))">>
const test_fn_higher_order_2: true = {} as Equal<'4', Cion.Lisp<"(let [twice (fn [f x] (f (f x))) inc (fn [n] (+ n 1))] (twice inc 2))">>

const test_fn_immediate_0: true = {} as Equal<'100', Cion.Lisp<"((fn [x] (* x x)) 10)">>
const test_fn_immediate_1: true = {} as Equal<'3', Cion.Lisp<"((fn [a] ((fn [b] (+ a b)) 2)) 1)">>

type Actual_data_structure_0 = Cion.Lisp<"((fn [] {:res (+ 10 20)}))">
const test_fn_data_structure_0: true = {} as Equal<"{:res 30}", Actual_data_structure_0>

type Actual_data_structure_1 = Cion.Lisp<"((fn [a] {:res (+ a 20)}) 10)">
const test_fn_data_structure_1: true = {} as Equal<"{:res 30}", Actual_data_structure_1>

type Actual_data_structure_2 = Cion.Lisp<"((fn [a b] {:res (+ a b)}) 10 20)">
const test_fn_data_structure_2: true = {} as Equal<"{:res 30}", Actual_data_structure_2>

// @ts-expect-error:
const test_fn_multi_body_0: true = {} as Equal<'2', Cion.Lisp<"((fn [a] (+ a 10) (+ a 1)) 1)">>

const test_fn_nest_let_0: true = {} as Equal<'3', Cion.Lisp<"((fn [x] (let [y 2] (+ x y))) 1)">>
const test_fn_nest_let_1: true = {} as Equal<'6', Cion.Lisp<"(let [x 1] ((fn [y] (let [z 3] (+ x y z))) 2))">>
