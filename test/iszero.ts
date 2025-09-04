import type { Cion } from '../src/index'

const test_iszero0: Cion.Lisp<`(zero? 0)`> = 'true'
const test_iszero1: Cion.Lisp<`(zero? 1)`> = 'false'
const test_iszero2: Cion.Lisp<`(zero? [])`> = 'false'
const test_iszero3: Cion.Lisp<`(zero? nil)`> = 'false'
const test_iszero4: Cion.Lisp<`(-> 0 zero? not)`> = 'false'

const maintest_iszero_0: Cion.RawLisp<`(zero? 1)`> = ['prim', false]
const maintest_iszero_1: Cion.RawLisp<`(zero? -1)`> = ['prim', false]
const maintest_iszero_2: Cion.RawLisp<`(zero? 29438)`> = ['prim', false]
const maintest_iszero_3: Cion.RawLisp<`(zero? '')`> = ['prim', false]
const maintest_iszero_4: Cion.RawLisp<`(zero? '102')`> = ['prim', false]
const maintest_iszero_5: Cion.RawLisp<`(zero? false)`> = ['prim', false]
const maintest_iszero_6: Cion.RawLisp<`(zero? [])`> = ['prim', false]
const maintest_iszero_7: Cion.RawLisp<`(zero? [1 2 3])`> = ['prim', false]
const maintest_iszero_8: Cion.RawLisp<`(zero? (fn [x y] (+ x y)))`> = ['prim', false]
const maintest_iszero_9: Cion.RawLisp<`(zero? {:x 1 :y 1})`> = ['prim', false]
const maintest_iszero_10: Cion.RawLisp<`(zero? nil)`> = ['prim', false]
const maintest_iszero_11: Cion.RawLisp<`(zero? (+ 1 2))`> = ['prim', false]
const maintest_iszero_12: Cion.RawLisp<`(zero? ((fn [] (+ 1 2))))`> = ['prim', false]
const maintest_iszero_13: Cion.RawLisp<`(zero? ((fn [x y] (+ x y)) 1 2))`> = ['prim', false]
const maintest_iszero_14: Cion.RawLisp<`(zero? (:x {:x 1}))`> = ['prim', false]
const maintest_iszero_15: Cion.RawLisp<`(zero? (first [0 '1' 2]))`> = ['prim', true]
const maintest_iszero_16: Cion.RawLisp<`(zero? (let [x 1 y 2] (+ x y)))`> = ['prim', false]
const maintest_iszero_17: Cion.RawLisp<`(zero? (let [] (+ 1 2)))`> = ['prim', false]
const maintest_iszero_18: Cion.RawLisp<`(zero? (+ 3 (let [x 1 y 2] (+ x y))))`> = ['prim', false]

