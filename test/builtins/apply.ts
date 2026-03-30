import type Cion from '../../src/index.ts'
import type { Equal } from '../../src/util'

const apply_test_0: true = {} as Equal<'1', Cion.Lisp<`(apply inc [0])`>>
const apply_test_1: true = {} as Equal<'3', Cion.Lisp<`(apply + [0 1 2])`>>
const apply_test_2: true = {} as Equal<'1', Cion.Lisp<`(apply max [-1 0 1])`>>

const apply_test_3: true = {} as Equal<'1', Cion.Lisp<`(apply (fn [n] (inc n)) [0])`>>
const apply_test_4: true = {} as Equal<'3', Cion.Lisp<`(apply (fn [x y z] (+ x y z)) [0 1 2])`>>
const apply_test_5: true = {} as Equal<'1', Cion.Lisp<`(apply (fn [x y z] (max x y z)) [-1 0 1])`>>
