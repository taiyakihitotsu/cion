import type Cion from '../../src/index.ts'
import type { Equal } from '../../src/util'

const repeat_test_0 : true = {} as Equal<'[]', Cion.Lisp<`(repeat 0 'x')`>>

const repeat_test_1 : true = {} as Equal<"['x' 'x']", Cion.Lisp<`(repeat 2 'x')`>>

const repeat_test_2 : true = {} as Equal<'[]', Cion.Lisp<`(repeat -1 'x')`>>

const repeat_test_3 : true = {} as Equal<
  '[(fn [n] (inc n)) (fn [n] (inc n)) (fn [n] (inc n))]',
  Cion.Lisp<`(repeat 3 (fn [n] (inc n)))`>>
