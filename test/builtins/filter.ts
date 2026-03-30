import type Cion from '../../src/index'
import type { Filter } from '../../src/index'
import type { Equal } from '../../src/util'

const filter_test_0 : true = {} as Equal<[`vec`, [`prim`, 1], [`prim`, 1]], 
Filter<
    [`fn`, [[`sym`, `a`]], [[`sym`, `eq`], [`sym`, `a`], [`prim`, 1]]],
    [`vec`, [`prim`, 0], [`prim`, 1], [`prim`, 1], [`prim`, 2]]
  >>

const filter_test_1 : true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']], 
Cion.RawLisp<'(filter (fn [n] (> 3 n)) [0 1 2 3 4 5])'>>

const filter_test_2 : true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']], 
Cion.RawLisp<'(let [f (fn [n] (> 3 n))] (filter f [0 1 2 3 4 5]))'>>

const filter_test_3 : true = {} as Equal<['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']], 
Cion.RawLisp<'(filter number? [0 1 2 3 4 5])'>>

// String Expression Tests
const filter_test_4 : true = {} as Equal<'[]', Cion.Lisp<`(filter number? [])`>>
const filter_test_5 : true = {} as Equal<`[0 1 2]`, Cion.Lisp<`(filter number? ['s' 0 1 false 2])`>>

const filter_test_6 : true = {} as Equal<'[0 1 2]', 
Cion.Lisp<`(filter number? [0 1 2])`>>

const filter_test_7 : true = {} as Equal<`[]`, 
Cion.Lisp<`(filter number? [true false 's'])`>>
