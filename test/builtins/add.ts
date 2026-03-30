import Cion, { LispAdd } from '../../src/index'
import type { Equal } from '../../src/util'

// -- integer
const integer_test0: true = {} as Equal<[`prim`, ['0000000000000100', '0000000000000001']],LispAdd<[[`prim`, '00000011'], [`prim`, '0000001']]>>
const interger_test1: true = {} as Equal<[`prim`, ['0000000000000111', '0000000000000001']], LispAdd<[[`prim`, '00000011'], [`prim`, '0000001'], [`prim`, '00000011']]>>
const integer_test2: true = {} as Equal<[`prim`, ['0000000000010000', '0000000000000001']], LispAdd<[[`prim`, '00001001'], [`prim`, '00000110'], [`prim`, '00000001']]>>

// -- rational
const rational_test_0: true = {} as Equal<[`prim`, ['0000000000010000', '0000000000000010']], LispAdd<[[`prim`, ['00001001', '0000000000000010']], [`prim`, ['00000110', '0000000000000010']], [`prim`, ['00000001', '0000000000000010']]]>>

// -- culculated
const add_test_0: true = {} as Equal<['prim', ["0000000000000010", '0000000000000001']], Cion.RawLisp<"(+ 1 1)">>
const add_test_1: true = {} as Equal<['prim', ["0000000000000001", '0000000000000001']], Cion.RawLisp<"(+ 1 0)">>
const add_test_2: true = {} as Equal<['prim', ["0000000000000000", '0000000000000001']], Cion.RawLisp<"(+ 0 0)">>
const add_test_3: true = {} as Equal<['prim', ["0000000000001001", '0000000000000001']], Cion.RawLisp<"(+ 4 5)">>
const add_test_4: true = {} as Equal<['prim', ["0000000000001111", '0000000000000001']], Cion.RawLisp<"(+ 5 4 6)">>
const add_test_5: true = {} as Equal<['prim', ["0000000000001001", '0000000000000001']], Cion.RawLisp<"(+ 5 4 0)">>
const add_test_6: true = {} as Equal<['prim', ["0000000000001001", '0000000000000001']], Cion.RawLisp<"(+ 5 0 4)">>
const add_test_7: true = {} as Equal<['prim', ["0000000000001001", '0000000000000001']], Cion.RawLisp<"(+ 0 4 5)">>
const add_test_8: true = {} as Equal<['prim', ["0000000000000000", '0000000000000001']], Cion.RawLisp<"(+ -1 1)">>
const add_test_9: true = {} as Equal<['prim', ["1111111111111100", '0000000000000001']], Cion.RawLisp<"(+ -2 -2)">>

// String Expression Test
const add_test_sexpr_0: true = {} as Equal<'1', Cion.Lisp<`(+ 1 0)`>>
const add_test_sexpr_1: true = {} as Equal<'5/3', Cion.Lisp<`(+ 1 2/3)`>>
const add_test_sexpr_2: true = {} as Equal<'1/3', Cion.Lisp<`(+ 1 -2/3)`>>

// Abnormal Test
type GetError<T> = T extends { ast: { error: infer E } } ? E : never;
const error_test_0: true = {} as Equal<'LispAddError2', GetError<Cion.Lisp<`(+ 1 nil)`>>>
const error_test_1: true = {} as Equal<'LispAddError2', GetError<Cion.Lisp<`(+ 1 true)`>>>
const error_test_2: true = {} as Equal<'LispAddError2', GetError<Cion.Lisp<`(+ 1 'string')`>>>
const error_test_3: true = {} as Equal<'LispAddError2', GetError<Cion.Lisp<`(+ 1 inc)`>>>
const error_test_4: true = {} as Equal<'LispAddError2', GetError<Cion.Lisp<`(+ 1 (fn [x] x))`>>>
const error_test_5: true = {} as Equal<'LispAddError2', GetError<Cion.Lisp<`(+ 1 [])`>>>
const error_test_6: true = {} as Equal<'LispAddError2', GetError<Cion.Lisp<`(+ 1 {})`>>>
