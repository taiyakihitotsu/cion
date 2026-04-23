import type Cion from '../../src/index.js'
import type { Eval } from '../../src/index.js'
import type { Equal } from '../../src/util.js'

type ExpectedError_And_1 = {
  error: 'LispAddError1',
  message: 'Args should be sexpr.',
  sexpr: { sexpr: ['NotMatch'], error: 'ReadingError0', message: 'sexpr is not atom list.' }
}
type ActualError_And_1 = Eval<[['fn', [['sym', 'a'], ['sym', 'b']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]]]>
const fn_test_0 : true = {} as Equal<ExpectedError_And_1, ActualError_And_1>

const fn_test_1 : true = {} as Equal<['prim', ['0000000000000101', '0000000000000001']]
  ,Eval<[['fn', [['sym', 'a'], ['sym', 'b']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]], ['prim', '10'], ['prim', '11']]>>

const fn_test_2 : true = {} as Equal<[`prim`, false]
  ,Eval<
    [
      [`fn`, [[`sym`, `a`]], [[`sym`, `eq`], [`sym`, `a`], [`prim`, `'1'`]]],
      [`prim`, `'0'`],
    ]
  >>

const fn_test_3 : true = {} as Equal<[`prim`, true]
  ,Eval<
    [
      [`fn`, [[`sym`, `a`]], [[`sym`, `eq`], [`sym`, `a`], [`prim`, `'1'`]]],
      [`prim`, `'1'`],
    ]
  >>

const fn_test_4 : true = {} as Equal<['prim', true]
  ,Eval<[['fn', [['sym', 'aaa']], [['sym', 'not'], [['sym', '>'], ['prim', '0000000000000010'], ['sym', 'aaa']]]], ['prim', '0000000000000010']]>>

// Multi arguments fn test
const fn_test_5 : true = {} as Equal<[`prim`, `'01'`]
  ,Eval<
    [
      [
        `fn`,
        [[`sym`, `a`], [`sym`, `b`]],
        [[`sym`, `str`], [`sym`, `a`], [`sym`, `b`]],
      ],
      [`prim`, `'0'`],
      [`prim`, `'1'`],
    ]
  >>
// String Expression Test
const fn_test_6 : true = {} as Equal<'2'
  ,Cion.Lisp<'((fn [n] (n 1)) (fn [m] (+ 1 m)))'>>
const fn_test_7 : true = {} as Equal<'2'
  ,Cion.Lisp<'(let [b (fn [m] (+ 1 m))] ((fn [n] (n 1)) b))'>>
const fn_test_8 : true = {} as Equal<'3'
  ,Cion.Lisp<'(((fn [n] (fn [m] (+ n m 1))) 1) ((fn [] 1)))'>>
const fn_test_9 : true = {} as Equal<'3'
  ,Cion.Lisp<'(((fn [n] (fn [m] (+ n m 1))) 1) 1)'>>
const fn_test_10 : true = {} as Equal<'3/2'
  ,Cion.Lisp<'(((fn [n] (fn [m] (/ n m 1))) 3) 2)'>>

const fn_test_11 : true = {} as Equal<'3'
  ,Cion.Lisp<'((fn [n m] (+ n m)) 1 2)'>>
const fn_test_12 : true = {} as Equal<'3'
  ,Cion.Lisp<'((fn [f n m] (f n m)) + 1 2)'>>
