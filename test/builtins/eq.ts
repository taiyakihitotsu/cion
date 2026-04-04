import type Cion from '../../src/index'
import type { LispEq, _LispEq, _LispCollEq, Eval, MakeVar } from '../../src/index'
import type { Equal } from '../../src/util'

/** ------------------------------
      Internal Map Equality
------------------------------- */

type DumpFlatMap = ['map', [['key', 'a'], ['prim', '1'], ['key', 'b'], ['prim', 'b-str']]]
type LispCollEq_Dump_Flat =
  _LispCollEq<
     DumpFlatMap
   , DumpFlatMap
   , [['key', 'a'], ['key', 'b']]>
const expected_LispCollEq_Dump_Flat: true = {} as Equal<{r: {r: {r: true}}}, LispCollEq_Dump_Flat>

type LispCollEq_Dump_Flat_Rev =
  _LispCollEq<
     DumpFlatMap
   , ['map', [['key', 'b'], ['prim', 'b-str'], ['key', 'a'], ['prim', '1']]]
   , [['key', 'a'], ['key', 'b']]>
const expected_LispCollEq_Dump_Flat_Rev: true = {} as Equal<{r: {r: {r: true}}}, LispCollEq_Dump_Flat_Rev>

type DumpDeepMap = ['map', [['key', 'a'], ['prim', '1'], ['key', 'b'], ['map', [['key', 'b0'], ['prim', '2'], ['key', 'b1'], ['prim', '3']]]]]
type LispCollEq_Dump_Deep =
  _LispCollEq<
     DumpDeepMap
   , DumpDeepMap
   , [['key', 'a'], ['key', 'b']]>
const expected_LispCollEq_Dump_Deep: true = {} as Equal<{r: {r: {r: true}}}, LispCollEq_Dump_Deep>

type LispCollEq_Dump_Deep_Rev =
  _LispCollEq<
     ['map', [['key', 'b'], ['map', [['key', 'b1'], ['prim', '3'], ['key', 'b0'], ['prim', '2']]], ['key', 'a'], ['prim', '1']]]
   , DumpDeepMap
   , [['key', 'a'], ['key', 'b']]>
const expected_LispCollEq_Dump_Deep_Rev: true = {} as Equal<{r: {r: {r: true}}}, LispCollEq_Dump_Deep_Rev>

/** -----------------------
      Internal LispEq
------------------------ */

type LispEq_VecTest_0_Actual =
_LispEq< ['vec', ['prim', `'x'`]]
         , ['vec', ['prim', `'y'`]]>
const LispEq_VecTest_0: Equal<LispEq_VecTest_0_Actual, ['prim', false]> = true

type LispEq_VecTest_1_Actual =
_LispEq< ['vec', ['prim', `'x'`], ['vec', ['prim', true]]]
         , ['vec', ['prim', `'x'`], ['vec', ['prim', true]]]>
const LispEq_VecTest_1: Equal<LispEq_VecTest_1_Actual, ['prim', true]> = true

type LispEq_PrimTest_0_Actual =
_LispEq< ['prim', 1]
         , ['prim', 0]>
const LispEq_PrimTest_0: Equal<LispEq_PrimTest_0_Actual, ['prim', false]> = true

type LispEq_PrimTest_1_Actual = _LispEq<['prim', 0], ['prim', 0]>
const LispEq_PrimTest_1: Equal<LispEq_PrimTest_1_Actual, ['prim', true]> = true

type LispEq_MapTest_0_Actual = _LispEq<['map', [['key', 'a'], ['prim', '1'], ['key', 'b'], ['prim', 'b-str']]], ['map', [['key', 'a'], ['prim', '1'], ['key', 'b'], ['prim', 'b-str']]]>
const LispEq_MapTest_0: Equal<LispEq_MapTest_0_Actual, ['prim', true]> = true

const eq_test_0 : true = {} as Equal<['vec', ['sym', '='], ['prim', "'in'"], ['prim', "'in'"]], Eval<['vec', ['sym', '='], ['prim', "'in'"], ['prim', "'in'"]]>>
const eq_test_1 : true = {} as Equal<[`prim`, true], Eval<[[`sym`, `eq`], [`prim`, 0], [`prim`, 0]]>>
const eq_test_2 : true = {} as Equal<[`prim`, false], Eval<[[`sym`, `eq`], [`prim`, 1], [`prim`, 0]]>>
const eq_test_3 : true = {} as Equal<[`prim`, true], Eval<[[`sym`, `eq`], [`prim`, 0], [`prim`, 0], [`prim`, 0]]>>
const eq_test_4 : true = {} as Equal<[`prim`, false], Eval<[[`sym`, `eq`], [`prim`, 0], [`prim`, 1]]>>
const eq_test_5 : true = {} as Equal<[`prim`, false], Eval<[[`sym`, `eq`], [`sym`, `a`], [`prim`, 1]], [[MakeVar<`a`, [`prim`, 0]>]]>>
const eq_test_7 : true = {} as Equal<[`prim`, true], LispEq<[[`prim`, "'a'"], [`prim`, "'a'"]]>>
const eq_test_8 : true = {} as Equal<[`prim`, false], LispEq<[[`prim`, "'a'"], [`prim`, "'b'"]]>>
const eq_test_9 : true = {} as Equal<[`prim`, false], LispEq<[[`prim`, "'a'"], [`prim`, "'b'"], [`prim`, "'a'"]]>>
const eq_test_10 : true = {} as Equal<[`prim`, true], LispEq<[[`prim`, "'a'"], [`prim`, "'a'"], [`prim`, "'a'"]]>>
const eq_test_11 : true = {} as Equal<[`prim`, false], LispEq<[[`prim`, "'a'"], [`prim`, "''"]]>>
const eq_test_12 : true = {} as Equal<[`prim`, false], Cion.RawLisp<"(eq 'a' 'b')">>
const eq_test_13 : true = {} as Equal<[`prim`, true], Cion.RawLisp<"(eq 'a' 'a')">>
const eq_test_14 : true = {} as Equal<[`prim`, true], Cion.RawLisp<"(let [a 'a'] (eq a 'a'))">>
const eq_test_15 : true = {} as Equal<[`prim`, false], Cion.RawLisp<"(let [a 'b'] (eq a 'a'))">>
const eq_test_16 : true = {} as Equal<[`prim`, true], Cion.RawLisp<"(eq 1 1)">>
const eq_test_17 : true = {} as Equal<[`prim`, false], Cion.RawLisp<"(eq 1 2)">>
const eq_test_18 : true = {} as Equal<[`prim`, true], Cion.RawLisp<"(let [a 1] (eq a 1))">>
const eq_test_19 : true = {} as Equal<[`prim`, false], Cion.RawLisp<"(let [a 2] (eq a 1))">>
const eq_test_20 : true = {} as Equal<[`prim`, false], Cion.RawLisp<"(= 'a' 'b')">>
const eq_test_21 : true = {} as Equal<[`prim`, true], Cion.RawLisp<"(= 'a' 'a')">>
const eq_test_22 : true = {} as Equal<[`prim`, true], Cion.RawLisp<"(let [a 'a'] (= a 'a'))">>
const eq_test_23 : true = {} as Equal<[`prim`, false], Cion.RawLisp<"(let [a 'b'] (= a 'a'))">>
const eq_test_24 : true = {} as Equal<[`prim`, true], Cion.RawLisp<"(= 1 1)">>
const eq_test_25 : true = {} as Equal<[`prim`, false], Cion.RawLisp<"(= 1 2)">>
const eq_test_26 : true = {} as Equal<[`prim`, true], Cion.RawLisp<"(let [a 1] (= a 1))">>
const eq_test_27 : true = {} as Equal<[`prim`, false], Cion.RawLisp<"(let [a 2] (= a 1))">>

/** -----------------------
      `Cion.Lisp` test
------------------------- */

const eq_test_28 : true = {} as Equal<'true', Cion.Lisp<`(= 0 0)`>>
const eq_test_29 : true = {} as Equal<'true', Cion.Lisp<`(= 'abcd' 'abcd')`>>
const eq_test_30 : true = {} as Equal<'true', Cion.Lisp<`(= true true)`>>
const eq_test_31 : true = {} as Equal<'true', Cion.Lisp<`(= [0 1 2] [0 1 2])`>>
const eq_test_32 : true = {} as Equal<'true', Cion.Lisp<`(= [0 1 2] [0 1 4/2])`>>
const eq_test_33 : true = {} as Equal<'true', Cion.Lisp<`(= [0 [1 2]] [0 [1 2]])`>>
const eq_test_34 : true = {} as Equal<'true', Cion.Lisp<`(= [] [])`>>
const eq_test_35 : true = {} as Equal<'true', Cion.Lisp<`(= {:a 1} {:a 1})`>>
const eq_test_36 : true = {} as Equal<'true', Cion.Lisp<`(= {:a 1 :b 2} {:a 1 :b 2})`>>
const eq_test_37 : true = {} as Equal<'true', Cion.Lisp<`(= {:b 2 :a 1} {:a 1 :b 2})`>>
const eq_test_38 : true = {} as Equal<'true', Cion.Lisp<`(= inc inc)`>>
const eq_test_39 : true = {} as Equal<'false', Cion.Lisp<`(= inc (fn [x] (+ 1 x)))`>>

// Recursively 
const eq_test_40 : true = {} as Equal<'true', Cion.Lisp<`(= {:b 2 :a {:c 0 :d 1}} {:a {:d 1 :c 0} :b 2})`>>

const eq_test_41 : true = {} as Equal<'true', Cion.Lisp<`(= (fn [x y] (+ 1 x)) (fn [x y] (+ 1 x)))`>>
const eq_test_42 : true = {} as Equal<'false', Cion.Lisp<`(= (fn [x a] (+ 1 x)) (fn [x b] (+ 1 x)))`>>
