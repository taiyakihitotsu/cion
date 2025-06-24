import type Cion from '../src/index'
import type { Eval } from '../src/index'
import type {LetVal,LetArg,LetForm,Each,Atom,TMap,Sexpr,TNil,Keyword,Sym,PrimString,PrimBoolean,PrimTestNumber,PrimNumber,Prim,Args,Fn,Vector,Var,Env,TNotMatch,IfForm} from '../src/sexprtypes'

// test let
const evallettest: Eval<[`let`, [[`sym`, `t`], `'test'`], [`sym`, `t`]]> = {error: 'EvalError7', message: '', sexpr: `'test'`, env: [[]]}
const evallettest_true: Eval<[`let`, [[`sym`, `t`], ['prim', `'test'`]], [`sym`, `t`]]> = ['prim', `'test'`]

// recursive test[let]
type InnerLetTest = [
  `let`,
  [[`sym`, `str`], [`prim`, `'test'`]],
  [[`sym`, `AppendP`], [`sym`, `str`]],
];
type OuterLetTest = [
  `let`,
  [[`sym`, `aaa`], InnerLetTest],
  [[`sym`, `AppendP`], [`sym`, `aaa`]],
];
type RecLetTest = [
  `let`,
  [[`sym`, `aaa`], InnerLetTest],
  [
    `let`,
    [[`sym`, `bbb`], [`sym`, `aaa`]],
    [[`sym`, `AppendP`], [`sym`, `bbb`]],
  ],
];

const evalletwprimtest: Eval<
  [`let`, [[`sym`, `t`], [`prim`, `'test'`]], [`sym`, `t`]],
  []
> = [`prim`, "'test'"];

// recursive test[fn in let]
type FlInnerTest = [
  `let`,
  [[`sym`, `str`], [`fn`, [[`sym`, `a`]], [[`sym`, `AppendP`], [`sym`, `a`]]]],
  [[`sym`, `str`], [`prim`, `'test'`]],
];

// test interleaved let form
const testiletform: Eval<
  [
    `let`,
    [[[`sym`, `a`], [`sym`, `b`]], [[`prim`, `'1'`], [`prim`, `'2'`]]],
    [[`sym`, `str`], [`sym`, `a`], [`sym`, `b`]],
  ]
> = [`prim`, `'12'`];

const testttt1: Eval<['let', [], ['prim', '1']]> = ['prim', '1']

// test let >1
type Letmoretest = [
  `let`,
  [[`sym`, `a`], [`prim`, `text-a`], [`sym`, `b`], [`prim`, `/text-b`]],
  [[`sym`, `str`], [`sym`, `a`], [`sym`, `b`]],
];
const evalletmoretest1: Eval<Letmoretest> = [`prim`, `'text-a/text-b'`];
const aaaaaaaaa: LetForm = [
  `let`,
  [
    [`sym`, `a`],
    [`prim`, `text-a`],
    [`sym`, `b`],
    [`prim`, `text-b`],
  ],
  [
    [`sym`, `str`],
    [`sym`, `a`],
    [`sym`, `b`],
  ],
];

const testletfn0: Eval<['let', [['sym', 'x'], [['fn', [['sym', 'a']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]], ['prim', '0000000000000001']]], [['sym', '*'], ['prim', '0000000000000010'], ['sym', 'x']]]> = ['prim', ['0000000000000100', '0000000000000001']]
const testletfn1: Eval<['let', [['sym', 'x'], [['fn', [['sym', 'a']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]], ['prim', '0000000000000001']]], [['sym', '*'], ['prim', '0000000000000010'], ['sym', 'x']]]> = ['prim', ['0000000000000100', '0000000000000001']]
const testletfn2: Eval<['let', [['sym', 'x'], ['fn', [['sym', 'a']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]]], [['sym', 'x'], ['prim', '0000000000000010'], ['prim', '0000000000000001']]]> = ['prim', ['0000000000000011', '0000000000000001']]
const testletfn3: Eval<['let', [['sym', 'x'], ['fn', [['sym', 'a'], ['sym', 'b']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]]], [['sym', 'x'], ['prim', '0000000000000010'], ['prim', '0000000000000001']]]> = ['prim', ['0000000000000011', '0000000000000001']]
const testletfn4: Eval<['let', [['sym', 'x'], ['fn', [['sym', 'a'], ['sym', 'b']], [['sym', '+'], ['sym', 'b'], ['sym', 'a']]]], [['sym', 'x'], ['prim', '0000000000000010'], ['prim', '0000000000000001']]]> = ['prim', ['0000000000000011', '0000000000000001']]
const testletfn5: Eval<['let', [['sym', 'x'], ['let', [['sym', 'y'], ['prim', true]], ['sym', 'y']]], ['sym', 'x']]> = ['prim', true]
const testletfn6: Eval<['let', [['sym', 'x'], ['fn', [['sym', 'a']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]]], ['sym', 'x']]> = ['fn', [['sym', 'a']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]]
const testletfn7: Eval<['let', [['sym', 'x'], ['let', [['sym', 'a'], ['prim', '0000000000000010'], ['sym', 'b'], ['prim', '0000000000000010']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]]], ['sym', 'x']]> = ['prim', ['0000000000000100', '0000000000000001']]
const testletfn8: Eval<['let', [['sym', 'x'], ['let', [['sym', 'a'], ['prim', '0000000000000011'], ['sym', 'b'], ['prim', '0000000000000010']], ['fn', [['sym', 'c']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]]]], ['sym', 'x']]> = ['fn', [['sym', 'c']], [['sym', '+'], ['prim', '0000000000000011'], ['prim', '0000000000000010']]]


const maintest0_letif_0: Cion.RawLisp<'(let [x (fn [a b] (+ a b)) y 8] (+ 2 1))'> = ['prim', ['0000000000000011', '0000000000000001']]
const maintest0_letif_01: Cion.RawLisp<'(let [x (fn [a b] (+ a b)) y 8] (+ y 2 1))'> = ['prim', ['0000000000001011', '0000000000000001']]
const maintest0_letif_02: Cion.RawLisp<'(let [x (fn [a] (+ 1 a))] (x 1 1))'> = ['prim', ['0000000000000010', '0000000000000001']]
// const testletfn2: Eval<['let', [['sym', 'x'], ['fn', [['sym', 'a']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]]], [['sym', 'x'], ['prim', '0000000000000010'], ['prim', '0000000000000001']]]> = ['prim', '0000000000000011']
const maintest0_letif_03: Cion.RawLisp<'(let [x (fn [a] (+ 1 a)) y 9] (x 1 1))'> = ['prim', ['0000000000000010', '0000000000000001']]
const maintest0_letif_04: Cion.RawLisp<'(let [x (fn [a] (+ 1 a)) y 9] (x y 1))'> = ['prim', ['0000000000001010', '0000000000000001']]
const maintest0_letif_05: Cion.RawLisp<'(let [x (fn [a b] (+ 1 a)) y 9] (x 1 1))'> = ['prim', ['0000000000000010', '0000000000000001']]
const maintest0_letif_06: Cion.RawLisp<'(let [x (fn [a b] (+ b a)) y 9] (x 1 2))'> = ['prim', ['0000000000000011', '0000000000000001']]
const maintest0_letif_07: Cion.RawLisp<'((fn [a b] (+ b a)) 1 2)'> = ['prim', ['0000000000000011', '0000000000000001']]
const maintest0_letif_1: Cion.RawLisp<'(let [x ((fn [a] (+ a 1)) 1)] (* 2 x))'> = ['prim', ['0000000000000100', '0000000000000001']] 
const maintest0_letif_2: Cion.RawLisp<'(let [x ((fn [a b] (+ a 1 b)) 1 8)] (* 2 x))'> = ['prim', ['0000000000010100', '0000000000000001']]
const maintest0_letif_3: Cion.RawLisp<'(let [x 8 y ((fn [a b] (+ a x b)) 1 8)] (* 2 y))'> = ['prim', ['0000000000100010', '0000000000000001']]
const maintest0_letif_4: Cion.RawLisp<'(let [x (let [a 2 b 6] (+ a b)) y ((fn [a b] (+ a x b)) 1 8)] (* 2 y))'> = ['prim', ['0000000000100010', '0000000000000001']]
const maintest0_letif_5: Cion.RawLisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) y 8] x)'> = ['fn', [['sym', 'c']], [['sym', '+'], ['prim', '0000000000000010'], ['prim', '0000000000000110'], ['sym', 'c']]]
const maintest0_letif_6: Cion.RawLisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c)))] (x 10))'> = ['prim', ['0000000000010010', '0000000000000001']]
const maintest0_letif_7: Cion.RawLisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) y ((fn [d] (x 2 d)) 10)] y)'> = ['prim', ['0000000000001010', '0000000000000001']]
const maintest0_letif_8: Cion.RawLisp<'(let [z 2 y ((fn [d] (+ z d)) 10)] y)'> = ['prim', ['0000000000001100', '0000000000000001']]
const maintest0_letif_9: Cion.RawLisp<'(let [z 2 y ((fn [d] (+ z d)) 10) x 10] y)'> = ['prim', ['0000000000001100', '0000000000000001']]
const maintest0_letif_10: Cion.RawLisp<'(let [z 2 y 3 x 10] y)'> = ['prim', '0000000000000011']
const maintest0_letif_11: Cion.RawLisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) z 2 y ((fn [d] (x z d)) 10)] (x z))'> = ['prim', ['0000000000001010', '0000000000000001']]
const maintest0_letif_12: Cion.RawLisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) z 20 y ((fn [d] (x d)) z)] y)'> = ['prim', ['0000000000011100', '0000000000000001']]
const maintest0_letif_13: Cion.RawLisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) z 20 y ((fn [d] (x d)) z 999)] y)'> = ['prim', ['0000000000011100', '0000000000000001']] // a case of a number of args being over. rest parts are ignored.

const maintest_let_0: Cion.RawLisp<'(let [a {:a "a"}] (+ 1 2))'> = ['prim', ['0000000000000011', '0000000000000001']]

const maintest_letmap_0: Cion.RawLisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= "in" (b a)))] (->> cv (map :status)))`> = ['vec', ['prim', "'in'"], ['prim', "'out'"], ['prim', "'out'"]]
const maintest_letmap_1a: Cion.RawLisp<`(= 'in' (:status {:status 'in'}))`> = ['prim', true]
// const maintest_letmap_1b: Cion.RawLisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= 'in' (b a)))] (->> cv first))`> = ['map', [['key', ':status'], ['prim', "'in'"], ['key', ':message'], ['prim', "'message1'"]]]
const maintest_letmap_1c: Cion.RawLisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] (= c (a b)))] (f :status {:status 'in' :message 'message1'} msg))`> = ['prim', true]
const maintest_letmap_1caaa: Cion.RawLisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] (= c (b a)))] (f :status {:status 'in' :message 'message1'} msg))`> = ['prim', true]
const maintest_letmap_1caa: Cion.RawLisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] [c (a b)])] (f :status {:status 'in' :message 'message1'} msg))`> = ['vec', ['prim', "'in'"], ['prim', "'in'"]]
const maintest_letmap_1cab: Cion.RawLisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] [= (a b)])] (f :status {:status 'in' :message 'message1'} msg))`> = ['vec', ['sym', '='], ['prim', "'in'"]]
const maintest_letmap_1ca: Cion.RawLisp<`[(:status {:status 'in' :message 'message1'}) 'in']`> = ['vec', ['prim', "'in'"], ['prim', "'in'"]]
const maintest_letmap_1cb: Cion.RawLisp<`[(:status {:status 'in' :message 'message1'}) ((fn [] 'in'))]`> = ['vec', ['prim', "'in'"], ['prim', "'in'"]]
const maintest_letmap_1cc: Cion.RawLisp<`(let [v 'in'] [(:status {:status 'in' :message 'message1'}) ((fn [] v))])`> = ['vec', ['prim', "'in'"], ['prim', "'in'"]]
const maintest_letmap_1cd: Cion.RawLisp<`(let [v 'in'] [(:status {:status 'in' :message 'message1'}) ((fn [] (first [v v])))])`> = ['vec', ['prim', "'in'"], ['prim', "'in'"]]
const maintest_letmap_1d: Cion.RawLisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] (= c (b a)))] [msg])`> = ['vec', ['prim', "'in'"]]
const maintest_letmap_1ce: Cion.RawLisp<`(let [v 'in'] [(:status {:status 'in' :message 'message1'}) ((fn [] (:a {:a 'in'})))])`> = ['vec', ['prim', "'in'"], ['prim', "'in'"]]
const maintest_aflet_0: Cion.RawLisp<`(+ 1 (let [x 1 y 2] (- x y)))`> = ['prim', ['0000000000000000', '0000000000000001']]

const testsometh5aaa: Cion.Lisp<`(some-> 1 boolean? string? ((fn [n] (if (= n false) 2 -2))) neg-int?)`> = 'nil'
const testsometh5a: Cion.Lisp<`(some-> 1 boolean? string? ((fn [n] (if (= n false) 2 -2))) pos-int?)`> = 'true'
const testsometh5aa: Cion.Lisp<`(some-> 1 boolean? string? ((fn [n] (if (= n false) 2 -2))) inc)`> = '3'

const maintest_aflet_1: Cion.RawLisp<`[1 (let [x 1 y 2] (- x y))]`> = ['vec', ['prim', '0000000000000001'], ['prim', ['1111111111111111', '0000000000000001']]]


// [note]
// see `some-thread-first.ts` too.
const testsometh5ab: Cion.Lisp<`(some-> 1 boolean? string? ((fn [n] (if (= n false) 2 -2))) inc pos-int?)`> = 'true'
const testsomet60: Cion.Lisp<`((fn [n] (some-> n number?)) 1)`> = 'true'
const testsometh00: Cion.Lisp<`((fn [n] (some-> n inc)) 1)`> = '2'

const _testsometh00: Cion.Lisp<`(let [m (inc n)] (if m m nil))`>['ast']['error'] = 'EvalError12'
const _testsometh01: Cion.Lisp<`(let [m (inc 1)] (if m m nil))`> = '2'
const _testsometh02: Cion.Lisp<`(let [m (inc n)] m)`>['ast']['error'] = 'EvalError12'
const _testsometh03: Cion.Lisp<`(let [m (inc 1)] m)`> = '2'
const _testsometh03a: Cion.Lisp<`(let [m (fn [n] (inc n))] m)`> = '(fn [n] (inc n))'
const _testsometh03b: Cion.Lisp<`(let [m ((fn [n] (inc n)) 1)] m)`> = '2' // m
const _testsometh05: Cion.Lisp<`(let [m 1] m)`> = '1'

// fn -> let
const ___testsometh06: Cion.Lisp<`(let [r (update-in [0 1 [2]] [2 0] inc)] r)`> = '[0 1 [3]]'
const ___testsometh06x: Cion.Lisp<`((fn [m] (let [r [0 1 [(m 2)]]] r)) inc)`> = '[0 1 [3]]'
const _testsmeth06: Cion.Lisp<`((fn [m] (let [r (update-in [0] [0] m)] r)) inc)`> = '[1]'
const _testsmeth07: Cion.Lisp<`((fn [m] (let [r (update-in (get m 0) (get m 1) (get m 2))] r)) [{:b 2 :a 1} [:a] inc])`> = `{:b 2 :a 2}`
// const _testsmeth08: Cion.Lisp<`(fn [m] (let [r (update-in (get m 0) (get m 1) (get m 2))] r))`>['ast']['sexpr'] = ['fn', [['sym', 'm']], ['let',[['sym', 'r'], [['sym', 'update-in'], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000000']], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000001']], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000010']]]],['sym','r']]]
// const _testsmeth08: Eval<['fn', [['sym', 'm']], ['let',[['sym', 'r'], [['sym', 'update-in'], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000000']], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000001']], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000010']]]], ['sym','r']]]> = ''
const _testsmeth08_err0: Eval<['fn', [['sym', 'm']], ['let', [], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000010']]]]> = ['fn', [['sym', 'm']], ['let', [], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000010']]]]
const _testsmeth08_err1: Eval<['fn', [['sym', 'm']], ['let', [['sym', 'r'], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000010']]], ['sym', 'inc']]]> = ['fn', [['sym', 'm']], ['let', [['sym', 'r'], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000010']]], ['sym', 'inc']]]
const _testsmeth08_err2: Eval<['fn', [['sym', 'm']], ['let', [['sym', 'r'], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000010']]]]]>['sexpr'] = ['fn', [['sym', 'm']], ['let', [['sym', 'r'], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000010']]]]]
// const _testsmeth08_err0d: ['fn', [['sym', 'm']], ['let', [], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000010']]]]
// const _testsmeth08_err1d: ['fn', [['sym', 'm']], ['let', [['sym', 'r'], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000010']]], ['sym', 'inc']]]
const _testsmeth08_err2d: ['fn', [['sym', 'm']], ['let',[['sym', 'r'], [['sym', 'update-in'], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000000']], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000001']], [['sym', 'get'], ['sym', 'm'], ['prim', '0000000000000010']]]], ['sym','r']]][2] extends LetForm ? true : false = true
const _testmeth00_err3d:
   [ ['sym', 'r']
   , [ ['sym', 'update-in']
     , [ ['sym', 'get']
       , ['sym', 'm']
       , ['prim', '0000000000000000']]
     , [ ['sym', 'get']
       , ['sym', 'm']
       , ['prim', '0000000000000001']]
     , [ ['sym', 'get']
       , ['sym', 'm']
       , ['prim', '0000000000000010']]]] extends LetArg ? true : false = true
// const _testmeth00_err3d:
//    [ ['sym', 'r']
//    , [ ['sym', 'update-in']
//      , [ ['sym', 'get']
//        , ['sym', 'm']
//        , ['prim', '0000000000000000']]
//      , [ ['sym', 'get']
//        , ['sym', 'm']
//        , ['prim', '0000000000000001']]
//      , [ ['sym', 'get']
//        , ['sym', 'm']
//        , ['prim', '0000000000000010']]]] extends [Sym, Sexpr | Each] | [] ? true : false = true
