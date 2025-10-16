import type Cion from '../src/index'
import type { Eq, LispEq, Eval, MakeVar } from '../src/index'

const eqtest1: Eq<"a", "a"> = true;
const eqtest2: Eq<"a", ""> = false;
const eqtest3: Eq<null, []> = false;
// const eqtest4: Eq<undefined, null> = true
const eqtest5: Eq<undefined, undefined> = true;
const eqtest6: Eq<{}, null> = false;
const eqtest7: Eq<1, "1"> = false;
const eqtest8: Eq<["a"], ["a", ""]> = false;
const eqtest9: Eq<[""], ["a"]> = false;
const eqtest10: Eq<["a"], ["a"]> = true;
const eqtest11: Eq<[""], [""]> = true;

const testletfn9: Eval<['vec', ['sym', '='], ['prim', "'in'"], ['prim', "'in'"]]> = ['vec', ['sym', '='], ['prim', "'in'"], ['prim', "'in'"]]

// test lispeq
const evallispeqtest0: Eval<[[`sym`, `eq`], [`prim`, 0], [`prim`, 0]]> = [
  `prim`,
  true,
];
const evallispeqtest1: Eval<[[`sym`, `eq`], [`prim`, 1], [`prim`, 0]]> = [
  `prim`,
  false,
];
const evallispeqtest2: Eval<
  [[`sym`, `eq`], [`prim`, 0], [`prim`, 0], [`prim`, 0]]
> = [`prim`, true];

const testfnlispeqa: Eval<[[`sym`, `eq`], [`prim`, 0], [`prim`, 1]]> = [
  `prim`,
  false,
];
const testfnlispeqaa: Eval<
  [[`sym`, `eq`], [`sym`, `a`], [`prim`, 1]],
  [[MakeVar<`a`, [`prim`, 0]>]]
> = [`prim`, false];

const lispeqtest1: LispEq<[[`prim`, "'a'"], [`prim`, "'a'"]]> = [`prim`, true];
const lispeqtest2: LispEq<[[`prim`, "'a'"], [`prim`, "'b'"]]> = [`prim`, false];
const lispeqtest3: LispEq<[[`prim`, "'a'"], [`prim`, "'b'"], [`prim`, "'a'"]]> =
  [`prim`, false];
const lispeqtest4: LispEq<[[`prim`, "'a'"], [`prim`, "'a'"], [`prim`, "'a'"]]> =
  [`prim`, true];
const lispeqtest5: LispEq<[[`prim`, "'a'"], [`prim`, "''"]]> = [`prim`, false];

const maintest0_eq: Cion.RawLisp<"(eq 'a' 'b')"> = [`prim`, false]
const maintest1_eq: Cion.RawLisp<"(eq 'a' 'a')"> = [`prim`, true]
const maintest2_eq: Cion.RawLisp<"(let [a 'a'] (eq a 'a'))"> = [`prim`, true]
const maintest3_eq: Cion.RawLisp<"(let [a 'b'] (eq a 'a'))"> = [`prim`, false]

const maintest0a_eq: Cion.RawLisp<"(eq 1 1)"> = [`prim`, true]
const maintest1a_eq: Cion.RawLisp<"(eq 1 2)"> = [`prim`, false]
const maintest2a_eq: Cion.RawLisp<"(let [a 1] (eq a 1))"> = [`prim`, true]
const maintest3a_eq: Cion.RawLisp<"(let [a 2] (eq a 1))"> = [`prim`, false]

const maintest0_eq1: Cion.RawLisp<"(= 'a' 'b')"> = [`prim`, false]
const maintest1_eq1: Cion.RawLisp<"(= 'a' 'a')"> = [`prim`, true]
const maintest2_eq1: Cion.RawLisp<"(let [a 'a'] (= a 'a'))"> = [`prim`, true]
const maintest3_eq1: Cion.RawLisp<"(let [a 'b'] (= a 'a'))"> = [`prim`, false]

const maintest0a_eq1: Cion.RawLisp<"(= 1 1)"> = [`prim`, true]
const maintest1a_eq1: Cion.RawLisp<"(= 1 2)"> = [`prim`, false]
const maintest2a_eq1: Cion.RawLisp<"(let [a 1] (= a 1))"> = [`prim`, true]
const maintest3a_eq1: Cion.RawLisp<"(let [a 2] (= a 1))"> = [`prim`, false]

const test_t_0: Cion.Lisp<`(= 0 0)`> = 'true'
const test_t_1: Cion.Lisp<`(= 'abcd' 'abcd')`> = 'true'
const test_t_2: Cion.Lisp<`(= true true)`> = 'true'
const test_t_3: Cion.Lisp<`(= [0 1 2] [0 1 2])`> = 'true'
const test_t_3b: Cion.Lisp<`(= [0 1 2] [0 1 4/2])`> = 'true'
const test_t_4: Cion.Lisp<`(= [0 [1 2]] [0 [1 2]])`> = 'true'
const test_t_5: Cion.Lisp<`(= [] [])`> = 'true'
const test_t_6: Cion.Lisp<`(= {:a 1} {:a 1})`> = 'true'
const test_t_7: Cion.Lisp<`(= {:a 1 :b 2} {:a 1 :b 2})`> = 'true'
const test_t_8: Cion.Lisp<`(= {:b 2 :a 1} {:a 1 :b 2})`> = 'true'
const test_t_9: Cion.Lisp<`(= inc inc)`> = 'true'
const test_t_10: Cion.Lisp<`(= inc (fn [x] (+ 1 x)))`> = 'false'
