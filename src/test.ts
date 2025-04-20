import type Lisp from './index'

const maintest0: Lisp<"(eq 'a' 'b')"> = [`prim`, false]
const maintest1: Lisp<"(eq 'a' 'a')"> = [`prim`, true]
const maintest2: Lisp<"(let [a 'a'] (eq a 'a'))"> = [`prim`, true]
const maintest3: Lisp<"(let [a 'b'] (eq a 'a'))"> = [`prim`, false]
// todo :
// string split works but not correctly, in current.
// Use _ as space until I will have implemented a string parser. 
const maintest4: Lisp<"(let [a 'a'] (if (eq a 'a') 'this_is_true', 'this_is_false')"> = [`prim`, "'this_is_false'"]
const maintest5: Lisp<"(if true 1 2)"> = ['prim', '0000000000000001']
const maintest6: Lisp<"(if true (let [a 'astr' b 'bstr'] (str a b)) 11)"> = ['prim', `'astrbstr'`]

const maintest7_and: Lisp<"(and true true)"> = ['prim', true]
const maintest8_and: Lisp<"(and true false)"> = ['prim', false]
const maintest9_and: Lisp<"(and false false)"> = ['prim', false]
const maintest10_and: Lisp<"(and false true)"> = ['prim', false]
const maintest7_1_and: Lisp<"(and true true true)"> = ['prim', true]
const maintest8_1_and: Lisp<"(and false true false)"> = ['prim', false]
const maintest9_1_and: Lisp<"(and false false false)"> = ['prim', false]
const maintest10_1_and: Lisp<"(and false true true)"> = ['prim', false]

const maintest7_or: Lisp<"(or true true)"> = ['prim', true]
const maintest8_or: Lisp<"(or true false)"> = ['prim', true]
const maintest9_or: Lisp<"(or false false)"> = ['prim', false]
const maintest10_or: Lisp<"(or false true)"> = ['prim', true]
const maintest7_1_or: Lisp<"(or true true true)"> = ['prim', true]
const maintest8_1_or: Lisp<"(or false true false)"> = ['prim', true]
const maintest9_1_or: Lisp<"(or false false false)"> = ['prim', false]
const maintest10_1_or: Lisp<"(or false true true)"> = ['prim', true]

const maintest11_gt_0: Lisp<"(> 00001111 00001110)"> = [`prim`, true]
const maintest11_gt_1: Lisp<"(> 00001111 00001110 00001100)"> = [`prim`, true]
const maintest11_gt_2: Lisp<"(> 00001111 00001110 00010000)"> = [`prim`, false]

const maintest11_lt_0: Lisp<"(< 00001111 00001110)"> = [`prim`, false]
const maintest11_lt_1: Lisp<"(< 00001111 00001110 00001100)"> = [`prim`, false]
const maintest11_lt_0_1: Lisp<"(< 00001110 00001111)"> = [`prim`, true]
const maintest11_lt_1_1: Lisp<"(< 00001100 00001110 00001111)"> = [`prim`, true]
const maintest11_lt_2: Lisp<"(< 00001111 00001110 00010000)"> = [`prim`, false]

const maintest11_gte_0: Lisp<"(>= 00001111 00001110)"> = [`prim`, true]
const maintest11_gte_1: Lisp<"(>= 00001111 00001110 00001100)"> = [`prim`, true]
const maintest11_gte_2: Lisp<"(>= 00001111 00001110 00010000)"> = [`prim`, false]
const maintest11_gte_0_1: Lisp<"(>= 00001110 00001111)"> = [`prim`, false]
const maintest11_gte_1_1: Lisp<"(>= 00001100 00001110 00001111)"> = [`prim`, false]
const maintest11_gte_2_1: Lisp<"(>= 00001111 00001111 00001111)"> = [`prim`, true]

const maintest11_lte_0: Lisp<"(<= 00001111 00001110)"> = [`prim`, false]
const maintest11_lte_1: Lisp<"(<= 00001111 00001110 00001100)"> = [`prim`, false]
const maintest11_lte_2: Lisp<"(<= 00001111 00001110 00010000)"> = [`prim`, false]
const maintest11_lte_0_1: Lisp<"(<= 00001110 00001111)"> = [`prim`, true]
const maintest11_lte_1_1: Lisp<"(<= 00001100 00001110 00001111)"> = [`prim`, true]
const maintest11_lte_2_1: Lisp<"(<= 00001111 00001111 00001111)"> = [`prim`, true]

const maintest12_get_0: Lisp<"(:a {:a 1})"> = ['prim', '0000000000000001']
const maintest12_get_1: Lisp<"(:a {:a 1 :b 2})"> = ['prim', '0000000000000001']
const maintest12_get_2: Lisp<"({:a 1 :b 2} :a)"> = ['prim', '0000000000000001']
const maintest12_get_3: Lisp<"(:c {:a 1 :b 2})"> = []
const maintest12_get_4: Lisp<"({:a 1 :b 2} :c)"> = []
const maintest12_get_5: Lisp<"(get {:a 1 :b 2} :a)"> = ['prim', '0000000000000001']

const maintest_threadf_0: Lisp<"(-> 's' (str '01'))"> = ['prim', "'s01'"]
const maintest_threadf_1: Lisp<"(-> 'a' (str '01') (str 's'))"> = ['prim', "'a01s'"]
const maintest_threadf_2: Lisp<"(str 'a' (str '01' 's'))"> = ['prim', "'a01s'"]
const maintest_threadf_3: Lisp<"(-> 1 (+ 1) (+ 2))"> = ['prim', '0000000000000100']
const maintest_threadf_4: Lisp<"(+ 2 (+ 1 1))"> = ['prim', '0000000000000100']

const maintest_threadl_0: Lisp<"(->> 's' (str '01'))"> = ['prim', "'01s'"]
const maintest_threadl_1: Lisp<"(->> 'a' (str '01') (str 's'))"> = ['prim', "'s01a'"]
const maintest_threadl_2: Lisp<"(str 'a' (str '01' 's'))"> = ['prim', "'a01s'"]
const maintest_threadl_3: Lisp<"(->> 1 (+ 1) (+ 2))"> = ['prim', '0000000000000100']
const maintest_threadl_4: Lisp<"(+ 2 (+ 1 1))"> = ['prim', '0000000000000100']



