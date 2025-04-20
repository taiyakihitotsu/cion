import type Lisp from './index'

// todo :
// string split works but not correctly, in current.
// Use _ as space until I will have implemented a string parser. 
const maintest4: Lisp<"(let [a 'a'] (if (eq a 'a') 'this_is_true', 'this_is_false')"> = [`prim`, "'this_is_false'"]
const maintest5: Lisp<"(if true 1 2)"> = ['prim', '0000000000000001']
const maintest6: Lisp<"(if true (let [a 'astr' b 'bstr'] (str a b)) 11)"> = ['prim', `'astrbstr'`]

// boolean operators: and, or, not.

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

const maintest0_not: Lisp<"(not true)"> = ['prim', false]
const maintest1_not: Lisp<"(not false)"> = ['prim', true]

// order tests: >, <, >=, <=, =, eq.

const maintest0_eq: Lisp<"(eq 'a' 'b')"> = [`prim`, false]
const maintest1_eq: Lisp<"(eq 'a' 'a')"> = [`prim`, true]
const maintest2_eq: Lisp<"(let [a 'a'] (eq a 'a'))"> = [`prim`, true]
const maintest3_eq: Lisp<"(let [a 'b'] (eq a 'a'))"> = [`prim`, false]

const maintest0a_eq: Lisp<"(eq 1 1)"> = [`prim`, true]
const maintest1a_eq: Lisp<"(eq 1 2)"> = [`prim`, false]
const maintest2a_eq: Lisp<"(let [a 1] (eq a 1))"> = [`prim`, true]
const maintest3a_eq: Lisp<"(let [a 2] (eq a 1))"> = [`prim`, false]

const maintest0_eq1: Lisp<"(= 'a' 'b')"> = [`prim`, false]
const maintest1_eq1: Lisp<"(= 'a' 'a')"> = [`prim`, true]
const maintest2_eq1: Lisp<"(let [a 'a'] (= a 'a'))"> = [`prim`, true]
const maintest3_eq1: Lisp<"(let [a 'b'] (= a 'a'))"> = [`prim`, false]

const maintest0a_eq1: Lisp<"(= 1 1)"> = [`prim`, true]
const maintest1a_eq1: Lisp<"(= 1 2)"> = [`prim`, false]
const maintest2a_eq1: Lisp<"(let [a 1] (= a 1))"> = [`prim`, true]
const maintest3a_eq1: Lisp<"(let [a 2] (= a 1))"> = [`prim`, false]

const maintest11_gt_0: Lisp<"(> 15 14)"> = [`prim`, true]
const maintest11_gt_1: Lisp<"(> 15 14 13)"> = [`prim`, true]
const maintest11_gt_2: Lisp<"(> 15 14 16)"> = [`prim`, false]

const maintest11_lt_0: Lisp<"(< 15 14)"> = [`prim`, false]
const maintest11_lt_1: Lisp<"(< 15 14 13)"> = [`prim`, false]
const maintest11_lt_0_1: Lisp<"(< 14 15)"> = [`prim`, true]
const maintest11_lt_1_1: Lisp<"(< 13 14 15)"> = [`prim`, true]
const maintest11_lt_2: Lisp<"(< 13 15 14)"> = [`prim`, false]
const maintest11_lt_3: Lisp<"(< 15 14 13)"> = [`prim`, false]

const maintest11_gte_0: Lisp<"(>= 15 14)"> = [`prim`, true]
const maintest11_gte_1: Lisp<"(>= 15 14 13)"> = [`prim`, true]
const maintest11_gte_2: Lisp<"(>= 13 14 15)"> = [`prim`, false]
const maintest11_gte_0_1: Lisp<"(>= 14 15)"> = [`prim`, false]
const maintest11_gte_1_1: Lisp<"(>= 13 14 15)"> = [`prim`, false]
const maintest11_gte_2_1: Lisp<"(>= 15 15 15)"> = [`prim`, true]
const maintest11_gte_2_2: Lisp<"(>= 15 15 14)"> = [`prim`, true]
const maintest11_gte_2_3: Lisp<"(>= 15 14 14)"> = [`prim`, true]
const maintest11_gte_2_4: Lisp<"(>= 15 14 15)"> = [`prim`, false]

const maintest11_lte_0: Lisp<"(<= 15 14)"> = [`prim`, false]
const maintest11_lte_1: Lisp<"(<= 15 14 13)"> = [`prim`, false]
const maintest11_lte_2: Lisp<"(<= 15 15 14)"> = [`prim`, false]
const maintest11_lte_0_1: Lisp<"(<= 14 15)"> = [`prim`, true]
const maintest11_lte_1_1: Lisp<"(<= 13 14 15)"> = [`prim`, true]
const maintest11_lte_2_1: Lisp<"(<= 15 15 15)"> = [`prim`, true]
const maintest11_lte_2_2: Lisp<"(<= 14 15 15)"> = [`prim`, true]
const maintest11_lte_2_3: Lisp<"(<= 14 14 15)"> = [`prim`, true]

// math operators: +, -, *, /, %, mod.

const maintest0_add: Lisp<"(+ 1 1)"> = ['prim', "0000000000000010"]
const maintest1_add: Lisp<"(+ 1 0)"> = ['prim', "0000000000000001"]
const maintest2_add: Lisp<"(+ 0 0)"> = ['prim', "0000000000000000"]
const maintest3_add: Lisp<"(+ 4 5)"> = ['prim', "0000000000001001"]
const maintest4_add: Lisp<"(+ 5 4 6)"> = ['prim', "0000000000001111"]
const maintest5_add: Lisp<"(+ 5 4 0)"> = ['prim', "0000000000001001"]
const maintest6_add: Lisp<"(+ 5 0 4)"> = ['prim', "0000000000001001"]
const maintest7_add: Lisp<"(+ 0 4 5)"> = ['prim', "0000000000001001"]
const maintest8_add: Lisp<"(+ -1 1)"> = ['prim', "0000000000000000"]
const maintest9_add: Lisp<"(+ -2 -2)"> = ['prim', "1111111111111100"]

const maintest0_sub: Lisp<"(- 4 1)"> = ['prim', "0000000000000011"]
const maintest1_sub: Lisp<"(- 1 0)"> = ['prim', "0000000000000001"]
const maintest2_sub: Lisp<"(- 0 0)"> = ['prim', "0000000000000000"]
const maintest3_sub: Lisp<"(- 0 1)"> = ['prim', "1111111111111111"]
const maintest4_sub: Lisp<"(- 10 1)"> = ['prim', "0000000000001001"]
const maintest5_sub: Lisp<"(- 10 -9)"> = ['prim', "0000000000010011"]

const maintest0_mul: Lisp<"(* 3 1)"> = ['prim', "0000000000000011"]
const maintest1_mul: Lisp<"(* 0 1)"> = ['prim', "0000000000000000"]
const maintest2_mul: Lisp<"(* 0 0)"> = ['prim', "0000000000000000"]
const maintest3_mul: Lisp<"(* 2 3)"> = ['prim', "0000000000000110"]
const maintest4_mul: Lisp<"(* (- 2 3) 3)"> = ['prim', "1111111111111101"]
const maintest5_mul: Lisp<"(* (- 0 2) 3)"> = ['prim', "1111111111111010"]
const maintest6_mul: Lisp<"(* -2 3)"> = ['prim', "1111111111111010"]

const maintest0_div: Lisp<"(/ 4 1)"> = ['prim', "0000000000000100"]
const maintest1_div: Lisp<"(/ 1 0)"> = ['prim', "nil"]
const maintest2_div: Lisp<"(/ 0 0)"> = ['prim', "nil"]
const maintest3_div: Lisp<"(/ 4 4)"> = ['prim', "0000000000000001"]
const maintest4_div: Lisp<"(/ 4 2)"> = ['prim', "0000000000000010"]
const maintest5_div: Lisp<"(/ 4 (- 0 2))"> = ['prim', "1111111111111110"]
const maintest6_div: Lisp<"(/ (- 2 6) (- 0 2))"> = ['prim', "0000000000000010"]
const maintest7_div: Lisp<"(/ (- 2 6) -2)"> = ['prim', "0000000000000010"]

const maintest0_mod: Lisp<"(mod 2 5)"> = ['prim', '0000000000000010']
const maintest1_mod: Lisp<"(mod 5 5)"> = ['prim', '0000000000000000']
const maintest2_mod: Lisp<"(mod 7 6)"> = ['prim', '0000000000000001']
const maintest3_mod: Lisp<"(mod 14 6)"> = ['prim', '0000000000000010']
const maintest4_mod: Lisp<"(mod 6 0)"> = ['prim', 'nil']
const maintest5_mod: Lisp<"(mod -1 2)"> = ['prim', '0000000000000001']
const maintest6_mod: Lisp<"(mod (- 0 1) 2)"> = ['prim', '0000000000000001']
const maintest7_mod: Lisp<"(mod 6 1)"> = ['prim', '0000000000000000']
const maintest8_mod: Lisp<"(mod -17 5)"> = ['prim', '0000000000000011']

const maintest0_mod1: Lisp<"(% 2 5)"> = ['prim', '0000000000000010']
const maintest1_mod1: Lisp<"(% 5 5)"> = ['prim', '0000000000000000']
const maintest2_mod1: Lisp<"(% 7 6)"> = ['prim', '0000000000000001']
const maintest3_mod1: Lisp<"(% 14 6)"> = ['prim', '0000000000000010']
const maintest4_mod1: Lisp<"(% 6 0)"> = ['prim', 'nil']
const maintest5_mod1: Lisp<"(% -1 2)"> = ['prim', '0000000000000001']
const maintest6_mod1: Lisp<"(% (- 0 1) 2)"> = ['prim', '0000000000000001']
const maintest7_mod1: Lisp<"(% 6 1)"> = ['prim', '0000000000000000']
const maintest8_mod1: Lisp<"(% -17 5)"> = ['prim', '0000000000000011']

// structure: map, vec, list.
const maintest12_get_0: Lisp<"(:a {:a 1})"> = ['prim', '0000000000000001']
const maintest12_get_1: Lisp<"(:a {:a 1 :b 2})"> = ['prim', '0000000000000001']
const maintest12_get_2: Lisp<"({:a 1 :b 2} :a)"> = ['prim', '0000000000000001']
const maintest12_get_3: Lisp<"(:c {:a 1 :b 2})"> = []
const maintest12_get_4: Lisp<"({:a 1 :b 2} :c)"> = []
const maintest12_get_5: Lisp<"(get {:a 1 :b 2} :a)"> = ['prim', '0000000000000001']

const maintest0_mapst_0: Lisp<"{:a 1}"> = ['map', [['key', ':a'], ['prim', '0000000000000001']]]

const maintest0_vecst_0: Lisp<"[0 1 2 3]"> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']]
const maintest1_vecst_0: Lisp<"(vector 0 1 2 3)"> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']]

// arrow macro: ->, ->>

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



