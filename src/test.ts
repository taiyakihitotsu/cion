import type Cion from './index'

// todo :
// string split works but not correctly, in current.
// Use _ as space until I will have implemented a string parser. 
const maintest4: Cion.RawLisp<"(let [a 'a'] (if (eq a 'a') 'this_is_true', 'this_is_false')"> = [`prim`, "'this_is_false'"]
const maintest5: Cion.RawLisp<"(if true 1 2)"> = ['prim', '0000000000000001']
const maintest6: Cion.RawLisp<"(if true (let [a 'astr' b 'bstr'] (str a b)) 11)"> = ['prim', `'astrbstr'`]

// boolean operators: and, or, not.

const maintest7_and: Cion.RawLisp<"(and true true)"> = ['prim', true]
const maintest8_and: Cion.RawLisp<"(and true false)"> = ['prim', false]
const maintest9_and: Cion.RawLisp<"(and false false)"> = ['prim', false]
const maintest10_and: Cion.RawLisp<"(and false true)"> = ['prim', false]
const maintest7_1_and: Cion.RawLisp<"(and true true true)"> = ['prim', true]
const maintest8_1_and: Cion.RawLisp<"(and false true false)"> = ['prim', false]
const maintest9_1_and: Cion.RawLisp<"(and false false false)"> = ['prim', false]
const maintest10_1_and: Cion.RawLisp<"(and false true true)"> = ['prim', false]

const maintest7_or: Cion.RawLisp<"(or true true)"> = ['prim', true]
const maintest8_or: Cion.RawLisp<"(or true false)"> = ['prim', true]
const maintest9_or: Cion.RawLisp<"(or false false)"> = ['prim', false]
const maintest10_or: Cion.RawLisp<"(or false true)"> = ['prim', true]
const maintest7_1_or: Cion.RawLisp<"(or true true true)"> = ['prim', true]
const maintest8_1_or: Cion.RawLisp<"(or false true false)"> = ['prim', true]
const maintest9_1_or: Cion.RawLisp<"(or false false false)"> = ['prim', false]
const maintest10_1_or: Cion.RawLisp<"(or false true true)"> = ['prim', true]

const maintest0_not: Cion.RawLisp<"(not true)"> = ['prim', false]
const maintest1_not: Cion.RawLisp<"(not false)"> = ['prim', true]

// order tests: >, <, >=, <=, =, eq.

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

const maintest11_gt_0: Cion.RawLisp<"(> 15 14)"> = [`prim`, true]
const maintest11_gt_1: Cion.RawLisp<"(> 15 14 13)"> = [`prim`, true]
const maintest11_gt_2: Cion.RawLisp<"(> 15 14 16)"> = [`prim`, false]

const maintest11_lt_0: Cion.RawLisp<"(< 15 14)"> = [`prim`, false]
const maintest11_lt_1: Cion.RawLisp<"(< 15 14 13)"> = [`prim`, false]
const maintest11_lt_0_1: Cion.RawLisp<"(< 14 15)"> = [`prim`, true]
const maintest11_lt_1_1: Cion.RawLisp<"(< 13 14 15)"> = [`prim`, true]
const maintest11_lt_2: Cion.RawLisp<"(< 13 15 14)"> = [`prim`, false]
const maintest11_lt_3: Cion.RawLisp<"(< 15 14 13)"> = [`prim`, false]

const maintest11_gte_0: Cion.RawLisp<"(>= 15 14)"> = [`prim`, true]
const maintest11_gte_1: Cion.RawLisp<"(>= 15 14 13)"> = [`prim`, true]
const maintest11_gte_2: Cion.RawLisp<"(>= 13 14 15)"> = [`prim`, false]
const maintest11_gte_0_1: Cion.RawLisp<"(>= 14 15)"> = [`prim`, false]
const maintest11_gte_1_1: Cion.RawLisp<"(>= 13 14 15)"> = [`prim`, false]
const maintest11_gte_2_1: Cion.RawLisp<"(>= 15 15 15)"> = [`prim`, true]
const maintest11_gte_2_2: Cion.RawLisp<"(>= 15 15 14)"> = [`prim`, true]
const maintest11_gte_2_3: Cion.RawLisp<"(>= 15 14 14)"> = [`prim`, true]
const maintest11_gte_2_4: Cion.RawLisp<"(>= 15 14 15)"> = [`prim`, false]

const maintest11_lte_0: Cion.RawLisp<"(<= 15 14)"> = [`prim`, false]
const maintest11_lte_1: Cion.RawLisp<"(<= 15 14 13)"> = [`prim`, false]
const maintest11_lte_2: Cion.RawLisp<"(<= 15 15 14)"> = [`prim`, false]
const maintest11_lte_0_1: Cion.RawLisp<"(<= 14 15)"> = [`prim`, true]
const maintest11_lte_1_1: Cion.RawLisp<"(<= 13 14 15)"> = [`prim`, true]
const maintest11_lte_2_1: Cion.RawLisp<"(<= 15 15 15)"> = [`prim`, true]
const maintest11_lte_2_2: Cion.RawLisp<"(<= 14 15 15)"> = [`prim`, true]
const maintest11_lte_2_3: Cion.RawLisp<"(<= 14 14 15)"> = [`prim`, true]

// math operators: +, -, *, /, %, mod.

const maintest0_add: Cion.RawLisp<"(+ 1 1)"> = ['prim', "0000000000000010"]
const maintest1_add: Cion.RawLisp<"(+ 1 0)"> = ['prim', "0000000000000001"]
const maintest2_add: Cion.RawLisp<"(+ 0 0)"> = ['prim', "0000000000000000"]
const maintest3_add: Cion.RawLisp<"(+ 4 5)"> = ['prim', "0000000000001001"]
const maintest4_add: Cion.RawLisp<"(+ 5 4 6)"> = ['prim', "0000000000001111"]
const maintest5_add: Cion.RawLisp<"(+ 5 4 0)"> = ['prim', "0000000000001001"]
const maintest6_add: Cion.RawLisp<"(+ 5 0 4)"> = ['prim', "0000000000001001"]
const maintest7_add: Cion.RawLisp<"(+ 0 4 5)"> = ['prim', "0000000000001001"]
const maintest8_add: Cion.RawLisp<"(+ -1 1)"> = ['prim', "0000000000000000"]
const maintest9_add: Cion.RawLisp<"(+ -2 -2)"> = ['prim', "1111111111111100"]

const maintest0_sub: Cion.RawLisp<"(- 4 1)"> = ['prim', "0000000000000011"]
const maintest1_sub: Cion.RawLisp<"(- 1 0)"> = ['prim', "0000000000000001"]
const maintest2_sub: Cion.RawLisp<"(- 0 0)"> = ['prim', "0000000000000000"]
const maintest3_sub: Cion.RawLisp<"(- 0 1)"> = ['prim', "1111111111111111"]
const maintest4_sub: Cion.RawLisp<"(- 10 1)"> = ['prim', "0000000000001001"]
const maintest5_sub: Cion.RawLisp<"(- 10 -9)"> = ['prim', "0000000000010011"]

const maintest0_mul: Cion.RawLisp<"(* 3 1)"> = ['prim', "0000000000000011"]
const maintest1_mul: Cion.RawLisp<"(* 0 1)"> = ['prim', "0000000000000000"]
const maintest2_mul: Cion.RawLisp<"(* 0 0)"> = ['prim', "0000000000000000"]
const maintest3_mul: Cion.RawLisp<"(* 2 3)"> = ['prim', "0000000000000110"]
const maintest4_mul: Cion.RawLisp<"(* (- 2 3) 3)"> = ['prim', "1111111111111101"]
const maintest5_mul: Cion.RawLisp<"(* (- 0 2) 3)"> = ['prim', "1111111111111010"]
const maintest6_mul: Cion.RawLisp<"(* -2 3)"> = ['prim', "1111111111111010"]

const maintest0_div: Cion.RawLisp<"(/ 4 1)"> = ['prim', "0000000000000100"]
const maintest1_div: Cion.RawLisp<"(/ 1 0)"> = ['prim', "nil"]
const maintest2_div: Cion.RawLisp<"(/ 0 0)"> = ['prim', "nil"]
const maintest3_div: Cion.RawLisp<"(/ 4 4)"> = ['prim', "0000000000000001"]
const maintest4_div: Cion.RawLisp<"(/ 4 2)"> = ['prim', "0000000000000010"]
const maintest5_div: Cion.RawLisp<"(/ 4 (- 0 2))"> = ['prim', "1111111111111110"]
const maintest6_div: Cion.RawLisp<"(/ (- 2 6) (- 0 2))"> = ['prim', "0000000000000010"]
const maintest7_div: Cion.RawLisp<"(/ (- 2 6) -2)"> = ['prim', "0000000000000010"]

const maintest0_mod: Cion.RawLisp<"(mod 2 5)"> = ['prim', '0000000000000010']
const maintest1_mod: Cion.RawLisp<"(mod 5 5)"> = ['prim', '0000000000000000']
const maintest2_mod: Cion.RawLisp<"(mod 7 6)"> = ['prim', '0000000000000001']
const maintest3_mod: Cion.RawLisp<"(mod 14 6)"> = ['prim', '0000000000000010']
const maintest4_mod: Cion.RawLisp<"(mod 6 0)"> = ['prim', 'nil']
const maintest5_mod: Cion.RawLisp<"(mod -1 2)"> = ['prim', '0000000000000001']
const maintest6_mod: Cion.RawLisp<"(mod (- 0 1) 2)"> = ['prim', '0000000000000001']
const maintest7_mod: Cion.RawLisp<"(mod 6 1)"> = ['prim', '0000000000000000']
const maintest8_mod: Cion.RawLisp<"(mod -17 5)"> = ['prim', '0000000000000011']

const maintest0_mod1: Cion.RawLisp<"(% 2 5)"> = ['prim', '0000000000000010']
const maintest1_mod1: Cion.RawLisp<"(% 5 5)"> = ['prim', '0000000000000000']
const maintest2_mod1: Cion.RawLisp<"(% 7 6)"> = ['prim', '0000000000000001']
const maintest3_mod1: Cion.RawLisp<"(% 14 6)"> = ['prim', '0000000000000010']
const maintest4_mod1: Cion.RawLisp<"(% 6 0)"> = ['prim', 'nil']
const maintest5_mod1: Cion.RawLisp<"(% -1 2)"> = ['prim', '0000000000000001']
const maintest6_mod1: Cion.RawLisp<"(% (- 0 1) 2)"> = ['prim', '0000000000000001']
const maintest7_mod1: Cion.RawLisp<"(% 6 1)"> = ['prim', '0000000000000000']
const maintest8_mod1: Cion.RawLisp<"(% -17 5)"> = ['prim', '0000000000000011']

// structure: map, vec, list.
const maintest12_get_0: Cion.RawLisp<"(:a {:a 1})"> = ['prim', '0000000000000001']
const maintest12_get_1: Cion.RawLisp<"(:a {:a 1 :b 2})"> = ['prim', '0000000000000001']
const maintest12_get_2: Cion.RawLisp<"({:a 1 :b 2} :a)"> = ['prim', '0000000000000001']
const maintest12_get_3: Cion.RawLisp<"(:c {:a 1 :b 2})"> = []
const maintest12_get_4: Cion.RawLisp<"({:a 1 :b 2} :c)"> = []
const maintest12_get_5: Cion.RawLisp<"(get {:a 1 :b 2} :a)"> = ['prim', '0000000000000001']

const maintest0_mapst_0: Cion.RawLisp<"{:a 1}"> = ['map', [['key', ':a'], ['prim', '0000000000000001']]]

const maintest0_vecst_0: Cion.RawLisp<"[0 1 2 3]"> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']]
const maintest1_vecst_0: Cion.RawLisp<"(vector 0 1 2 3)"> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']]

// form macro: if, let.

const maintest0_letif_0: Cion.RawLisp<'(let [x (fn [a b] (+ a b)) y 8] (+ 2 1))'> = ['prim', '0000000000000011']
const maintest0_letif_01: Cion.RawLisp<'(let [x (fn [a b] (+ a b)) y 8] (+ y 2 1))'> = ['prim', '0000000000001011']
const maintest0_letif_02: Cion.RawLisp<'(let [x (fn [a] (+ 1 a))] (x 1 1))'> = ['prim', '0000000000000010']
// const testletfn2: Eval<['let', [['sym', 'x'], ['fn', [['sym', 'a']], [['sym', '+'], ['prim', '0000000000000001'], ['sym', 'a']]]], [['sym', 'x'], ['prim', '0000000000000010'], ['prim', '0000000000000001']]]> = ['prim', '0000000000000011']
const maintest0_letif_03: Cion.RawLisp<'(let [x (fn [a] (+ 1 a)) y 9] (x 1 1))'> = ['prim', '0000000000000010']
const maintest0_letif_04: Cion.RawLisp<'(let [x (fn [a] (+ 1 a)) y 9] (x y 1))'> = ['prim', '0000000000001010']
const maintest0_letif_05: Cion.RawLisp<'(let [x (fn [a b] (+ 1 a)) y 9] (x 1 1))'> = ['prim', '0000000000000010']
const maintest0_letif_06: Cion.RawLisp<'(let [x (fn [a b] (+ b a)) y 9] (x 1 2))'> = ['prim', '0000000000000011']
const maintest0_letif_07: Cion.RawLisp<'((fn [a b] (+ b a)) 1 2)'> = ['prim', '0000000000000011']
const maintest0_letif_1: Cion.RawLisp<'(let [x ((fn [a] (+ a 1)) 1)] (* 2 x))'> = ['prim', '0000000000000100'] 
const maintest0_letif_2: Cion.RawLisp<'(let [x ((fn [a b] (+ a 1 b)) 1 8)] (* 2 x))'> = ['prim', '0000000000010100']
const maintest0_letif_3: Cion.RawLisp<'(let [x 8 y ((fn [a b] (+ a x b)) 1 8)] (* 2 y))'> = ['prim', '0000000000100010']
const maintest0_letif_4: Cion.RawLisp<'(let [x (let [a 2 b 6] (+ a b)) y ((fn [a b] (+ a x b)) 1 8)] (* 2 y))'> = ['prim', '0000000000100010']
const maintest0_letif_5: Cion.RawLisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) y 8] x)'> = ['fn', [['sym', 'c']], [['sym', '+'], ['prim', '0000000000000010'], ['prim', '0000000000000110'], ['sym', 'c']]]
const maintest0_letif_6: Cion.RawLisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c)))] (x 10))'> = ['prim', '0000000000010010']
const maintest0_letif_7: Cion.RawLisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) y ((fn [d] (x 2 d)) 10)] y)'> = ['prim', '0000000000001010']
const maintest0_letif_8: Cion.RawLisp<'(let [z 2 y ((fn [d] (+ z d)) 10)] y)'> = ['prim', '0000000000001100']
const maintest0_letif_9: Cion.RawLisp<'(let [z 2 y ((fn [d] (+ z d)) 10) x 10] y)'> = ['prim', '0000000000001100']
const maintest0_letif_10: Cion.RawLisp<'(let [z 2 y 3 x 10] y)'> = ['prim', '0000000000000011']
const maintest0_letif_11: Cion.RawLisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) z 2 y ((fn [d] (x z d)) 10)] (x z))'> = ['prim', '0000000000001010']
const maintest0_letif_12: Cion.RawLisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) z 20 y ((fn [d] (x d)) z)] y)'> = ['prim', '0000000000011100']
const maintest0_letif_13: Cion.RawLisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) z 20 y ((fn [d] (x d)) z 999)] y)'> = ['prim', '0000000000011100'] // a case of a number of args being over. rest parts are ignored.

// lift: map, filter, remove, reduce.

const maintest0_map_0: Cion.RawLisp<'(map (fn [n] (* 2 n)) [0 1 2])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000010'], ['prim', '0000000000000100']]
const maintest0_map_1: Cion.RawLisp<'(let [f (fn [n] (* 2 n))] (map f [0 1 2]))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000010'], ['prim', '0000000000000100']]

// doing
const maintest0_filter_0: Cion.RawLisp<'(filter (fn [n] (> 3 n)) [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
const maintest0_filter_1: Cion.RawLisp<'(let [f (fn [n] (> 3 n))] (filter f [0 1 2 3 4 5]))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
 
const maintest0_remove_0: Cion.RawLisp<'(remove (fn [n] (> 3 n)) [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']]
const maintest0_remove_1: Cion.RawLisp<'(let [f (fn [n] (> 3 n))] (remove f [0 1 2 3 4 5]))'> = ['vec', ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']]

const maintest0_reduce_0: Cion.RawLisp<'(reduce (fn [r i] (+ r i)) 0 [0 1 2 3 4 5])'> = ['prim', '0000000000001111']
const maintest0_reduce_1: Cion.RawLisp<'(let [f (fn [r i] (+ r i))] (reduce f 0 [0 1 2 3 4 5]))'> = ['prim', '0000000000001111']

// loop
const maintest0_loop_1: Cion.RawLisp<'(let [f (fn [i] (if (> i 0) (f (- i 2)) true))] (f 10))'> = ['prim', true] 

// rest, butlast, interleave, take, drop
const maintest0_rest_0: Cion.RawLisp<'(rest [0 1 2 3])'> = ['vec', ['prim', '0000000000000001'], ['prim', '0000000000000010'],['prim', '0000000000000011']]
const maintest0_rest_1: Cion.RawLisp<'(rest [0])'> = ['vec']

const maintest0_butlast_0: Cion.RawLisp<'(butlast [0 1 2 3])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
const maintest0_butlast_1: Cion.RawLisp<'(butlast [0])'> = ['vec']

const maintest0_reverse_0: Cion.RawLisp<'(reverse [0 1 2 3])'> = ['vec', ['prim', '0000000000000011'], ['prim', '0000000000000010'], ['prim', '0000000000000001'],  ['prim', '0000000000000000']]
const maintest0_reverse_1: Cion.RawLisp<'(reverse [0])'> = ['vec',  ['prim', '0000000000000000']]

const maintest0_interleave_0: Cion.RawLisp<'(interleave [0 0 0] [1 1 1])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000000'], ['prim', '0000000000000001']]
const maintest0_interleave_1: Cion.RawLisp<'(interleave [0 0 0] [1 1])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000000'], ['prim', '0000000000000001']]

const maintest0_take_0: Cion.RawLisp<'(take 0 [0 1 2 3 4 5])'> = ['vec']
const maintest0_take_1: Cion.RawLisp<'(take 2 [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001']]
const maintest0_take_2: Cion.RawLisp<'(take 9 [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']]

const maintest0_drop_0: Cion.RawLisp<'(drop 9 [0 1 2 3 4 5])'> = ['vec']
const maintest0_drop_1: Cion.RawLisp<'(drop 2 [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']]
const maintest0_drop_2: Cion.RawLisp<'(drop 0 [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']]

// getter: first, last.
const maintest0_first_0: Cion.RawLisp<'(first [0 1 2])'> = ['prim', '0000000000000000']
const maintest0_first_1: Cion.RawLisp<'(first [])'> = []

const maintest0_last_0: Cion.RawLisp<'(last [0 1 2])'> = ['prim', '0000000000000010']
const maintest0_last_1: Cion.RawLisp<'(last [])'> = []

// conj, concat.
const maintest0_conj_0: Cion.RawLisp<'(conj [0 1] 2 3)'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']]
const maintest0_conj_1: Cion.RawLisp<'(conj [0 1] [2])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000000000010']]]

// assoc, update, assoc-in, update-in
const maintest0_assoc_0: Cion.RawLisp<'(assoc [0 1 2] 0 99)'> = ['vec', ['prim', '0000000001100011'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
const maintest0_assoc_1: Cion.RawLisp<'(assoc [0 1 2] 3 99)'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']] // no effect

const maintest0_update_0: Cion.RawLisp<'(update [0 1 2] 1 (fn [x] (+ x 99)))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000001100100'], ['prim', '0000000000000010']]
const maintest0_update_1: Cion.RawLisp<'(update [0 1 2] 99 (fn [x] (+ x 99)))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']] // no effect

const maintest0_assocIn_0: Cion.RawLisp<'(assoc-in [0 1 2] [0] 99)'> = ['vec', ['prim', '0000000001100011'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
const maintest0_assocIn_1: Cion.RawLisp<'(assoc-in [0 1 2] [99] 99)'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']] // no effect
const maintest0_assocIn_2: Cion.RawLisp<'(assoc-in [0 1 2] [0 0] 99)'> = {error: "AssocInError8", message: "Keys rests but its value is not vector nor map."}
const maintest0_assocIn_3: Cion.RawLisp<'(assoc-in [0 1 [2 3 4]] [2 0] 99)'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000001100011'], ['prim', '0000000000000011'], ['prim', '0000000000000100']]]
const maintest0_assocIn_4: Cion.RawLisp<'(assoc-in [0 1 [2 3 {:a 4 :b 5}]] [2 0 :a] 99)'> = {error: 'AssocInError7', message: `The value of key (0000000000000000) is not vector nor map.`}
const maintest0_assocIn_5: Cion.RawLisp<'(assoc-in [0 1 [2 3 {:a 4 :b 5}]] [2 2 :a] 99)'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['map', [['key', ':a'], ['prim', '0000000001100011'], ['key', ':b'], ['prim', '0000000000000101']]]]]
const maintest0_assocIn_6: Cion.RawLisp<'(assoc-in {:x [0 1 [2 3 {:a 4 :b 5}]] :y 0} [:x 2 2 :a] 99)'> = ['map', [['key', ':x'], ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['map', [['key', ':a'], ['prim', '0000000001100011'], ['key', ':b'], ['prim', '0000000000000101']]]]], ['key', ':y'], ['prim', '0000000000000000']]]

const maintest0_updateIn_0: Cion.RawLisp<'(update-in [0 1 2] [1] (fn [x] (+ x 99)))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000001100100'], ['prim', '0000000000000010']]
const maintest0_updateIn_1: Cion.RawLisp<'(update-in [0 1 2] [99] (fn [x] (+ x 99)))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']] // no effect
const maintest0_updateIn_2: Cion.RawLisp<'(update-in [0 1 2] [99 99] (fn [x] (+ x 99)))'> = {error: "AssocInError8", message: "Keys rests but its value is not vector nor map."}

// arrow macro: ->, ->>.

const maintest_threadf_0: Cion.RawLisp<"(-> 's' (str '01'))"> = ['prim', "'s01'"]
const maintest_threadf_1: Cion.RawLisp<"(-> 'a' (str '01') (str 's'))"> = ['prim', "'a01s'"]
const maintest_threadf_2: Cion.RawLisp<"(str 'a' (str '01' 's'))"> = ['prim', "'a01s'"]
const maintest_threadf_3: Cion.RawLisp<"(-> 1 (+ 1) (+ 2))"> = ['prim', '0000000000000100']
const maintest_threadf_4: Cion.RawLisp<"(+ 2 (+ 1 1))"> = ['prim', '0000000000000100']

const maintest_threadl_0: Cion.RawLisp<"(->> 's' (str '01'))"> = ['prim', "'01s'"]
const maintest_threadl_1: Cion.RawLisp<"(->> 'a' (str '01') (str 's'))"> = ['prim', "'s01a'"]
const maintest_threadl_2: Cion.RawLisp<"(str 'a' (str '01' 's'))"> = ['prim', "'a01s'"]
const maintest_threadl_3: Cion.RawLisp<"(->> 1 (+ 1) (+ 2))"> = ['prim', '0000000000000100']
const maintest_threadl_4: Cion.RawLisp<"(+ 2 (+ 1 1))"> = ['prim', '0000000000000100']

// let test
const maintest_let_0: Cion.RawLisp<'(let [a {:a "a"}] (+ 1 2))'> = ['prim', '0000000000000011']

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
