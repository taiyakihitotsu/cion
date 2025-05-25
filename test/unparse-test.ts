import type Cion from '../src/index'

// ---------------------
// -- Unparse tests
// ---------------------


const unparse_maintest4: Cion.Lisp<"(let [a 'a'] (if (eq a 'a') 'this_is_true', 'this_is_false')"> = "'this_is_false'"
const unparse_maintest5: Cion.Lisp<"(if true 1 2)"> = '1'
const unparse_maintest6: Cion.Lisp<"(if true (let [a 'astr' b 'bstr'] (str a b)) 11)"> = `'astrbstr'`

// boolean operators: and, or, not.

const unparse_maintest7_and: Cion.Lisp<"(and true true)"> = 'true'
const unparse_maintest8_and: Cion.Lisp<"(and true false)"> = 'false'
const unparse_maintest9_and: Cion.Lisp<"(and false false)"> = 'false'
const unparse_maintest10_and: Cion.Lisp<"(and false true)"> = 'false'
const unparse_maintest7_1_and: Cion.Lisp<"(and true true true)"> = 'true'
const unparse_maintest8_1_and: Cion.Lisp<"(and false true false)"> = 'false'
const unparse_maintest9_1_and: Cion.Lisp<"(and false false false)"> = 'false'
const unparse_maintest10_1_and: Cion.Lisp<"(and false true true)"> = 'false'

const unparse_maintest7_or: Cion.Lisp<"(or true true)"> = 'true'
const unparse_maintest8_or: Cion.Lisp<"(or true false)"> = 'true'
const unparse_maintest9_or: Cion.Lisp<"(or false false)"> = 'false'
const unparse_maintest10_or: Cion.Lisp<"(or false true)"> = 'true'
const unparse_maintest7_1_or: Cion.Lisp<"(or true true true)"> = 'true'
const unparse_maintest8_1_or: Cion.Lisp<"(or false true false)"> = 'true'
const unparse_maintest9_1_or: Cion.Lisp<"(or false false false)"> = 'false'
const unparse_maintest10_1_or: Cion.Lisp<"(or false true true)"> = 'true'

const unparse_maintest0_not: Cion.Lisp<"(not true)"> = 'false'
const unparse_maintest1_not: Cion.Lisp<"(not false)"> = 'true'

// order tests: >, <, >=, <=, =, eq.

const unparse_maintest0_eq: Cion.Lisp<"(eq 'a' 'b')"> = 'false'
const unparse_maintest1_eq: Cion.Lisp<"(eq 'a' 'a')"> = 'true'
const unparse_maintest2_eq: Cion.Lisp<"(let [a 'a'] (eq a 'a'))"> = 'true'
const unparse_maintest3_eq: Cion.Lisp<"(let [a 'b'] (eq a 'a'))"> = 'false'

const unparse_maintest0a_eq: Cion.Lisp<"(eq 1 1)"> = 'true'
const unparse_maintest1a_eq: Cion.Lisp<"(eq 1 2)"> = 'false'
const unparse_maintest2a_eq: Cion.Lisp<"(let [a 1] (eq a 1))"> = 'true'
const unparse_maintest3a_eq: Cion.Lisp<"(let [a 2] (eq a 1))"> = 'false'

const unparse_maintest0_eq1: Cion.Lisp<"(= 'a' 'b')"> = 'false'
const unparse_maintest1_eq1: Cion.Lisp<"(= 'a' 'a')"> = 'true'
const unparse_maintest2_eq1: Cion.Lisp<"(let [a 'a'] (= a 'a'))"> = 'true'
const unparse_maintest3_eq1: Cion.Lisp<"(let [a 'b'] (= a 'a'))"> = 'false'

const unparse_maintest0a_eq1: Cion.Lisp<"(= 1 1)"> = 'true'
const unparse_maintest1a_eq1: Cion.Lisp<"(= 1 2)"> = 'false'
const unparse_maintest2a_eq1: Cion.Lisp<"(let [a 1] (= a 1))"> = 'true'
const unparse_maintest3a_eq1: Cion.Lisp<"(let [a 2] (= a 1))"> = 'false'

const unparse_maintest11_gt_0: Cion.Lisp<"(> 15 14)"> = 'true'
const unparse_maintest11_gt_1: Cion.Lisp<"(> 15 14 13)"> = 'true'
const unparse_maintest11_gt_2: Cion.Lisp<"(> 15 14 16)"> = 'false'

const unparse_maintest11_lt_0: Cion.Lisp<"(< 15 14)"> = 'false'
const unparse_maintest11_lt_1: Cion.Lisp<"(< 15 14 13)"> = 'false'
const unparse_maintest11_lt_0_1: Cion.Lisp<"(< 14 15)"> = 'true'
const unparse_maintest11_lt_1_1: Cion.Lisp<"(< 13 14 15)"> = 'true'
const unparse_maintest11_lt_2: Cion.Lisp<"(< 13 15 14)"> = 'false'
const unparse_maintest11_lt_3: Cion.Lisp<"(< 15 14 13)"> = 'false'

const unparse_maintest11_gte_0: Cion.Lisp<"(>= 15 14)"> = 'true'
const unparse_maintest11_gte_1: Cion.Lisp<"(>= 15 14 13)"> = 'true'
const unparse_maintest11_gte_2: Cion.Lisp<"(>= 13 14 15)"> = 'false'
const unparse_maintest11_gte_0_1: Cion.Lisp<"(>= 14 15)"> = 'false'
const unparse_maintest11_gte_1_1: Cion.Lisp<"(>= 13 14 15)"> = 'false'
const unparse_maintest11_gte_2_1: Cion.Lisp<"(>= 15 15 15)"> = 'true'
const unparse_maintest11_gte_2_2: Cion.Lisp<"(>= 15 15 14)"> = 'true'
const unparse_maintest11_gte_2_3: Cion.Lisp<"(>= 15 14 14)"> = 'true'
const unparse_maintest11_gte_2_4: Cion.Lisp<"(>= 15 14 15)"> = 'false'

const unparse_maintest11_lte_0: Cion.Lisp<"(<= 15 14)"> = 'false'
const unparse_maintest11_lte_1: Cion.Lisp<"(<= 15 14 13)"> = 'false'
const unparse_maintest11_lte_2: Cion.Lisp<"(<= 15 15 14)"> = 'false'
const unparse_maintest11_lte_0_1: Cion.Lisp<"(<= 14 15)"> = 'true'
const unparse_maintest11_lte_1_1: Cion.Lisp<"(<= 13 14 15)"> = 'true'
const unparse_maintest11_lte_2_1: Cion.Lisp<"(<= 15 15 15)"> = 'true'
const unparse_maintest11_lte_2_2: Cion.Lisp<"(<= 14 15 15)"> = 'true'
const unparse_maintest11_lte_2_3: Cion.Lisp<"(<= 14 14 15)"> = 'true'

// math operators: +, -, *, /, %, mod.

const unparse_maintest0_add: Cion.Lisp<"(+ 1 1)"> = '2'
const unparse_maintest1_add: Cion.Lisp<"(+ 1 0)"> = '1'
const unparse_maintest2_add: Cion.Lisp<"(+ 0 0)"> = '0'
const unparse_maintest3_add: Cion.Lisp<"(+ 4 5)"> = '9'
const unparse_maintest4_add: Cion.Lisp<"(+ 5 4 6)"> = '15'
const unparse_maintest5_add: Cion.Lisp<"(+ 5 4 0)"> = '9'
const unparse_maintest6_add: Cion.Lisp<"(+ 5 0 4)"> = '9'
const unparse_maintest7_add: Cion.Lisp<"(+ 0 4 5)"> = '9'
const unparse_maintest8_add: Cion.Lisp<"(+ -1 1)"> = '0'
const unparse_maintest9_add: Cion.Lisp<"(+ -2 -2)"> = '-4'

const unparse_maintest0_sub: Cion.Lisp<"(- 4 1)"> = '3'
const unparse_maintest1_sub: Cion.Lisp<"(- 1 0)"> = '1'
const unparse_maintest2_sub: Cion.Lisp<"(- 0 0)"> = '0'
const unparse_maintest3_sub: Cion.Lisp<"(- 0 1)"> = '-1'
const unparse_maintest4_sub: Cion.Lisp<"(- 10 1)"> = '9'
const unparse_maintest5_sub: Cion.Lisp<"(- 10 -9)"> = '19'

const unparse_maintest0_mul: Cion.Lisp<"(* 3 1)"> = '3'
const unparse_maintest1_mul: Cion.Lisp<"(* 0 1)"> = '0'
const unparse_maintest2_mul: Cion.Lisp<"(* 0 0)"> = '0'
const unparse_maintest3_mul: Cion.Lisp<"(* 2 3)"> = '6'
const unparse_maintest4_mul: Cion.Lisp<"(* (- 2 3) 3)"> = '-3'
const unparse_maintest5_mul: Cion.Lisp<"(* (- 0 2) 3)"> = '-6'
const unparse_maintest6_mul: Cion.Lisp<"(* -2 3)"> = '-6'

const unparse_maintest0_div: Cion.Lisp<"(/ 4 1)"> = '4'
const unparse_maintest1_div: Cion.Lisp<"(/ 1 0)"> = 'nil'
const unparse_maintest2_div: Cion.Lisp<"(/ 0 0)"> = 'nil'
const unparse_maintest3_div: Cion.Lisp<"(/ 4 4)"> = '1'
const unparse_maintest4_div: Cion.Lisp<"(/ 4 2)"> = '2'
const unparse_maintest5_div: Cion.Lisp<"(/ 4 (- 0 2))"> = '-2'
const unparse_maintest6_div: Cion.Lisp<"(/ (- 2 6) (- 0 2))"> = '2'
const unparse_maintest7_div: Cion.Lisp<"(/ (- 2 6) -2)"> = '2'

const unparse_maintest0_mod: Cion.Lisp<"(mod 2 5)"> = '2'
const unparse_maintest1_mod: Cion.Lisp<"(mod 5 5)"> = '0'
const unparse_maintest2_mod: Cion.Lisp<"(mod 7 6)"> = '1'
const unparse_maintest3_mod: Cion.Lisp<"(mod 14 6)"> = '2'
const unparse_maintest4_mod: Cion.Lisp<"(mod 6 0)"> = 'nil'
const unparse_maintest5_mod: Cion.Lisp<"(mod -1 2)"> = '1'
const unparse_maintest6_mod: Cion.Lisp<"(mod (- 0 1) 2)"> = '1'
const unparse_maintest7_mod: Cion.Lisp<"(mod 6 1)"> = '0'
const unparse_maintest8_mod: Cion.Lisp<"(mod -17 5)"> = '3'

const unparse_maintest0_mod1: Cion.Lisp<"(% 2 5)"> = '2'
const unparse_maintest1_mod1: Cion.Lisp<"(% 5 5)"> = '0'
const unparse_maintest2_mod1: Cion.Lisp<"(% 7 6)"> = '1'
const unparse_maintest3_mod1: Cion.Lisp<"(% 14 6)"> = '2'
const unparse_maintest4_mod1: Cion.Lisp<"(% 6 0)"> = 'nil'
const unparse_maintest5_mod1: Cion.Lisp<"(% -1 2)"> = '1'
const unparse_maintest6_mod1: Cion.Lisp<"(% (- 0 1) 2)"> = '1'
const unparse_maintest7_mod1: Cion.Lisp<"(% 6 1)"> = '0'
const unparse_maintest8_mod1: Cion.Lisp<"(% -17 5)"> = '3'

// structure: map, vec, list.
const unparse_maintest12_get_0: Cion.Lisp<"(:a {:a 1})"> = '1'
const unparse_maintest12_get_1: Cion.Lisp<"(:a {:a 1 :b 2})"> = '1'
const unparse_maintest12_get_2: Cion.Lisp<"({:a 1 :b 2} :a)"> = '1'
const unparse_maintest12_get_3: Cion.Lisp<"(:c {:a 1 :b 2})"> = 'nil'
const unparse_maintest12_get_4: Cion.Lisp<"({:a 1 :b 2} :c)"> = 'nil'
const unparse_maintest12_get_5: Cion.Lisp<"(get {:a 1 :b 2} :a)"> = '1'

const unparse_maintest0_mapst_0: Cion.Lisp<"{:a 1}"> = '{:a 1}'

const unparse_maintest0_vecst_0: Cion.Lisp<"[0 1 2 3]"> = '[0 1 2 3]'
const unparse_maintest1_vecst_0: Cion.Lisp<"(vector 0 1 2 3)"> = '[0 1 2 3]'

// form macro: if, let.

const unparse_maintest0_letif_0: Cion.Lisp<'(let [x (fn [a b] (+ a b)) y 8] (+ 2 1))'> = '3'
const unparse_maintest0_letif_01: Cion.Lisp<'(let [x (fn [a b] (+ a b)) y 8] (+ y 2 1))'> = '11'
const unparse_maintest0_letif_02: Cion.Lisp<'(let [x (fn [a] (+ 1 a))] (x 1 1))'> = '2'
const unparse_maintest0_letif_03: Cion.Lisp<'(let [x (fn [a] (+ 1 a)) y 9] (x 1 1))'> = '2'
const unparse_maintest0_letif_04: Cion.Lisp<'(let [x (fn [a] (+ 1 a)) y 9] (x y 1))'> = '10'
const unparse_maintest0_letif_05: Cion.Lisp<'(let [x (fn [a b] (+ 1 a)) y 9] (x 1 1))'> = '2'
const unparse_maintest0_letif_06: Cion.Lisp<'(let [x (fn [a b] (+ b a)) y 9] (x 1 2))'> = '3'
const unparse_maintest0_letif_07: Cion.Lisp<'((fn [a b] (+ b a)) 1 2)'> = '3'
const unparse_maintest0_letif_1: Cion.Lisp<'(let [x ((fn [a] (+ a 1)) 1)] (* 2 x))'> = '4'
const unparse_maintest0_letif_2: Cion.Lisp<'(let [x ((fn [a b] (+ a 1 b)) 1 8)] (* 2 x))'> = '20'
const unparse_maintest0_letif_3: Cion.Lisp<'(let [x 8 y ((fn [a b] (+ a x b)) 1 8)] (* 2 y))'> = '34'
const unparse_maintest0_letif_4: Cion.Lisp<'(let [x (let [a 2 b 6] (+ a b)) y ((fn [a b] (+ a x b)) 1 8)] (* 2 y))'> = '34'
const unparse_maintest0_letif_5: Cion.Lisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) y 8] x)'> = '(fn [c] (+ 2 6 c))'
const unparse_maintest0_letif_6: Cion.Lisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c)))] (x 10))'> = '18'
const unparse_maintest0_letif_7: Cion.Lisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) y ((fn [d] (x 2 d)) 10)] y)'> = '10'
const unparse_maintest0_letif_8: Cion.Lisp<'(let [z 2 y ((fn [d] (+ z d)) 10)] y)'> = '12'
const unparse_maintest0_letif_9: Cion.Lisp<'(let [z 2 y ((fn [d] (+ z d)) 10) x 10] y)'> = '12'
const unparse_maintest0_letif_10: Cion.Lisp<'(let [z 2 y 3 x 10] y)'> = '3'
const unparse_maintest0_letif_11: Cion.Lisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) z 2 y ((fn [d] (x z d)) 10)] (x z))'> = '10'
const unparse_maintest0_letif_12: Cion.Lisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) z 20 y ((fn [d] (x d)) z)] y)'> = '28'
const unparse_maintest0_letif_13: Cion.Lisp<'(let [x (let [a 2 b 6] (fn [c] (+ a b c))) z 20 y ((fn [d] (x d)) z 999)] y)'> = '28'

// lift: map, filter, remove, reduce.

const unparse_maintest0_map_0: Cion.Lisp<'(map (fn [n] (* 2 n)) [0 1 2])'> = '[0 2 4]'
const unparse_maintest0_map_1: Cion.Lisp<'(let [f (fn [n] (* 2 n))] (map f [0 1 2]))'> = '[0 2 4]'

// doing
const unparse_maintest0_filter_0: Cion.Lisp<'(filter (fn [n] (> 3 n)) [0 1 2 3 4 5])'> = '[0 1 2]'
const unparse_maintest0_filter_1: Cion.Lisp<'(let [f (fn [n] (> 3 n))] (filter f [0 1 2 3 4 5]))'> = '[0 1 2]'
 
const unparse_maintest0_remove_0: Cion.Lisp<'(remove (fn [n] (> 3 n)) [0 1 2 3 4 5])'> = '[3 4 5]'
const unparse_maintest0_remove_1: Cion.Lisp<'(let [f (fn [n] (> 3 n))] (remove f [0 1 2 3 4 5]))'> = '[3 4 5]'

const unparse_maintest0_reduce_0: Cion.Lisp<'(reduce (fn [r i] (+ r i)) 0 [0 1 2 3 4 5])'> = '15'
const unparse_maintest0_reduce_1: Cion.Lisp<'(let [f (fn [r i] (+ r i))] (reduce f 0 [0 1 2 3 4 5]))'> = '15'

// loop
const unparse_maintest0_loop_1: Cion.Lisp<'(let [f (fn [i] (if (> i 0) (f (- i 2)) true))] (f 10))'> = 'true'

// rest, butlast, interleave, take, drop
const unparse_maintest0_rest_0: Cion.Lisp<'(rest [0 1 2 3])'> = '[1 2 3]'
const unparse_maintest0_rest_1: Cion.Lisp<'(rest [0])'> = '[]'

const unparse_maintest0_butlast_0: Cion.Lisp<'(butlast [0 1 2 3])'> = '[0 1 2]'
const unparse_maintest0_butlast_1: Cion.Lisp<'(butlast [0])'> = '[]'

const unparse_maintest0_reverse_0: Cion.Lisp<'(reverse [0 1 2 3])'> = '[3 2 1 0]'
const unparse_maintest0_reverse_1: Cion.Lisp<'(reverse [0])'> = '[0]'

const unparse_maintest0_interleave_0: Cion.Lisp<'(interleave [0 0 0] [1 1 1])'> = '[0 1 0 1 0 1]'
const unparse_maintest0_interleave_1: Cion.Lisp<'(interleave [0 0 0] [1 1])'> = '[0 1 0 1]'

const unparse_maintest0_take_0: Cion.Lisp<'(take 0 [0 1 2 3 4 5])'> = '[]'
const unparse_maintest0_take_1: Cion.Lisp<'(take 2 [0 1 2 3 4 5])'> = '[0 1]'
const unparse_maintest0_take_2: Cion.Lisp<'(take 9 [0 1 2 3 4 5])'> = '[0 1 2 3 4 5]'

const unparse_maintest0_drop_0: Cion.Lisp<'(drop 9 [0 1 2 3 4 5])'> = '[]'
const unparse_maintest0_drop_1: Cion.Lisp<'(drop 2 [0 1 2 3 4 5])'> = '[2 3 4 5]'
const unparse_maintest0_drop_2: Cion.Lisp<'(drop 0 [0 1 2 3 4 5])'> = '[0 1 2 3 4 5]'

// getter: first, last.
const unparse_maintest0_first_0: Cion.Lisp<'(first [0 1 2])'> = '0'
const unparse_maintest0_first_1: Cion.Lisp<'(first [])'> = 'nil'

const unparse_maintest0_last_0: Cion.Lisp<'(last [0 1 2])'> = '2'
const unparse_maintest0_last_1: Cion.Lisp<'(last [])'> = 'nil'

// conj, concat.
const unparse_maintest0_conj_0: Cion.Lisp<'(conj [0 1] 2 3)'> = '[0 1 2 3]'
const unparse_maintest0_conj_1: Cion.Lisp<'(conj [0 1] [2])'> = '[0 1 [2]]'

// assoc, update, assoc-in, update-in
const unparse_maintest0_assoc_0: Cion.Lisp<'(assoc [0 1 2] 0 99)'> = '[99 1 2]'
const unparse_maintest0_assoc_1: Cion.Lisp<'(assoc [0 1 2] 3 99)'> = '[0 1 2]' // no effect

const unparse_maintest0_update_0: Cion.Lisp<'(update [0 1 2] 1 (fn [x] (+ x 99)))'> = '[0 100 2]'
const unparse_maintest0_update_1: Cion.Lisp<'(update [0 1 2] 99 (fn [x] (+ x 99)))'> = '[0 1 2]' // no effect

const unparse_maintest0_assocIn_0: Cion.Lisp<'(assoc-in [0 1 2] [0] 99)'> = '[99 1 2]'
const unparse_maintest0_assocIn_1: Cion.Lisp<'(assoc-in [0 1 2] [99] 99)'> = '[0 1 2]' // no effect
const unparse_maintest0_assocIn_2: Cion.Lisp<'(assoc-in [0 1 2] [0 0] 99)'> = '{error: "AssocInError8", message: "Keys rests but its value is not vector nor map."}'
const unparse_maintest0_assocIn_3: Cion.Lisp<'(assoc-in [0 1 [2 3 4]] [2 0] 99)'> = '[0 1 [99 3 4]]'
const unparse_maintest0_assocIn_4: Cion.Lisp<'(assoc-in [0 1 [2 3 {:a 4 :b 5}]] [2 0 :a] 99)'> = '{error: "AssocInError7", message: "The value of key (0000000000000000) is not vector nor map."}'
const unparse_maintest0_assocIn_5: Cion.Lisp<'(assoc-in [0 1 [2 3 {:a 4 :b 5}]] [2 2 :a] 99)'> = '[0 1 [2 3 {:a 99 :b 5}]]'
const unparse_maintest0_assocIn_6: Cion.Lisp<'(assoc-in {:x [0 1 [2 3 {:a 4 :b 5}]] :y 0} [:x 2 2 :a] 99)'> = '{:x [0 1 [2 3 {:a 99 :b 5}]] :y 0}'

const unparse_maintest0_updateIn_0: Cion.Lisp<'(update-in [0 1 2] [1] (fn [x] (+ x 99)))'> = '[0 100 2]'
const unparse_maintest0_updateIn_1: Cion.Lisp<'(update-in [0 1 2] [99] (fn [x] (+ x 99)))'> = '[0 1 2]' // no effect
const unparse_maintest0_updateIn_2: Cion.Lisp<'(update-in [0 1 2] [99 99] (fn [x] (+ x 99)))'> = '{error: "AssocInError8", message: "Keys rests but its value is not vector nor map."}'

// arrow macro: ->, ->>.

const unparse_maintest_threadf_0: Cion.Lisp<"(-> 's' (str '01'))"> = "'s01'"
const unparse_maintest_threadf_1: Cion.Lisp<"(-> 'a' (str '01') (str 's'))"> = "'a01s'"
const unparse_maintest_threadf_2: Cion.Lisp<"(str 'a' (str '01' 's'))"> = "'a01s'"
const unparse_maintest_threadf_3: Cion.Lisp<"(-> 1 (+ 1) (+ 2))"> = '4'
const unparse_maintest_threadf_4: Cion.Lisp<"(+ 2 (+ 1 1))"> = '4'

const unparse_maintest_threadl_0: Cion.Lisp<"(->> 's' (str '01'))"> = "'01s'"
const unparse_maintest_threadl_1: Cion.Lisp<"(->> 'a' (str '01') (str 's'))"> = "'s01a'"
const unparse_maintest_threadl_2: Cion.Lisp<"(str 'a' (str '01' 's'))"> = "'a01s'"
const unparse_maintest_threadl_3: Cion.Lisp<"(->> 1 (+ 1) (+ 2))"> = '4'
const unparse_maintest_threadl_4: Cion.Lisp<"(+ 2 (+ 1 1))"> = '4'

// let test
const unparse_maintest_let_0: Cion.Lisp<'(let [a {:a "a"}] (+ 1 2))'> = '3'

const unparse_maintest_letmap_0: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= "in" (b a)))] (->> cv (map :status)))`> = "['in' 'out' 'out']"
const unparse_maintest_letmap_1a: Cion.Lisp<`(= 'in' (:status {:status 'in'}))`> = 'true'
// const unparse_maintest_letmap_1b: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= 'in' (b a)))] (->> cv first))`> = ['map', [['key', ':status'], "'in'"], ['key', ':message'], "'message1'"]]]
const unparse_maintest_letmap_1c: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] (= c (a b)))] (f :status {:status 'in' :message 'message1'} msg))`> = 'true'
const unparse_maintest_letmap_1caaa: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] (= c (b a)))] (f :status {:status 'in' :message 'message1'} msg))`> = 'true'
const unparse_maintest_letmap_1caa: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] [c (a b)])] (f :status {:status 'in' :message 'message1'} msg))`> = "['in' 'in']"
const unparse_maintest_letmap_1cab: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] [= (a b)])] (f :status {:status 'in' :message 'message1'} msg))`> = "[= 'in']"
const unparse_maintest_letmap_1ca: Cion.Lisp<`[(:status {:status 'in' :message 'message1'}) 'in']`> = "['in' 'in']"
const unparse_maintest_letmap_1cb: Cion.Lisp<`[(:status {:status 'in' :message 'message1'}) ((fn [] 'in'))]`> = "['in' 'in']"
const unparse_maintest_letmap_1cc: Cion.Lisp<`(let [v 'in'] [(:status {:status 'in' :message 'message1'}) ((fn [] v))])`> = "['in' 'in']"
const unparse_maintest_letmap_1cd: Cion.Lisp<`(let [v 'in'] [(:status {:status 'in' :message 'message1'}) ((fn [] (first [v v])))])`> = "['in' 'in']"
const unparse_maintest_letmap_1d: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] msg 'in' f (fn [a b c] (= c (b a)))] [msg])`> = "['in']"
const unparse_maintest_letmap_1ce: Cion.Lisp<`(let [v 'in'] [(:status {:status 'in' :message 'message1'}) ((fn [] (:a {:a 'in'})))])`> = "['in' 'in']"

