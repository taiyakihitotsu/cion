import type Cion from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// --- Cion Lisp Interpreter Patterns ---

// Control Flow: let, if, eq, str
const test_cion_let_if: true = {} as Equal<"'this_is_true'", Cion.Lisp<"(let [a 'a'] (if (eq a 'a') 'this_is_true' 'this_is_false'))">>
const test_cion_if_simple: true = {} as Equal<'1', Cion.Lisp<"(if true 1 2)">>
const test_cion_if_nested: true = {} as Equal<"'astrbstr'", Cion.Lisp<"(if true (let [a 'astr' b 'bstr'] (str a b)) 11)">>

// Boolean Operators: AND
const test_cion_and_tt: true = {} as Equal<'true', Cion.Lisp<"(and true true)">>
const test_cion_and_tf: true = {} as Equal<'false', Cion.Lisp<"(and true false)">>
const test_cion_and_ff: true = {} as Equal<'false', Cion.Lisp<"(and false false)">>
const test_cion_and_ft: true = {} as Equal<'false', Cion.Lisp<"(and false true)">>
const test_cion_and_ttt: true = {} as Equal<'true', Cion.Lisp<"(and true true true)">>
const test_cion_and_ftf: true = {} as Equal<'false', Cion.Lisp<"(and false true false)">>
const test_cion_and_fff: true = {} as Equal<'false', Cion.Lisp<"(and false false false)">>
const test_cion_and_ftt: true = {} as Equal<'false', Cion.Lisp<"(and false true true)">>

// Boolean Operators: OR
const test_cion_or_tt: true = {} as Equal<'true', Cion.Lisp<"(or true true)">>
const test_cion_or_tf: true = {} as Equal<'true', Cion.Lisp<"(or true false)">>
const test_cion_or_ff: true = {} as Equal<'false', Cion.Lisp<"(or false false)">>
const test_cion_or_ft: true = {} as Equal<'true', Cion.Lisp<"(or false true)">>
const test_cion_or_ttt: true = {} as Equal<'true', Cion.Lisp<"(or true true true)">>
const test_cion_or_ftf: true = {} as Equal<'true', Cion.Lisp<"(or false true false)">>
const test_cion_or_fff: true = {} as Equal<'false', Cion.Lisp<"(or false false false)">>
const test_cion_or_ftt: true = {} as Equal<'true', Cion.Lisp<"(or false true true)">>

// Boolean Operators: NOT
const test_cion_not_t: true = {} as Equal<'false', Cion.Lisp<"(not true)">>
const test_cion_not_f: true = {} as Equal<'true', Cion.Lisp<"(not false)">>

// order tests: >, <, >=, <=, =, eq.
// --- Cion Lisp: Equality and Comparison Operators ---

// Equality: eq
const test_cion_eq_0: true = {} as Equal<'false', Cion.Lisp<"(eq 'a' 'b')">>
const test_cion_eq_1: true = {} as Equal<'true', Cion.Lisp<"(eq 'a' 'a')">>
const test_cion_eq_2: true = {} as Equal<'true', Cion.Lisp<"(let [a 'a'] (eq a 'a'))">>
const test_cion_eq_3: true = {} as Equal<'false', Cion.Lisp<"(let [a 'b'] (eq a 'a'))">>

// Equality: eq (Numeric)
const test_cion_eq_num_0: true = {} as Equal<'true', Cion.Lisp<"(eq 1 1)">>
const test_cion_eq_num_1: true = {} as Equal<'false', Cion.Lisp<"(eq 1 2)">>
const test_cion_eq_num_2: true = {} as Equal<'true', Cion.Lisp<"(let [a 1] (eq a 1))">>
const test_cion_eq_num_3: true = {} as Equal<'false', Cion.Lisp<"(let [a 2] (eq a 1))">>

// Equality: = alias
const test_cion_alias_eq_0: true = {} as Equal<'false', Cion.Lisp<"(= 'a' 'b')">>
const test_cion_alias_eq_1: true = {} as Equal<'true', Cion.Lisp<"(= 'a' 'a')">>
const test_cion_alias_eq_2: true = {} as Equal<'true', Cion.Lisp<"(let [a 'a'] (= a 'a'))">>
const test_cion_alias_eq_3: true = {} as Equal<'false', Cion.Lisp<"(let [a 'b'] (= a 'a'))">>

const test_cion_alias_eq_num_0: true = {} as Equal<'true', Cion.Lisp<"(= 1 1)">>
const test_cion_alias_eq_num_1: true = {} as Equal<'false', Cion.Lisp<"(= 1 2)">>
const test_cion_alias_eq_num_2: true = {} as Equal<'true', Cion.Lisp<"(let [a 1] (= a 1))">>
const test_cion_alias_eq_num_3: true = {} as Equal<'false', Cion.Lisp<"(let [a 2] (= a 1))">>

// Comparison: Greater Than (>)
const test_cion_gt_0: true = {} as Equal<'true', Cion.Lisp<"(> 15 14)">>
const test_cion_gt_1: true = {} as Equal<'true', Cion.Lisp<"(> 15 14 13)">>
const test_cion_gt_2: true = {} as Equal<'false', Cion.Lisp<"(> 15 14 16)">>

// Comparison: Less Than (<)
const test_cion_lt_0: true = {} as Equal<'false', Cion.Lisp<"(< 15 14)">>
const test_cion_lt_1: true = {} as Equal<'false', Cion.Lisp<"(< 15 14 13)">>
const test_cion_lt_0_1: true = {} as Equal<'true', Cion.Lisp<"(< 14 15)">>
const test_cion_lt_1_1: true = {} as Equal<'true', Cion.Lisp<"(< 13 14 15)">>
const test_cion_lt_2: true = {} as Equal<'false', Cion.Lisp<"(< 13 15 14)">>
const test_cion_lt_3: true = {} as Equal<'false', Cion.Lisp<"(< 15 14 13)">>

// Comparison: Greater Than or Equal (>=)
const test_cion_gte_0: true = {} as Equal<'true', Cion.Lisp<"(>= 15 14)">>
const test_cion_gte_1: true = {} as Equal<'true', Cion.Lisp<"(>= 15 14 13)">>
const test_cion_gte_2: true = {} as Equal<'false', Cion.Lisp<"(>= 13 14 15)">>
const test_cion_gte_0_1: true = {} as Equal<'false', Cion.Lisp<"(>= 14 15)">>
const test_cion_gte_1_1: true = {} as Equal<'false', Cion.Lisp<"(>= 13 14 15)">>
const test_cion_gte_2_1: true = {} as Equal<'true', Cion.Lisp<"(>= 15 15 15)">>
const test_cion_gte_2_2: true = {} as Equal<'true', Cion.Lisp<"(>= 15 15 14)">>
const test_cion_gte_2_3: true = {} as Equal<'true', Cion.Lisp<"(>= 15 14 14)">>
const test_cion_gte_2_4: true = {} as Equal<'false', Cion.Lisp<"(>= 15 14 15)">>

// Comparison: Less Than or Equal (<=)
const test_cion_lte_0: true = {} as Equal<'false', Cion.Lisp<"(<= 15 14)">>
const test_cion_lte_1: true = {} as Equal<'false', Cion.Lisp<"(<= 15 14 13)">>
const test_cion_lte_2: true = {} as Equal<'false', Cion.Lisp<"(<= 15 15 14)">>
const test_cion_lte_0_1: true = {} as Equal<'true', Cion.Lisp<"(<= 14 15)">>
const test_cion_lte_1_1: true = {} as Equal<'true', Cion.Lisp<"(<= 13 14 15)">>
const test_cion_lte_2_1: true = {} as Equal<'true', Cion.Lisp<"(<= 15 15 15)">>
const test_cion_lte_2_2: true = {} as Equal<'true', Cion.Lisp<"(<= 14 15 15)">>
const test_cion_lte_2_3: true = {} as Equal<'true', Cion.Lisp<"(<= 14 14 15)">>

// math operators: +, -, *, /, %, mod.
// Addition: +
const test_cion_add_0: true = {} as Equal<'2', Cion.Lisp<"(+ 1 1)">>
const test_cion_add_1: true = {} as Equal<'1', Cion.Lisp<"(+ 1 0)">>
const test_cion_add_2: true = {} as Equal<'0', Cion.Lisp<"(+ 0 0)">>
const test_cion_add_3: true = {} as Equal<'9', Cion.Lisp<"(+ 4 5)">>
const test_cion_add_4: true = {} as Equal<'15', Cion.Lisp<"(+ 5 4 6)">>
const test_cion_add_5: true = {} as Equal<'9', Cion.Lisp<"(+ 5 4 0)">>
const test_cion_add_6: true = {} as Equal<'9', Cion.Lisp<"(+ 5 0 4)">>
const test_cion_add_7: true = {} as Equal<'9', Cion.Lisp<"(+ 0 4 5)">>
const test_cion_add_8: true = {} as Equal<'0', Cion.Lisp<"(+ -1 1)">>
const test_cion_add_9: true = {} as Equal<'-4', Cion.Lisp<"(+ -2 -2)">>

// Subtraction: -
const test_cion_sub_0: true = {} as Equal<'3', Cion.Lisp<"(- 4 1)">>
const test_cion_sub_1: true = {} as Equal<'1', Cion.Lisp<"(- 1 0)">>
const test_cion_sub_2: true = {} as Equal<'0', Cion.Lisp<"(- 0 0)">>
const test_cion_sub_3: true = {} as Equal<'-1', Cion.Lisp<"(- 0 1)">>
const test_cion_sub_4: true = {} as Equal<'9', Cion.Lisp<"(- 10 1)">>
const test_cion_sub_5: true = {} as Equal<'19', Cion.Lisp<"(- 10 -9)">>

// Multiplication: *
const test_cion_mul_0: true = {} as Equal<'3', Cion.Lisp<"(* 3 1)">>
const test_cion_mul_1: true = {} as Equal<'0', Cion.Lisp<"(* 0 1)">>
const test_cion_mul_2: true = {} as Equal<'0', Cion.Lisp<"(* 0 0)">>
const test_cion_mul_3: true = {} as Equal<'6', Cion.Lisp<"(* 2 3)">>
const test_cion_mul_4: true = {} as Equal<'-3', Cion.Lisp<"(* (- 2 3) 3)">>
const test_cion_mul_5: true = {} as Equal<'-6', Cion.Lisp<"(* (- 0 2) 3)">>
const test_cion_mul_6: true = {} as Equal<'-6', Cion.Lisp<"(* -2 3)">>

// --- Cion Lisp: Division and Modulo Operators ---

// Division: /
const test_cion_div_0: true = {} as Equal<'4', Cion.Lisp<"(/ 4 1)">>
const test_cion_div_1: true = {} as Equal<'nil', Cion.Lisp<"(/ 1 0)">>
const test_cion_div_2: true = {} as Equal<'nil', Cion.Lisp<"(/ 0 0)">>
const test_cion_div_3: true = {} as Equal<'1', Cion.Lisp<"(/ 4 4)">>
const test_cion_div_4: true = {} as Equal<'2', Cion.Lisp<"(/ 4 2)">>
const test_cion_div_5: true = {} as Equal<'-2', Cion.Lisp<"(/ 4 (- 0 2))">>
const test_cion_div_6: true = {} as Equal<'2', Cion.Lisp<"(/ (- 2 6) (- 0 2))">>
const test_cion_div_7: true = {} as Equal<'2', Cion.Lisp<"(/ (- 2 6) -2)">>

// Modulo: mod (Mathematical modulo: result sign matches divisor)
const test_cion_mod_0: true = {} as Equal<'2', Cion.Lisp<"(mod 2 5)">>
const test_cion_mod_1: true = {} as Equal<'0', Cion.Lisp<"(mod 5 5)">>
const test_cion_mod_2: true = {} as Equal<'1', Cion.Lisp<"(mod 7 6)">>
const test_cion_mod_3: true = {} as Equal<'2', Cion.Lisp<"(mod 14 6)">>
const test_cion_mod_4: true = {} as Equal<'nil', Cion.Lisp<"(mod 6 0)">>
const test_cion_mod_5: true = {} as Equal<'1', Cion.Lisp<"(mod -1 2)">>
const test_cion_mod_6: true = {} as Equal<'1', Cion.Lisp<"(mod (- 0 1) 2)">>
const test_cion_mod_7: true = {} as Equal<'0', Cion.Lisp<"(mod 6 1)">>
const test_cion_mod_8: true = {} as Equal<'3', Cion.Lisp<"(mod -17 5)">>

// Remainder: % (C-style remainder: result sign matches dividend)
const test_cion_rem_0: true = {} as Equal<'2', Cion.Lisp<"(% 2 5)">>
const test_cion_rem_1: true = {} as Equal<'0', Cion.Lisp<"(% 5 5)">>
const test_cion_rem_2: true = {} as Equal<'1', Cion.Lisp<"(% 7 6)">>
const test_cion_rem_3: true = {} as Equal<'2', Cion.Lisp<"(% 14 6)">>
const test_cion_rem_4: true = {} as Equal<'nil', Cion.Lisp<"(% 6 0)">>
const test_cion_rem_5: true = {} as Equal<'-1', Cion.Lisp<"(% -1 2)">>
const test_cion_rem_6: true = {} as Equal<'-1', Cion.Lisp<"(% (- 0 1) 2)">>
const test_cion_rem_7: true = {} as Equal<'0', Cion.Lisp<"(% 6 1)">>
const test_cion_rem_8: true = {} as Equal<'-2', Cion.Lisp<"(% -17 5)">>

// structure: map, vec, list.
// --- Cion Lisp: Map and Vector Operations ---

// Map Access (Keyword lookup and get function)
const test_cion_map_get_0: true = {} as Equal<'1', Cion.Lisp<"(:a {:a 1})">>
const test_cion_map_get_1: true = {} as Equal<'1', Cion.Lisp<"(:a {:a 1 :b 2})">>
const test_cion_map_get_2: true = {} as Equal<'1', Cion.Lisp<"({:a 1 :b 2} :a)">>
const test_cion_map_get_3: true = {} as Equal<'nil', Cion.Lisp<"(:c {:a 1 :b 2})">>
const test_cion_map_get_4: true = {} as Equal<'nil', Cion.Lisp<"({:a 1 :b 2} :c)">>
const test_cion_map_get_5: true = {} as Equal<'1', Cion.Lisp<"(get {:a 1 :b 2} :a)">>

// Literal Representation (Map and Vector)
const test_cion_map_literal: true = {} as Equal<'{:a 1}', Cion.Lisp<"{:a 1}">>
const test_cion_vec_literal: true = {} as Equal<'[0 1 2 3]', Cion.Lisp<"[0 1 2 3]">>
const test_cion_vec_function: true = {} as Equal<'[0 1 2 3]', Cion.Lisp<"(vector 0 1 2 3)">>

// --- Cion Lisp: Higher-Order Collection Functions (map, filter, remove, reduce) ---

// Map: Transform each element in a vector
const test_cion_map_0: true = {} as Equal<'[0 2 4]', Cion.Lisp<"(map (fn [n] (* 2 n)) [0 1 2])">>
const test_cion_map_1: true = {} as Equal<'[0 2 4]', Cion.Lisp<"(let [f (fn [n] (* 2 n))] (map f [0 1 2]))">>

// Filter: Keep elements that satisfy the predicate
const test_cion_filter_0: true = {} as Equal<'[0 1 2]', Cion.Lisp<"(filter (fn [n] (> 3 n)) [0 1 2 3 4 5])">>
const test_cion_filter_1: true = {} as Equal<'[0 1 2]', Cion.Lisp<"(let [f (fn [n] (> 3 n))] (filter f [0 1 2 3 4 5]))">>

// Remove: Exclude elements that satisfy the predicate
const test_cion_remove_0: true = {} as Equal<'[3 4 5]', Cion.Lisp<"(remove (fn [n] (> 3 n)) [0 1 2 3 4 5])">>
const test_cion_remove_1: true = {} as Equal<'[3 4 5]', Cion.Lisp<"(let [f (fn [n] (> 3 n))] (remove f [0 1 2 3 4 5]))">>

// Reduce: Accumulate a value from elements
const test_cion_reduce_0: true = {} as Equal<'15', Cion.Lisp<"(reduce (fn [r i] (+ r i)) 0 [0 1 2 3 4 5])">>
const test_cion_reduce_1: true = {} as Equal<'15', Cion.Lisp<"(let [f (fn [r i] (+ r i))] (reduce f 0 [0 1 2 3 4 5]))">>

// loop
const unparse_maintest0_loop_1: true = {} as Equal<Cion.Lisp<'(let [f (fn [i] (if (> i 0) (f (- i 2)) true))] (f 10))'>, 'true'>

// rest, butlast, interleave, take, drop
const test_rest_0: true = {} as Equal<'[1 2 3]', Cion.Lisp<"(rest [0 1 2 3])">>
const test_rest_1: true = {} as Equal<'[]', Cion.Lisp<"(rest [0])">>

const test_butlast_0: true = {} as Equal<'[0 1 2]', Cion.Lisp<"(butlast [0 1 2 3])">>
const test_butlast_1: true = {} as Equal<'[]', Cion.Lisp<"(butlast [0])">>

const test_reverse_0: true = {} as Equal<'[3 2 1 0]', Cion.Lisp<"(reverse [0 1 2 3])">>
const test_reverse_1: true = {} as Equal<'[0]', Cion.Lisp<"(reverse [0])">>

const test_interleave_0: true = {} as Equal<'[0 1 0 1 0 1]', Cion.Lisp<"(interleave [0 0 0] [1 1 1])">>
const test_interleave_1: true = {} as Equal<'[0 1 0 1]', Cion.Lisp<"(interleave [0 0 0] [1 1])">>

const test_take_0: true = {} as Equal<'[]', Cion.Lisp<"(take 0 [0 1 2 3 4 5])">>
const test_take_1: true = {} as Equal<'[0 1]', Cion.Lisp<"(take 2 [0 1 2 3 4 5])">>
const test_take_2: true = {} as Equal<'[0 1 2 3 4 5]', Cion.Lisp<"(take 9 [0 1 2 3 4 5])">>

const test_drop_0: true = {} as Equal<'[]', Cion.Lisp<"(drop 9 [0 1 2 3 4 5])">>
const test_drop_1: true = {} as Equal<'[2 3 4 5]', Cion.Lisp<"(drop 2 [0 1 2 3 4 5])">>
const test_drop_2: true = {} as Equal<'[0 1 2 3 4 5]', Cion.Lisp<"(drop 0 [0 1 2 3 4 5])">>

// getter: first, last.
const test_first_0: true = {} as Equal<'0', Cion.Lisp<"(first [0 1 2])">>
const test_first_1: true = {} as Equal<'nil', Cion.Lisp<"(first [])">>

const test_last_0: true = {} as Equal<'2', Cion.Lisp<"(last [0 1 2])">>
const test_last_1: true = {} as Equal<'nil', Cion.Lisp<"(last [])">>

const test_conj_0: true = {} as Equal<'[0 1 2 3]', Cion.Lisp<"(conj [0 1] 2 3)">>
const test_conj_1: true = {} as Equal<'[0 1 [2]]', Cion.Lisp<"(conj [0 1] [2])">>

const test_assoc_0: true = {} as Equal<'[99 1 2]', Cion.Lisp<"(assoc [0 1 2] 0 99)">>
const test_assoc_1: true = {} as Equal<'nil', Cion.Lisp<"(assoc [0 1 2] 3 99)">>

const test_update_0: true = {} as Equal<'[0 100 2]', Cion.Lisp<"(update [0 1 2] 1 (fn [x] (+ x 99)))">>
const test_update_1: true = {} as Equal<'nil', Cion.Lisp<"(update [0 1 2] 99 (fn [x] (+ x 99)))">>

const test_assocIn_0: true = {} as Equal<'[99 1 2]', Cion.Lisp<"(assoc-in [0 1 2] [0] 99)">>
const test_assocIn_1: true = {} as Equal<'nil', Cion.Lisp<"(assoc-in [0 1 2] [99] 99)">>

type FailsAssocIn0 = Cion.Lisp<'(assoc-in [0 1 2] [0 0] 99)'>
const test_assocIn_2: true = {} as Equal<FailsAssocIn0, 'nil'>
const test_assocIn_3: true = {} as Equal<Cion.Lisp<'(assoc-in [0 1 [2 3 4]] [2 0] 99)'>, '[0 1 [99 3 4]]'>
type FailsAssocIn1 = Cion.Lisp<'(assoc-in [0 1 [2 3 {:a 4 :b 5}]] [2 0 :a] 99)'>
const test_assocIn_4: true = {} as Equal<FailsAssocIn1, 'nil'>

const test_assocIn_5: true = {} as Equal<'[0 1 [2 3 {:a 99 :b 5}]]', Cion.Lisp<"(assoc-in [0 1 [2 3 {:a 4 :b 5}]] [2 2 :a] 99)">>
const test_assocIn_6: true = {} as Equal<'{:x [0 1 [2 3 {:a 99 :b 5}]] :y 0}', Cion.Lisp<"(assoc-in {:x [0 1 [2 3 {:a 4 :b 5}]] :y 0} [:x 2 2 :a] 99)">>

const test_updateIn_0: true = {} as Equal<'[0 100 2]', Cion.Lisp<"(update-in [0 1 2] [1] (fn [x] (+ x 99)))">>
const test_updateIn_1: true = {} as Equal<'nil', Cion.Lisp<"(update-in [0 1 2] [99] (fn [x] (+ x 99)))">>

type FailsUpdateIn0 = Cion.Lisp<'(update-in [0 1 2] [99 99] (fn [x] (+ x 99)))'>
const test_updateIn_2: true = {} as Equal<'nil', FailsUpdateIn0>

// arrow macro: ->, ->>.
const test_threadf_0: true = {} as Equal<"'s01'", Cion.Lisp<"(-> 's' (str '01'))">>
const test_threadf_1: true = {} as Equal<"'a01s'", Cion.Lisp<"(-> 'a' (str '01') (str 's'))">>
const test_threadf_2: true = {} as Equal<"'a01s'", Cion.Lisp<"(str 'a' (str '01' 's'))">>
const test_threadf_3: true = {} as Equal<'4', Cion.Lisp<"(-> 1 (+ 1) (+ 2))">>
const test_threadf_4: true = {} as Equal<'4', Cion.Lisp<"(+ 2 (+ 1 1))">>

const test_threadl_0: true = {} as Equal<"'01s'", Cion.Lisp<"(->> 's' (str '01'))">>
const test_threadl_1: true = {} as Equal<"'s01a'", Cion.Lisp<"(->> 'a' (str '01') (str 's'))">>
const test_threadl_2: true = {} as Equal<"'a01s'", Cion.Lisp<"(str 'a' (str '01' 's'))">>
const test_threadl_3: true = {} as Equal<'4', Cion.Lisp<"(->> 1 (+ 1) (+ 2))">>
const test_threadl_4: true = {} as Equal<'4', Cion.Lisp<"(+ 2 (+ 1 1))">>
