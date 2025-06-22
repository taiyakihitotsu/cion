import type {Cion} from '../src/index'

const testsometh0: Cion.Lisp<`(some-> 1 number?)`> = 'true'

const testsometh1: Cion.Lisp<`(some-> 1 number? nil?)`> = 'nil'
const __testsometh1: Cion.Lisp<`(if (nil? (if (number? 1) (number? 1) false)) (nil? (if (number? 1) (number? 1) false)) false)`> = 'false'
const ____testsometh1: Cion.Lisp<`(nil? (if (number? 1) (number? 1) false))`> = 'false'
const ____testsometh2: Cion.Lisp<`(nil? true)`> = 'false'
const ____testsometh3: Cion.Lisp<`(nil? false)`> = 'false'
const ____testsometh4_letread: Cion.Lisp<`(nil? (let [n nil] n))`> = 'true'

const testsometh2: Cion.Lisp<`(some-> 1 number? (-> nil? not))`> = 'true'
const __testsometh2: Cion.Lisp<`(if (-> (if (number? 1) (number? 1) false) nil? not) (-> (if (number? 1) (number? 1) false) nil? not) false)`> = 'true'

const testsometh22: Cion.Lisp<`(some-> 1 number? (-> nil? not) string?)`> = 'nil'
const testsometh3: Cion.Lisp<`(some-> 1 boolean? number?)`> = 'nil'

const testsometh4: Cion.Lisp<`(some-> 1 boolean? ((fn [n] (if (= n false) 2 4))))`> = '2'
const __testsometh4: Cion.Lisp<`(if ((fn [n] (if (= n false) 2 4)) (if (boolean? 1) (boolean? 1) false)) ((fn [n] (if (= n false) 2 4)) (if (boolean? 1) (boolean? 1) false)) nil)`> = '2'

const testsometh5: Cion.Lisp<`(some-> 1 boolean? string? ((fn [n] (if (= n false) 2 -2))) neg-int?)`> = 'nil'
const __testsometh5: Cion.Lisp<`(some-> 1 boolean? string? ((fn [n] (if (= n false) 2 -2))) neg-int?)`> = 'nil'
const testsometh5aa: Cion.Lisp<`(some-> 1 boolean? ((fn [n] (if (= n false) 2 -2))) neg-int?)`> = 'nil'

const testsometh5a: Cion.Lisp<`(some-> 1 boolean? string? ((fn [n] (if (= n false) 2 -2))) pos-int?)`> = 'true'
const testsomet60: Cion.Lisp<`((fn [n] (some-> n number?)) 1)`> = 'true'
const testsometh00: Cion.Lisp<`((fn [n] (some-> n inc)) 1)`> = '2'
