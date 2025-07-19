import type {Cion} from '../src/index'

const xestsometh0: Cion.Lisp<`(some-> 1 number?)`> = 'true'

const testsometh1: Cion.Lisp<`(some-> 1 number? nil?)`> = 'false'

const testsometh1_sn: Cion.Lisp<`(some-> 1 string? nil?)`> = 'false'
const testsometh1_ss: Cion.Lisp<`(some-> 1 string? string?)`> = 'false'
const testsometh1_ef: Cion.Lisp<`(some-> [] first)`> = 'nil'
const testsometh1_efs: Cion.Lisp<`(some-> [] first string?)`> = 'nil'
const testsometh1_e: Cion.Lisp<`(some-> 1 (fn [n] nil) string?)`> = 'nil'
const testsometh1_en: Cion.Lisp<`(some-> 1 number? (fn [n] nil) string?)`> = 'nil'

const __testsometh1: Cion.Lisp<`(if (nil? (if (number? 1) (number? 1) false)) (nil? (if (number? 1) (number? 1) false)) false)`> = 'false'
const ____testsometh1: Cion.Lisp<`(nil? (if (number? 1) (number? 1) false))`> = 'false'
const ____testsometh2: Cion.Lisp<`(nil? true)`> = 'false'
const ____testsometh3: Cion.Lisp<`(nil? false)`> = 'false'
const ____testsometh4_letread: Cion.Lisp<`(nil? (let [n nil] n))`> = 'true'

const testsometh2: Cion.Lisp<`(some-> 1 number? (-> nil? not))`> = 'true'
const __testsometh2: Cion.Lisp<`(if (-> (if (number? 1) (number? 1) false) nil? not) (-> (if (number? 1) (number? 1) false) nil? not) false)`> = 'true'

const testsometh4: Cion.Lisp<`(some-> 1 boolean? ((fn [n] (if (= n false) 2 4))))`> = '2'
const __testsometh4: Cion.Lisp<`(if ((fn [n] (if (= n false) 2 4)) (if (boolean? 1) (boolean? 1) false)) ((fn [n] (if (= n false) 2 4)) (if (boolean? 1) (boolean? 1) false)) nil)`> = '2'

const testsometh5a: Cion.Lisp<`(some-> 1 boolean? string? ((fn [n] (if (= n false) 2 -2))) pos-int?)`> = 'true'
const testsomet60: Cion.Lisp<`((fn [n] (some-> n number?)) 1)`> = 'true'
const testsometh00: Cion.Lisp<`((fn [n] (some-> n inc)) 1)`> = '2'
