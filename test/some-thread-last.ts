import type {Cion} from '../src/index'

const testsometh0: Cion.Lisp<`(some->> 1 number?)`> = 'true'
const testsometh1: Cion.Lisp<`(some->> 1 number? nil?)`> = 'false'

const __testsometh2: Cion.Lisp<`(some->> 1 number?)`> = 'true'
// @ts-expect-error:
const testsometh2: Cion.Lisp<`(some->> 1 number? (-> nil? not))`> = 'nil'
// [todo]
const testsometh22: Cion.Lisp<`(some->> 1 number? (-> nil? not) string?)`> = 'nil'
const __testsometh22: Cion.Lisp<`(string? (-> nil? not true))`> = 'false'
const testsometh222: Cion.Lisp<`(some->> 1 (+ 2 2) number?)`> = 'true'
const testsometh3: Cion.Lisp<`(some->> 1 boolean? number?)`> = 'false'
const testsometh4: Cion.Lisp<`(some->> 1 boolean? ((fn [n] (if (= n false) 2 4))))`> = '2'
const testsometh5: Cion.Lisp<`(some->> 1 boolean? ((fn [n] (if (= n false) 2 4))) number?)`> = 'true'


