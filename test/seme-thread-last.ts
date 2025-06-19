import type {Cion} from '../src/index'

const testsometh0: Cion.Lisp<`(some->> 1 number?)`> = 'true'
const testsometh1: Cion.Lisp<`(some->> 1 number? nil?)`> = 'nil'
// [todo]
// @ts-expect-error:
const testsometh2: Cion.Lisp<`(some->> 1 number? (-> nil? not))`> = 'nil'
const testsometh22: Cion.Lisp<`(some->> 1 number? (-> nil? not) string?)`> = 'nil'
const testsometh222: Cion.Lisp<`(some->> 1 (+ 2 2) number?)`> = 'true'
const testsometh3: Cion.Lisp<`(some->> 1 boolean? number?)`> = 'nil'
const testsometh4: Cion.Lisp<`(some->> 1 boolean? ((fn [n] (if (= n false) 2 4))))`> = '2'
const testsometh5: Cion.Lisp<`(some->> 1 boolean? ((fn [n] (if (= n false) 2 4))) number?)`> = 'true'


