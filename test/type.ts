import type Cion from '../src/index.ts'

const test_type0: Cion.Lisp<`(type 9)`> = `'number'`
const test_type1: Cion.Lisp<`(type '9')`> = `'string'`
const test_type2: Cion.Lisp<`(type true)`> = `'boolean'`
const test_type3: Cion.Lisp<`(type [])`> = `'vector'`
const test_type4: Cion.Lisp<`(type [9 8 7])`> = `'vector'`
const test_type6: Cion.Lisp<`(type {a: 9})`> = `'map'`
const test_type7: Cion.Lisp<`(type {})`> = `'map'`
const test_type8: Cion.Lisp<`(type number?)`> = `'fn'`
const test_type8a: Cion.Lisp<`(type some->)`> = `'symbol'` // [todo]
const test_type9: Cion.Lisp<`(type (fn [n] (inc n)))`> = `'fn'`
const test_typea: Cion.Lisp<`(type :key)`> = `'key'`
const test_type: Cion.Lisp<`(type nil)`> = `'nil'`
