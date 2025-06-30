import type Cion from '../src/index'

// -------------------
// -- subs
// --------------------

const test_subs0: Cion.Lisp<`(subs '123456' 1 3)`> = `'23'`
const test_subs0x: Cion.Lisp<`(subs '123456' 0 3)`> = `'123'`
const test_subs0u: Cion.Lisp<`(subs '123456' 3 6)`> = `'456'`
const test_subs0w: Cion.Lisp<`(subs '123456' 3 8)`> = `'456'`
const test_subs0y: Cion.Lisp<`(subs '123456' -3 6)`> = `'123456'`
const test_subs0z: Cion.Lisp<`(subs '123456' 2 2)`> = `''`
const test_subs0az: Cion.Lisp<`(subs '123456' 0 0)`> = `''`
const test_subs0abz: Cion.Lisp<`(subs '123456' 6 6)`> = `''`
const test_subs0taz: Cion.Lisp<`(subs '123456' 0)`> = `'123456'`
const test_subs0tabz: Cion.Lisp<`(subs '123456' 6)`> = `''`
const test_subs0tazc: Cion.Lisp<`(subs '123456' 3)`> = `'456'`
