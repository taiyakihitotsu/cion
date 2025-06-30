import type Cion from '../src/index'

// ----------------
// -- subs-all
// ----------------

const test_subs_all_0: Cion.Lisp<`(subs-all '123456' 1 3)`> = `['1' '23' '456']`
const test_subs_all_1: Cion.Lisp<`(first (subs-all '123456' 1 3))`> = `'1'`

const test_subs_all_0x: Cion.Lisp<`(subs-all '123456' 0 3)`> = `['' '123' '456']`
const test_subs_all_1x: Cion.Lisp<`(first (subs-all '123456' 0 3))`> = `''`

const test_subs_all_0u: Cion.Lisp<`(subs-all '123456' 3 6)`> = `['123' '456' '']`
const test_subs_all_1u: Cion.Lisp<`(first (subs-all '123456' 3 6))`> = `'123'`

const test_subs_all_0w: Cion.Lisp<`(subs-all '123456' 3 8)`> = `['123' '456' '']`
const test_subs_all_1w: Cion.Lisp<`(first (subs-all '123456' 3 8))`> = `'123'`

const test_subs_all_0y: Cion.Lisp<`(subs-all '123456' -3 6)`> = `['' '123456' '']`
const test_subs_all_1y: Cion.Lisp<`(first (subs-all '123456' -3 6))`> = `''`

const test_subs_all_0z: Cion.Lisp<`(subs-all '123456' 2 2)`> = `['12' '' '3456']`
const test_subs_all_1z: Cion.Lisp<`(first (subs-all '123456' 2 2))`> = `'12'`

const test_subs_all_0az: Cion.Lisp<`(subs-all '123456' 0 0)`> = `['' '' '123456']`
const test_subs_all_1az: Cion.Lisp<`(first (subs-all '123456' 0 0))`> = `''`

const test_subs_all_0abz: Cion.Lisp<`(subs-all '123456' 6 6)`> = `['123456' '' '']`
const test_subs_all_1abz: Cion.Lisp<`(first (subs-all '123456' 6 6))`> = `'123456'`
