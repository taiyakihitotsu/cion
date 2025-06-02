import type { Cion } from '../src/index'

const test_iszero0: Cion.Lisp<`(zero? 0)`> = 'true'
const test_iszero1: Cion.Lisp<`(zero? 1)`> = 'false'
const test_iszero2: Cion.Lisp<`(zero? [])`> = 'false'
const test_iszero3: Cion.Lisp<`(zero? nil)`> = 'false'
const test_iszero4: Cion.Lisp<`(-> 0 zero? not)`> = 'false'
