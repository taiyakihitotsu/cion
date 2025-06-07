import type { Cion } from '../src/index'

// Regex test itself should be done in test/regex.ts I think.

const cionregex0: Cion.Lisp<`(re-find 'abc' 'abc')`> = `'abc'`
const cionregex1: Cion.Lisp<`(re-find 'ab\\d\\dc' 'ab12c')`> = `'ab12c'`
const cionregex:  Cion.Lisp<`(re-find 'ab[\\dz]{1,2}c' 'abz2c')`> = `'abz2c'`
