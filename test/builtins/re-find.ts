import type Cion from '../../src/index'
import type { Equal } from '../../src/util'

// [note]
// This is not for regex test but parse test.
// If you want to check the regex engine itself, see: `test/regex.ts`, `test/regex-eval.ts`, `test/regex-compiler.ts`

const refind_test_0 : true = {} as Equal<`'aaa'`, Cion.Lisp<`(re-find '[a-z]*' 'aaa')`>>

type EmailRegex = `'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'`
const refind_test_1 : true = {} as Equal<`'zzz.zzz@testmailreg.com'`, Cion.Lisp<`(re-find ${EmailRegex} 'zzz.zzz@testmailreg.com')`>>

const refind_test_2 : true = {} as Equal<`'abc'`,   Cion.Lisp<`(re-find 'abc' 'abc')`>>
const refind_test_3 : true = {} as Equal<`'ab12c'`, Cion.Lisp<`(re-find 'ab\\d\\dc' 'ab12c')`>>
const refind_test_4 : true = {} as Equal<`'abz2c'`, Cion.Lisp<`(re-find 'ab[\\dz]{1,2}c' 'abz2c')`>>
