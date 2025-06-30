import type Cion from '../src/index'

// [note]
// This is not for regex test but parse test.
// If you want to check the regex engine itself, see: `test/regex.ts`, `test/regex-eval.ts`, `test/regex-compiler.ts`

const test_refind1 : Cion.Lisp<`(re-find '[a-z]*' 'aaa')`> = `'aaa'`
type email = `'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'`
const test_refind : Cion.Lisp<`(re-find ${email} 'zzz.zzz@testmailreg.com')`> = `'zzz.zzz@testmailreg.com'`

// Regex test itself should be done in test/regex.ts I think.

const cionregex0: Cion.Lisp<`(re-find 'abc' 'abc')`> = `'abc'`
const cionregex1: Cion.Lisp<`(re-find 'ab\\d\\dc' 'ab12c')`> = `'ab12c'`
const cionregex:  Cion.Lisp<`(re-find 'ab[\\dz]{1,2}c' 'abz2c')`> = `'abz2c'`
