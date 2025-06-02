import type Cion from '../src/index'
import type { First } from '../src/index'
import { VNil }  from '../src/sexprtypes'

type testvec = [`vec`, [`prim`, true], [`prim`, 0], [`prim`, 1]];
const testfirst: First<testvec> = [`prim`, true];

const maintest0_first_0: Cion.RawLisp<'(first [0 1 2])'> = ['prim', '0000000000000000']
const maintest0_first_1: Cion.RawLisp<'(first [])'> = VNil

const maintest1_first_0: Cion.Lisp<'(first [0 1 2])'> = '0'
const maintest1_first_1: Cion.Lisp<'(first [])'> = 'nil'
// @ts-expect-error:
const maintest1_first_2: Cion.Lisp<'(first {:a 1})'> = 'nil'

