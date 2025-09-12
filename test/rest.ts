import type Cion from '../src/index'
import type { Rest } from '../src/index'

type testvec = [`vec`, [`prim`, true], [`prim`, 0], [`prim`, 1]];
const testrest: Rest<testvec> = [`vec`, [`prim`, 0], [`prim`, 1]];
const testrest1: Rest<Rest<testvec>> = [`vec`, [`prim`, 1]];
const testrest2: Rest<Rest<Rest<testvec>>> = [`vec`];

const maintest0_rest_0: Cion.RawLisp<'(rest [0 1 2 3])'> = ['vec', ['prim', '0000000000000001'], ['prim', '0000000000000010'],['prim', '0000000000000011']]
const maintest0_rest_1: Cion.RawLisp<'(rest [0])'> = ['vec']

const maintest_rest_0: Cion.Lisp<`(rest [0 1 2])`> = '[1 2]'
const maintest_rest_1: Cion.Lisp<`(rest [0])`> = '[]'
const maintest_rest_2: Cion.Lisp<`(rest [])`> = 'nil'

