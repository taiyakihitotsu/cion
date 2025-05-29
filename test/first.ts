import type Cion from '../src/index'
import type { First } from '../src/index'

type testvec = [`vec`, [`prim`, true], [`prim`, 0], [`prim`, 1]];
const testfirst: First<testvec> = [`prim`, true];

const maintest0_first_0: Cion.RawLisp<'(first [0 1 2])'> = ['prim', '0000000000000000']
const maintest0_first_1: Cion.RawLisp<'(first [])'> = []
