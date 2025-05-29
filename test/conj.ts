import type Cion from '../src/index'
import type { Conj } from '../src/index'

type testvec = [`vec`, [`prim`, true], [`prim`, 0], [`prim`, 1]];

const testconj: Conj<testvec, [`prim`, false]> = [
  `vec`,
  [`prim`, true],
  [`prim`, 0],
  [`prim`, 1],
  [`prim`, false],
];
const testconj1: Conj<[`vec`], [`prim`, false]> = [`vec`, [`prim`, false]];

const maintest0_conj_0: Cion.RawLisp<'(conj [0 1] 2 3)'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']]
const maintest0_conj_1: Cion.RawLisp<'(conj [0 1] [2])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['vec', ['prim', '0000000000000010']]]

