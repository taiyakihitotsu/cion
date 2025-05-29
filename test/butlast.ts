import type Cion from '../src/index'
import type { Butlast } from '../src/index'

type testbutlastvec = [`vec`, [`prim`, 0], [`prim`, 1], ['prim', 2], ['prim', 3]]
const testbutlast0: Butlast<testbutlastvec> = [`vec`, [`prim`, 0], [`prim`, 1], ['prim', 2]];
const testbutlast1: Butlast<Butlast<testbutlastvec>> = [`vec`, [`prim`, 0], [`prim`, 1]];
const testbutlast2: Butlast<Butlast<Butlast<testbutlastvec>>> = [`vec`, [`prim`, 0]];
const testbutlast3: Butlast<Butlast<Butlast<Butlast<testbutlastvec>>>> = [`vec`];

const maintest0_butlast_0: Cion.RawLisp<'(butlast [0 1 2 3])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
const maintest0_butlast_1: Cion.RawLisp<'(butlast [0])'> = ['vec']
