import type Cion from '../src/index'
import type { _Assoc } from '../src/index'

const testassoc0: _Assoc<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]], ['prim', '1'], ['prim', 10]> = ['vec', ['key', ':a'], ['prim', 10], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]
const testassoc1: _Assoc<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]], ['prim', '0'], ['prim', 10]> = ['vec', ['prim', 10], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]
const testassoc2: _Assoc<['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]], ['prim', '1111'], ['prim', 10]> = ['vec', ['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]
const testassoc3: _Assoc<['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]], ['key', ':a'], ['prim', 10]> = ['map', [['key', ':a'], ['prim', 10], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]]
const testassoc4: _Assoc<['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]], ['key', ':b'], ['prim', 10]> = ['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 10], ['key', ':c'], ['prim', 2]]]
const testassoc5: _Assoc<['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2]]], ['key', ':d'], ['prim', 10]> = ['map', [['key', ':a'], ['prim', 0], ['key', ':b'], ['prim', 1], ['key', ':c'], ['prim', 2], ['key', ':d'], ['prim', 10]]]

const maintest0_assoc_0: Cion.RawLisp<'(assoc [0 1 2] 0 99)'> = ['vec', ['prim', '0000000001100011'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
const maintest0_assoc_1: Cion.RawLisp<'(assoc [0 1 2] 3 99)'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']] // no effect
