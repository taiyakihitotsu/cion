import type Cion from '../src/index'
import type { _Update } from '../src/index'
import { AssocErrorMsg1 } from '../src/index'

const testUpdate0: _Update<['map', [['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '0001'], ['key', ':c'], ['prim', '0010']]], ['key', ':a'], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]> = ['map', [['key', ':a'], ['prim', '0000000000001001'], ['key', ':b'], ['prim', '0001'], ['key', ':c'], ['prim', '0010']]]
const testUpdate1: _Update<['vec', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '0001'], ['key', ':c'], ['prim', '0010']], ['prim', '11'], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]> = ['vec', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '0000000000001001'], ['key', ':c'], ['prim', '0010']]
const testUpdate2: _Update<['vec', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '0001'], ['key', ':c'], ['prim', '0010']], ['key', ':d'], ['fn', [['sym', 'x']], [['sym', '+'], ['sym', 'x'], ['prim', '1000']]]> = {error: 'AssocError1', message: AssocErrorMsg1, sexpr: ['vec', ['key', ':a'], ['prim', '01'], ['key', ':b'], ['prim', '0001'], ['key', ':c'], ['prim', '0010']]}

const maintest0_update_0: Cion.RawLisp<'(update [0 1 2] 1 (fn [x] (+ x 99)))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000001100100'], ['prim', '0000000000000010']]
const maintest0_update_1: Cion.RawLisp<'(update [0 1 2] 99 (fn [x] (+ x 99)))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']] // no effect

