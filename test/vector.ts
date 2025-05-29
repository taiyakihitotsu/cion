import type Cion from '../src/index'
import type { Eval } from '../src/index'
import type {LetVal,LetArg,LetForm,Each,Atom,TMap,Sexpr,TNil,Keyword,Sym,PrimString,PrimBoolean,PrimTestNumber,PrimNumber,Prim,Args,Fn,Vector,Var,Env,TNotMatch,IfForm} from '../src/sexprtypes'

const testvecvec0: Sexpr = [[['key', ':a'], ['map', [['key', ':a'], ['prim', `'-'`]]]], ['vec', ['prim', `'-'`]]]
const testvecvec1: ['vec', ...(Sexpr | Each)[]] = ['vec', [['key', ':a'], ['map', [['key', ':a'], ['prim', `'-'`]]]], ['prim', '0']]
const testvecvec2: Vector = ['vec', [['key', ':a'], ['map', [['key', ':a'], ['prim', `'-'`]]]], ['prim', '0']]
const testvecvec3: Eval<['vec', [['key', ':a'], ['map', [['key', ':a'], ['prim', `'-'`]]]], ['prim', '0']]> = ['vec', ['prim', `'-'`], ['prim', '0']]
const testvecvec4a: Eval<['vec', [['key', ':a'], ['map', [['key', ':a'], ['prim', '0001']]]]]> = ['vec', ['prim', '0001']]
const testvecvec4b: Eval<['vec', [['key', ':a'], ['map', [['key', ':a'], ['prim', '0001']]]], ['prim', '0001']]> = ['vec', ['prim', '0001'], ['prim', '0001']]
// doing
const testvecvec4c: Eval<['vec', [['key', ':a'], ['map', [['key', ':a'], ['prim', '0001']]]], ['prim', '0001'], ['prim', '0001']]> = ['vec', ['prim', '0001'], ['prim', '0001'], ['prim', '0001']]
const testvecvec5: Eval<['sym', 'x']> = ['sym', 'x']
const testvv6: Eval<['prim', '0']> = ['prim', '0']


const vectortest: Vector & [`vec`, [`prim`, `1`]] = [`vec`, [`prim`, `1`]];
const vectortest2: Vector & [`vec`, [`prim`, `1`], [`prim`, `2`]] = [
  `vec`,

  [`prim`, `1`],
  [`prim`, `2`],
];
const vectortest3: Vector & [`vec`, [`vec`, [`prim`, `2`]]] = [
  `vec`,
  [`vec`, [`prim`, `2`]],
];

const vectortest5: Vector &
  [`vec`, [`vec`, [`vec`, [`prim`, true], [`prim`, `1`]], [`prim`, `1`]]] = [
  `vec`,
  [`vec`, [`vec`, [`prim`, true], [`prim`, `1`]], [`prim`, `1`]],
];

const maintest0_vecst_0: Cion.RawLisp<"[0 1 2 3]"> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']]
const maintest1_vecst_0: Cion.RawLisp<"(vector 0 1 2 3)"> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011']]
