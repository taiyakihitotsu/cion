import type Cion from '../src/index'
import { VNil } from '../src/sexprtypes'

const maintest0_last_0: Cion.RawLisp<'(last [0 1 2])'> = ['prim', '0000000000000010']
const maintest0_last_1: Cion.RawLisp<'(last [])'> = VNil

const maintest1_last_0: Cion.Lisp<'(last [0 1 2])'> = '2'
const maintest1_last_1: Cion.Lisp<'(last [])'> = 'nil'
// @ts-expect-error:
const maintest1_last_2: Cion.Lisp<'(last {:a 1})'> = 'nil'
