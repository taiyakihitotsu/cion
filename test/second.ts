import type Cion from '../src/index'
import { VNil } from '../src/sexprtypes'

const maintest0_second_0: Cion.RawLisp<'(second [0 1 2])'> = ['prim', '0000000000000001']
const maintest0_second_1: Cion.RawLisp<'(second [])'> = VNil

const maintest1_second_0: Cion.Lisp<'(second [0 1 2])'> = '1'
const maintest1_second_1: Cion.Lisp<'(second [])'> = 'nil'
// @ts-expect-error:
const maintest1_second_2: Cion.Lisp<'(second {:a 1})'> = 'nil'
