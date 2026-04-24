import type {SIsInt} from '../../../src/compiler/index.js'
import type {Equal} from '../../../src/util.js'

const Zero_True: true = {} as Equal<true, SIsInt<'0'>>
const String_False: true = {} as Equal<SIsInt<'s'>, false>
const Redundant_True: true = {} as Equal<SIsInt<'001'>, true>
const Digits_True: true = {} as Equal<SIsInt<'38'>, true>
const Negative_True: true = {} as Equal<SIsInt<'-1'>, true>
const Rational_False: true = {} as Equal<SIsInt<'3/2'>, false>
const RationalPoint_False: true = {} as Equal<SIsInt<'1.4'>, false>
