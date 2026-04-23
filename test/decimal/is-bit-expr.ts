import type {Equal} from '../../src/util.js'
import type { IsBitExpr } from '../../src/decimal/index.js'

const isbitexpr_test_0: true = {} as Equal<IsBitExpr<'00010'>, true>
const isbitexpr_test_1: true = {} as Equal<IsBitExpr<'00010a'>, false>
const isbitexpr_test_2: true = {} as Equal<IsBitExpr<'a00010'>, false>
const isbitexpr_test_3: true = {} as Equal<IsBitExpr<''>, false>
