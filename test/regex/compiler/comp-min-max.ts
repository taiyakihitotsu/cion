import type { regexCompiler as rc } from '../../../src/regex/index.js'
import type {Equal} from '../../../src/util.js'
import type { _1, _2, _15, _16 } from './const.js'

const test_minmax_1_2: true = {} as Equal<[typeof _1, typeof _2, 'rest'], rc.CompMinMax<'{1,2}rest'>>
const test_minmax_1_inf: true = {} as Equal<[typeof _1, '<', 'rest'], rc.CompMinMax<'{1,}rest'>>
const test_minmax_1_15: true = {} as Equal<[typeof _1, typeof _15, 'rest'], rc.CompMinMax<'{1,15}rest'>>
const test_minmax_15_16: true = {} as Equal<[typeof _15, typeof _16, 'rest'], rc.CompMinMax<'{15,16}rest'>>
const test_minmax_15_inf: true = {} as Equal<[typeof _15, '<', 'rest'], rc.CompMinMax<'{15,}rest'>>
const test_minmax_2_eq: true = {} as Equal<[typeof _2, '=', 'rest'], rc.CompMinMax<'{2}rest'>>
const test_minmax_15_eq: true = {} as Equal<[typeof _15, '=', 'rest'], rc.CompMinMax<'{15}rest'>>
