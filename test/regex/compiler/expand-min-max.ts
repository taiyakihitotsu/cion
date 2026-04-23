import type { regexCompiler as rc } from '../../../src/regex/index.js'
import type {Equal} from '../../../src/util.js'
import type { _1, _2, _15, _16 } from './const.js'

const test_expand_1_2: true = {} as Equal<['rest', ['?', 'rest']], rc.ExpandMinMax<'rest', typeof _1, typeof _2>>
const test_expand_1_inf: true = {} as Equal<['rest', ['*', 'rest']], rc.ExpandMinMax<'rest', typeof _1, '<'>>
const test_expand_1_15: true = {} as Equal<[
  'rest', 
  ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], 
  ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], 
  ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest']
], rc.ExpandMinMax<'rest', typeof _1, typeof _15>>
const test_expand_15_eq: true = {} as Equal<[
  'rest', 'rest', 'rest', 'rest', 'rest', 
  'rest', 'rest', 'rest', 'rest', 'rest', 
  'rest', 'rest', 'rest', 'rest', 'rest'
], rc.ExpandMinMax<'rest', typeof _15, '='>>

const mmetest4: true = {} as Equal<rc.ExpandMinMax<'rest', typeof _1, typeof _2>, ['rest', ['?', 'rest']]>
const mmetest5: true = {} as Equal<rc.ExpandMinMax<'rest', typeof _1, typeof _2>, ['rest', ['?', 'rest']]>
const mmetest: true = {} as Equal<rc.ExpandMinMax<'rest', typeof _1, typeof _2>, ['rest', ['?', 'rest']]>
