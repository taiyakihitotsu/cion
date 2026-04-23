import type {Equal} from '../../src/util.js'
import type {BitRevSign} from '../../src/bit/index.js'

const testbitrevsign0: true = {} as Equal<BitRevSign<'0000'>, '0000000000000000'>
const testbitrevsign1: true = {} as Equal<BitRevSign<'1001'>, '1111111111110111'>
