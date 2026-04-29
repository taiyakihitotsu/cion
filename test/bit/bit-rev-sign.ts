import type {Equal} from '../../src/util.js'
import type {BitRevSign} from '../../src/bit/index.js'

const testbitrevsign0: true = {} as Equal<BitRevSign<'0000'>, '0000000000000000'>
const testbitrevsign1: true = {} as Equal<BitRevSign<'1001'>, '1111111111110111'>

type Actual_RevSign_One = BitRevSign<'0001'>
const testbitrevsign_one: true = {} as Equal<Actual_RevSign_One, '1111111111111111'>

type Actual_RevSign_NegOne = BitRevSign<'1111111111111111'>
const testbitrevsign_negone: true = {} as Equal<Actual_RevSign_NegOne, '0000000000000001'>

type Actual_RevSign_MaxPos = BitRevSign<'0111111111111111'>
const testbitrevsign_maxpos: true = {} as Equal<Actual_RevSign_MaxPos, '1000000000000001'>

type Actual_RevSign_MinNeg = BitRevSign<'1000000000000000'>
const testbitrevsign_minneg: true = {} as Equal<Actual_RevSign_MinNeg, '1000000000000000'>

type Actual_RevSign_Two = BitRevSign<'0010'>
const testbitrevsign_two: true = {} as Equal<Actual_RevSign_Two, '1111111111111110'>

type Actual_RevSign_Short = BitRevSign<'1'>
const testbitrevsign_short: true = {} as Equal<Actual_RevSign_Short, '1111111111111111'>

type Actual_RevSign_Mixed = BitRevSign<'0000000000001010'>
const testbitrevsign_mixed: true = {} as Equal<Actual_RevSign_Mixed, '1111111111110110'>
