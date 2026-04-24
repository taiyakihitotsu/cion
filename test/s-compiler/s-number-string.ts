import type {SNumberString} from '../../src/s-compiler/index.js'
import type {Equal} from '../../src/util.js'

// -------------------------------
// -- SNumberString
// -------------------------------

type Literal_0_0 = SNumberString<['prim', ['0000000000000000', '0000000000000000']]> // 'nil'
const expected_Nil: true = {} as Equal<Literal_0_0, 'nil'>
type Literal_0_1 = SNumberString<['prim', ['0000000000000000', '0000000000000001']]> // '0'
const expected_0: true = {} as Equal<Literal_0_1, '0'>
type Literal_1_1 = SNumberString<['prim', ['0000000000000001', '0000000000000001']]> // '1'
const expected_1: true = {} as Equal<Literal_1_1, '1'>
type Literal_8_2 = SNumberString<['prim', ['0000000000001000', '0000000000000010']]> // '4'
const expected_4: true = {} as Equal<Literal_8_2, '4'>
type Literal_8_3 = SNumberString<['prim', ['0000000000001000', '0000000000000011']]> // '8/3'
const expected_8_3: true = {} as Equal<Literal_8_3, '8/3'>
type Literal_m1_m1 = SNumberString<['prim', ['1111111111111111', '1111111111111111']]> // '1'
const expected_1_b: true = {} as Equal<Literal_m1_m1, '1'>
type Literal_m8_m2 = SNumberString<['prim', ['1111111111111000', '1111111111111110']]> // '4'
const expected_4_b: true = {} as Equal<Literal_m8_m2, '4'>
type Literal_m8_m3 = SNumberString<['prim', ['1111111111111000', '1111111111111101']]> // '8/3'
const expected_8_3_b: true = {} as Equal<Literal_m8_m3, '8/3'>
type Literal_m8_3 = SNumberString<['prim', ['1111111111111000', '0000000000000011']]> // '-8/3'
const expected_m8_3: true = {} as Equal<Literal_m8_3, '-8/3'>
type Literal_8_m3 = SNumberString<['prim', ['0000000000001000', '1111111111111101']]> // '-8/3'
const expected_m8_3_b: true = {} as Equal<Literal_8_m3, '-8/3'>
