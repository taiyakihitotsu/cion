import type * as Bit from "./bit";
import type * as Peano from "./peano"
import type * as Util from './util'

type D10    = '0000000000001010'
type D100   = '0000000001100100'
type D1000  = '0000001111101000'
type D10000 = '0010011100010000'

const d10    = '0000000000001010'
const d100   = '0000000001100100'
const d1000  = '0000001111101000'
const d10000 = '0010011100010000'

type MapMul<
  V extends string[]
, B extends string
, R extends string[] = []> =
  V extends [infer H, ...infer T]
    ? Bit.BitMul<H extends string ? H : never, B> extends infer M
      ? T['length'] extends 0
        ? [...R, M]
      : MapMul<T extends string[] ? T : never, B, [...R, M extends string ? M : never]>
    : never
  : never

type DecimalTable1 = ['0','1','10','11','100','101','110','111', '1000', '1001']
type DecimalTable2 = MapMul<DecimalTable1,  D10>
type DecimalTable3 = MapMul<DecimalTable1, D100>
type DecimalTable4 = MapMul<DecimalTable1, D1000>
type DecimalTable5 = MapMul<['00', '01', '10', '11', '00','00','00','00','00','00'], D10000>
type DecimalTables = {0: DecimalTable1, 1:DecimalTable2, 2:DecimalTable3, 3:DecimalTable4, 4:DecimalTable5}

const mapmul_test_0: MapMul<DecimalTable1, D10> =
    [`0000000000000000`,
     d10,
     `0000000000010100`,
     `0000000000011110`, 
     `0000000000101000`,
     `0000000000110010`,
     `0000000000111100`,
     `0000000001000110`,
     `0000000001010000`,
     `0000000001011010`
    ]

const mapmul_test_1: MapMul<DecimalTable1, D100> =
    [`0000000000000000`,
     d100,
     `0000000011001000`,
     `0000000100101100`,
     `0000000110010000`,
     `0000000111110100`,
     `0000001001011000`,
     `0000001010111100`,
     `0000001100100000`,
     `0000001110000100`
    ]

const mapmul_test_2: MapMul<DecimalTable1, D1000> =
    [`0000000000000000`,
     d1000,
     `0000011111010000`,
     `0000101110111000`,
     `0000111110100000`,
     `0001001110001000`,
     `0001011101110000`,
     `0001101101011000`,
     `0001111101000000`,
     `0010001100101000`
    ]

const mapmul_test_3: MapMul<['00', '01', '10', '11'], D10000> =
    [`0000000000000000`,
     d10000,
     `0100111000100000`,
     `0111010100110000`
    ]


type StrLen<
  S extends string
, I = null> =
  S extends `${infer _}${infer T}`
    ? StrLen<T, Peano.inc<I>>
  : I

const strlen_test_0: StrLen<'3999'>  = [[[[null]]]]
// note : see util.ts
// 
// const eeeee0: Util.Equal<[[[[[null]]]]], [[[[null]]]]> = false
// const eeeee1: Util.Equal<[[null]], [[null]]> = true
// const eeeee2: Util.Equal<StrLen<'3333'>, StrLen<'11111'>> = false

type PeanoToDecimal<
  N> =
  N extends null
    ? 0
  : N extends [null]
    ? 1
  : N extends [[null]]
    ? 2
  : N extends [[[null]]]
    ? 3
  : N extends [[[[null]]]]
    ? 4
  : N extends [[[[[null]]]]]
    ? 5
  : N extends [[[[[[null]]]]]]
    ? 6
  : N extends [[[[[[[null]]]]]]]
    ? 7
  : N extends [[[[[[[[null]]]]]]]]
    ? 8
  : N extends [[[[[[[[[null]]]]]]]]]
    ? 9
  : never

type DecimalToPeano<
  N> =
  N extends 0
    ? null
  : N extends 1
    ? [null]
  : N extends 2
    ? [[null]]
  : N extends 3
    ? [[[null]]]
  : N extends 4
    ? [[[[null]]]]
  : N extends 5
    ? [[[[[null]]]]]
  : N extends 6
    ? [[[[[[null]]]]]]
  : N extends 7
    ? [[[[[[[null]]]]]]]
  : N extends 8
    ? [[[[[[[[null]]]]]]]]
  : N extends 9
    ? [[[[[[[[[null]]]]]]]]]
  : never


type PeanoLimited = [[[[[null]]]]]
// note : case of signed 16 bit 
type DeciMaxes = ['3', '2', '7', '6', '7']
type DeciOvers =
['4' | '5' | '6' | '7' | '8' | '9',
		  '3' | '4' | '5' | '6' | '7' | '8' | '9',
		  '8' | '9',
		  '7' | '8' | '9',
		  '8' | '9']
type Digit1 = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

type DecimalToBitError0 = 'DecimalToBitError0'
type DecimalToBitError1 = 'DecimalToBitError1'
type DecimalToBitError2 = 'DecimalToBitError2'
type DecimalToBitError3 = 'DecimalToBitError3'
type DecimalToBitError4 = 'DecimalToBitError4'
type DecimalToBitError11 = 'DecimalToBitError11'

export type _DecimalToBit<
  S extends string
, IsLimited extends boolean = false
, Ret extends string = '0'> =
  true extends Peano.gethan<StrLen<S>, [PeanoLimited]>
    ? { error: DecimalToBitError0
      , message: 'greater than the max of unsigned-16-bit-number.' }
  : true extends Util.Equal<StrLen<S>, PeanoLimited> | IsLimited
    ? S extends `${infer F}${infer R}`
      ? F extends DeciOvers[PeanoToDecimal<Peano.min<PeanoLimited, StrLen<S>>>]
        ? { error: DecimalToBitError0
          , message: 'greater than the max of unsigned-16-bit-number.' }
      : F extends Digit1
        ? PeanoToDecimal<Peano.dec<StrLen<S>>> extends infer D
          ? _DecimalToBit<R, F extends DeciMaxes[PeanoToDecimal<Peano.min<PeanoLimited, StrLen<S>>>] ? true : false, Bit.BitAdd<Ret, D extends keyof DecimalTables ? F extends keyof DecimalTables[D] ? DecimalTables[D][F] : '' : ''>>
        : never
      : never
    : Ret
  : S extends `${infer F}${infer R}`
    ? F extends Digit1
      ? PeanoToDecimal<Peano.dec<StrLen<S>>> extends infer D
        ? _DecimalToBit<R, false, Bit.BitAdd<Ret, D extends keyof DecimalTables ? F extends keyof DecimalTables[D] ? DecimalTables[D][F] : 'never' : 'never'>>
      : never
    : never
  : Ret

    
export type DecimalToBit<
  S extends string> =
  S extends `${infer H}${infer T}`
    ? H extends '-'
      ? _DecimalToBit<T> extends infer retT
        ? retT extends string
          ? Bit.BitSub<'1000000000000000', retT> extends `${infer _}${infer rT}`
            ? `1${rT}`
          : never
        : retT
      : never
    : _DecimalToBit<S>
  : DecimalToBitError11


const decimaltobit_test_0: DecimalToBit<'32111'> = `${0}111110101101111`
const decimaltobit_test_1: DecimalToBit<'39000'> = {error: 'DecimalToBitError0', message: 'greater than the max of unsigned-16-bit-number.'}
const decimaltobit_test_2: DecimalToBit<'666666'> = {error: 'DecimalToBitError0', message: 'greater than the max of unsigned-16-bit-number.'}
const decimaltobit_test_3: DecimalToBit<'8'>  = `0000000000001000`
const decimaltobit_test_4: DecimalToBit<'9'>  = `0000000000001001`
const decimaltobit_test_5: DecimalToBit<'-9'> = '1111111111110111'
const decimaltobit_test_6: DecimalToBit<'0'>  = '0000000000000000'
const decimaltobit_test_7: DecimalToBit<'-1'> = '1111111111111111'
const decimaltobit_test_8: DecimalToBit<'-111111'> = {error: 'DecimalToBitError0', message: 'greater than the max of unsigned-16-bit-number.'}
const decimaltobit_test_9: DecimalToBit<'32767'> = "0111111111111111"
const decimaltobit_test_10: DecimalToBit<'32768'> = {error: 'DecimalToBitError0', message: 'greater than the max of unsigned-16-bit-number.'}
const decimaltobit_test_11: DecimalToBit<'-32768'> = {error: 'DecimalToBitError0', message: 'greater than the max of unsigned-16-bit-number.'}
const decimaltobit_test_12: DecimalToBit<'-32767'> = "1000000000000001"


type DigitTable = ['1', D10, D100, D1000, D10000]
type _DigitKeys = [4,3,2,1,0]
type DigitKidx = 0|1|2|3|4 

type _BitToDecimal<
  S extends string
, Ret extends string = '0'
, Keys = _DigitKeys
, Cul = null> =
  Keys extends [infer K, ...infer R]
    ? Bit.BitSub<S, DigitTable[K extends DigitKidx ? K : never]> extends infer u
      ? Bit.BitLTE<u extends string ? u : never,'0'> extends true
        ? _BitToDecimal<S, `${Ret}${PeanoToDecimal<Cul>}`, R>
      : _BitToDecimal<u extends string ? u : never, Ret, Keys, Peano.inc<Cul>>
    : never
  : Ret extends string
    ? Ret
  : never

type TrimZero<
  S extends string> =
  S extends `${infer H}${infer T}`
    ? H extends '0'
      ? TrimZero<T>
    : S
  : S

export type rBitToDecimal<
  S extends string> =
  _BitToDecimal<S> extends string & infer s
    ? TrimZero<s extends string ? s : never> extends infer trimed
      ? trimed extends ''
        ? '0'
      : trimed
    : never
  : never

export type BitToDecimal<
  S extends string> =
  Bit.BitFill<S, Peano.T16> extends `${infer H}${infer T}`
    ? H extends '1'
      ? `-${rBitToDecimal<Bit.BitAdd<'1', Bit.BitSub<'0111111111111111', `0${T}`>>>}`
    : rBitToDecimal<S>
  : never

const bittodecimal_test_0: BitToDecimal<'1010'> = '10'
const bittodecimal_test_1: BitToDecimal<'0001'> = '1'
const bittodecimal_test_2: BitToDecimal<'0000'> = '0'
const bittodecimal_test_3: BitToDecimal<`0111110101101111`> = '32111'
const bittodecimal_test_4: BitToDecimal<'0111111111111111'> = '32767'
const bittodecimal_test_5: BitToDecimal<`1111110101101111`> = '-657'
const bittodecimal_test_6: BitToDecimal<'1111111111111111'> = '-1'



export type IsBitExpr<
  S extends string> =
  S extends `${infer H}${infer T}`
    ? H extends '0' | '1'
      ? T extends ''
        ? H extends '0' | '1'
          ? true
        : false
      : IsBitExpr<T>
    : false
  : false

const isbitexpr_test_0: IsBitExpr<'00010'> = true
const isbitexpr_test_1: IsBitExpr<'00010a'> = false
const isbitexpr_test_2: IsBitExpr<'a00010'> = false
const isbitexpr_test_3: IsBitExpr<''> = false

export * as Decimal from './decimal'
