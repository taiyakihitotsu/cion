import type {Equal} from '../src/util.js'
import type {Nil, BitOr, BitAnd, BitXor, BitShiftLeftOne, BitShiftLeft, BitNot, BitEq, BitLen, BitLenGthan, _BitNeedFill, BitPadding, BitUniform, BitCut, BitIsZero, BitFill, _BitAdd, BitAdd, BitSub, BitGTE, BitGT, BitLT, BitLTE, BitMul, _BitShiftRight, BitRevSign, BitDiv, BitMod, BitDec, BitInc} from '../src/bit.js'
import {CurPad} from '../src/bit.js'
import type * as Peano from "../src/peano.js";

const bitor1: true = {} as Equal<BitOr<`1`, `1`>, `1`>
const bitor2: true = {} as Equal<BitOr<`1`, `0`>, `1`>
const bitor3: true = {} as Equal<BitOr<`0`, `1`>, `1`>
const bitor4: true = {} as Equal<BitOr<`0`, `0`>, `0`>
const bitor5: true = {} as Equal<BitOr<`010`, `000`>, `010`>
const bitor6: true = {} as Equal<BitOr<`111`, `111`>, `111`>
const bitor7: true = {} as Equal<BitOr<`110`, `110`>, `110`>
const bitor8: true = {} as Equal<BitOr<`000`, `000`>, `000`>

const bitand1: true = {} as Equal<BitAnd<`1`, `1`>, `1`>
const bitand2: true = {} as Equal<BitAnd<`1`, `0`>, `0`>
const bitand3: true = {} as Equal<BitAnd<`0`, `1`>, `0`>
const bitand4: true = {} as Equal<BitAnd<`0`, `0`>, `0`>
const bitand5: true = {} as Equal<BitAnd<`010`, `000`>, `000`>
const bitand6: true = {} as Equal<BitAnd<`111`, `111`>, `111`>
const bitand7: true = {} as Equal<BitAnd<`110`, `110`>, `110`>
const bitand8: true = {} as Equal<BitAnd<`000`, `000`>, `000`>

const bitxor1: true = {} as Equal<BitXor<`1`, `1`>, `0`>
const bitxor2: true = {} as Equal<BitXor<`1`, `0`>, `1`>
const bitxor3: true = {} as Equal<BitXor<`0`, `1`>, `1`>
const bitxor4: true = {} as Equal<BitXor<`0`, `0`>, `0`>
const bitxor5: true = {} as Equal<BitXor<`010`, `000`>, `010`>
const bitxor6: true = {} as Equal<BitXor<`111`, `111`>, `000`>
const bitxor7: true = {} as Equal<BitXor<`110`, `110`>, `000`>
const bitxor8: true = {} as Equal<BitXor<`000`, `000`>, `000`>
const bitxor9: true = {} as Equal<BitXor<`101`, `001`>, `100`>
const bitxor10: true = {} as Equal<BitXor<`00111`, `00101`>, `00010`>

const bitshiftlg0: true = {} as Equal<BitShiftLeftOne<`1111`>, `1110`>
const bitshiftlg1: true = {} as Equal<BitShiftLeftOne<`0000`>, `0000`>
const bitshiftlg2: true = {} as Equal<BitShiftLeftOne<`1010`>, `0100`>
// todo : true = {} as Equal<N pattern, with Peano

// test
const bitshiftl0: true = {} as Equal<BitShiftLeft<`1111`, [[null]]>, `1100`>
const bitshiftl1: true = {} as Equal<BitShiftLeft<`0000`, [null]>, `0000`>
const bitshiftl2: true = {} as Equal<BitShiftLeft<`1010`, [null]>, `0100`>
const bitshiftl3: true = {} as Equal<BitShiftLeft<`1111`, [[[null]]]>, `1000`>
const bitshiftl4: true = {} as Equal<BitShiftLeft<`1111`, [[[[null]]]]>, `0000`>
// fixme : true = {} as Equal<should it be an error ?
const bitshiftl5: true = {} as Equal<BitShiftLeft<`1111`, [[[[[null]]]]]>, `0000`>

// test
const bitnot0: true = {} as Equal<BitNot<"0">, "1">
const bitnot1: true = {} as Equal<BitNot<"1">, "0">
const bitnot2: true = {} as Equal<BitNot<"11000">, "00111">
 // test
const biteq0: true = {} as Equal<BitEq<"0", "0">, true>
const biteq1: true = {} as Equal<BitEq<"0", "1">, false>
const biteq2: true = {} as Equal<BitEq<"1", "0">, false>
const biteq3: true = {} as Equal<BitEq<"1", "1">, true>
const biteq4: true = {} as Equal<BitEq<"00", "10">, false>
const biteq5: true = {} as Equal<BitEq<"11", "11">, true>
const biteq6: true = {} as Equal<BitEq<"01", "10">, false>
const biteq7: true = {} as Equal<BitEq<"", "1">, false>
const biteq8: true = {} as Equal<BitEq<"0", "">, false>
// todo : true = {} as Equal<does init arg need?
const biteq9: true = {} as Equal<BitEq<"", "">, true>

// test
const bitlen0: true = {} as Equal<BitLen<"00000000">, [[[[[[[[null]]]]]]]]>
const bitlen1: true = {} as Equal<BitLen<"">, null>

// test
const bitgthan0: true = {} as Equal<BitLenGthan<"000", "000">, false>
const bitgthan1: true = {} as Equal<BitLenGthan<"000", "0001">, false>
const bitgthan2: true = {} as Equal<BitLenGthan<"0001", "000">, true>
// todo : true = {} as Equal<they should be an error.
const bitgthan3: true = {} as Equal<BitLenGthan<"", "000">, false>
const bitgthan4: true = {} as Equal<BitLenGthan<"", "">, false>
const bitgthan5: true = {} as Equal<BitLenGthan<"1", "">, true>
// test
const bitneedfill0: true = {} as Equal<_BitNeedFill<"00000000", [[[null]]]>, [[[[[null]]]]]>

// test
const bitpadding0: true = {} as Equal<BitPadding<"10101", [null]>, "010101">
const bitpadding1: true = {} as Equal<BitPadding<"10101", [[null]]>, "0010101">
const bitpadding2: true = {} as Equal<BitPadding<"10101", [[null]], "1">, "1110101">
// test
const bituniform0: true = {} as Equal<BitUniform<"1111", "00000">, ["01111", "00000"]>
const bituniform1: true = {} as Equal<BitUniform<"001111", "00000">, ["001111", "000000"]>
const bituniform2: true = {} as Equal<BitUniform<"001111", "111100">, ["001111", "111100"]>
// todo : true = {} as Equal<should be an error, and inconsistency in current.
// const bituniform3: true = {} as Equal<BitUniform<'', '111100'>, ['000000', '111100']
// const bituniform4: true = {} as Equal<BitUniform<'001111', ''>, [never, '']
// test
const bitcut0: true = {} as Equal<BitCut<"11111", Peano.T0>, "11111">
const bitcut1: true = {} as Equal<BitCut<"11111", [null]>, "1111">
// const bitcut2: true = {} as Equal<BitCut<"11111", [null]>, "111" // err
// const bitcut3: true = {} as Equal<BitCut<"", [null]>, null as never
// test
const bitiszero0: true = {} as Equal<BitIsZero<"111">, false>
const bitiszero1: true = {} as Equal<BitIsZero<"000">, true>

// test
const bitfill0: true = {} as Equal<BitFill<"1111", Peano.T8>, "00001111">
const bitfill1: true = {} as Equal<BitFill<"0000", Peano.T8>, "00000000">
const bitfill2: true = {} as Equal<BitFill<"111", Peano.T8>, "00000111">
const bitfill3: true = {} as Equal<BitFill<"11", Peano.T8>, "00000011">
const bitfill4: true = {} as Equal<BitFill<"1", Peano.T8>, "00000001">

// test
const _bitadd0: true = {} as Equal<_BitAdd<"00111", "00101">, "01100">
const _bitadd1: true = {} as Equal<_BitAdd<"00110", "00001">, "00111">
const _bitadd2: true = {} as Equal<_BitAdd<"00000", "00000">, "00000">
const _bitadd3: true = {} as Equal<_BitAdd<"11111", "11111">, "11110">
// test
// 7,5,12
// 6,1,7
// 0,0,0
// 31,31,62
const bitadd0: true = {} as Equal<BitAdd<"00111", "00101">, `${typeof CurPad}00001100`>
const bitadd1: true = {} as Equal<BitAdd<"00110", "00001">, `${typeof CurPad}00000111`>
const bitadd2: true = {} as Equal<BitAdd<"00000", "00000">, `${typeof CurPad}00000000`>
const bitadd3: true = {} as Equal<BitAdd<"11111", "11111">, `${typeof CurPad}00111110`> // shift.
const bitadd4: true = {} as Equal<BitAdd<'1111111111111111', '0000000000000011'>, '0000000000000010'>
const bitadd5: true = {} as Equal<BitAdd<'0000000000000011','1111111111111111'>, '0000000000000010'>
const bitadd6: true = {} as Equal<BitAdd<'1111111111111111', '1111111111111111'>, '1111111111111110'>

// test
const bitsub0: true = {} as Equal<BitSub<"00111", "00101">,  `${typeof CurPad}00000010`>
const bitsub1: true = {} as Equal<BitSub<"00110", "00001">,  `${typeof CurPad}00000101`>
const bitsub2: true = {} as Equal<BitSub<"00000", "00000">,  `${typeof CurPad}00000000`>
const bitsub3: true = {} as Equal<BitSub<"11111", "11111">,  `${typeof CurPad}00000000`>
const bitsub4: true = {} as Equal<BitSub<"00111", "01000">, "1111111111111111">
const bitsub5: true = {} as Equal<BitSub<"00000", "11111">, "1111111111100001">
const bitsub6: true = {} as Equal<BitSub<"1111111111111111","0000000000000001">, '1111111111111110'>

const bitsub0gte: true = {} as Equal<BitGTE<"00111", "00101">, true>
const bitsub1gte: true = {} as Equal<BitGTE<"00110", "00001">, true>
const bitsub2gte: true = {} as Equal<BitGTE<"00000", "00000">, true>
const bitsub3gte: true = {} as Equal<BitGTE<"11111", "11111">, true>
const bitsub4gte: true = {} as Equal<BitGTE<"00111", "01000">, false>
const bitsub5gte: true = {} as Equal<BitGTE<"00000", "11111">, false>

const bitsub0gt: true = {} as Equal<BitGT<"00111", "00101">, true>
const bitsub1gt: true = {} as Equal<BitGT<"00110", "00001">, true>
const bitsub2gt: true = {} as Equal<BitGT<"00000", "00000">, false>
const bitsub3gt: true = {} as Equal<BitGT<"11111", "11111">, false>
const bitsub4gt: true = {} as Equal<BitGT<"00111", "01000">, false>
const bitsub5gt: true = {} as Equal<BitGT<"00000", "11111">, false>

const bitsub0lt: true = {} as Equal<BitLT<"00111", "00101">, false>
const bitsub1lt: true = {} as Equal<BitLT<"00110", "00001">, false>
const bitsub2lt: true = {} as Equal<BitLT<"00000", "00000">, false>
const bitsub3lt: true = {} as Equal<BitLT<"11111", "11111">, false>
const bitsub4lt: true = {} as Equal<BitLT<"00111", "01000">, true>
const bitsub5lt: true = {} as Equal<BitLT<"00000", "11111">, true>

const bitsub0lte: true = {} as Equal<BitLTE<"00111", "00101">, false>
const bitsub1lte: true = {} as Equal<BitLTE<"00110", "00001">, false>
const bitsub2lte: true = {} as Equal<BitLTE<"00000", "00000">, true>
const bitsub3lte: true = {} as Equal<BitLTE<"11111", "11111">, true>
const bitsub4lte: true = {} as Equal<BitLTE<"00111", "01000">, true>
const bitsub5lte: true = {} as Equal<BitLTE<"00000", "11111">, true>

// test
// 7,5,35
// 6,2,12
// 0,0,0
// 31,1,31
// 1,1,1
// 0,1,0
// 1,0,0
const bitmul0: true = {} as Equal<BitMul<"00111", "00101">,  `${typeof CurPad}00100011`>
const bitmul1: true = {} as Equal<BitMul<"00110", "00010">,  `${typeof CurPad}00001100`>
const bitmul2: true = {} as Equal<BitMul<"00000", "00000">,  `${typeof CurPad}00000000`>
const bitmul3: true = {} as Equal<BitMul<"11111", "00001">,  `${typeof CurPad}00011111`>
const bitmul4: true = {} as Equal<BitMul<"00001", "00001">,  `${typeof CurPad}00000001`>
const bitmul5: true = {} as Equal<BitMul<"00000", "00001">,  `${typeof CurPad}00000000`>
const bitmul6: true = {} as Equal<BitMul<"00001", "00000">,  `${typeof CurPad}00000000`>
const bitmul7: true = {} as Equal<BitMul<'1111111111111111', '1111111111111111'>, `${typeof CurPad}00000001`>
const bitmul8: true = {} as Equal<BitMul<'1111111111111111', '1111111111110000'>, `${typeof CurPad}00010000`>
const bitmul9: true = {} as Equal<BitMul<'1111111111110000', '1111111111111111'>, `${typeof CurPad}00010000`>
const bitmul10: true = {} as Equal<BitMul<'0111111111110000', '1111111111111111'>, '1000000000010000'>
const bitmul11: true = {} as Equal<BitMul<'1111111111111111','0111111111110000'>, '1000000000010000'>


const bitsr0: true = {} as Equal<_BitShiftRight<"111111", [[[[[[null]]]]]], [[null]]>, "1111">
const bitsr1: true = {} as Equal<_BitShiftRight<"111111", [[[[[[null]]]]]], [[[null]]]>, "111">

const testbitrevsign0: true = {} as Equal<BitRevSign<'0000'>, '0000000000000000'>
const testbitrevsign1: true = {} as Equal<BitRevSign<'1001'>, '1111111111110111'>

//
const testbitdiv0: true = {} as Equal<BitDiv<"00001001", "00000001">, `${typeof CurPad}00001001`>
const testbitdiv1: true = {} as Equal<BitDiv<"00001001", "00000011">, `${typeof CurPad}00000011`>
const testbitdiv2: true = {} as Equal<BitDiv<"00001001", "00000010">, `${typeof CurPad}00000100`>
const testbitdiv3: true = {} as Equal<BitDiv<"00001001", "00000000">, Nil>
const testbitdiv4: true = {} as Equal<BitDiv<"00000010", "00001010">, `${typeof CurPad}00000000`>
const testbitdiv5: true = {} as Equal<BitDiv<'0000000000000110', '1111111111111110'>, '1111111111111101'>
const testbitdiv6: true = {} as Equal<BitDiv<'1111111111111010', '00000000000000010'>, '1111111111111101'>
const testbitdiv7: true = {} as Equal<BitDiv<'1111111111111010', '1111111111111110'>, '0000000000000011'>

// [todo] `BitAbs` tests

const testbitmod0: true = {} as Equal<BitMod<"00001001", "00000001">,  `${typeof CurPad}00000000`>
const testbitmod1: true = {} as Equal<BitMod<"00001001", "00000011">,  `${typeof CurPad}00000000`>
const testbitmod2: true = {} as Equal<BitMod<"00001001", "00000010">,  `${typeof CurPad}00000001`>
const testbitmod3: true = {} as Equal<BitMod<"00001001", "00000000">, Nil>
const testbitmod4: true = {} as Equal<BitMod<"00000010", "00001010">,  `${typeof CurPad}00000010`>
const testbitmod5: true = {} as Equal<BitMod<"1111111111111110", "1111111111111010">,  `${typeof CurPad}00000010`>

const testbitdec0: true = {} as Equal<BitDec<"0000000000000001">, "0000000000000000">
const testbitdec1: true = {} as Equal<BitDec<"0000000000000000">, "1111111111111111">
const testbitdec2: true = {} as Equal<BitDec<"0000000000000100">, "0000000000000011">

const testbitinc0: true = {} as Equal<BitInc<"0000000000000000">, "0000000000000001">
const testbitinc1: true = {} as Equal<BitInc<"1111111111111111">, "0000000000000000">
const testbitinc2: true = {} as Equal<BitInc<"0000000000000011">, "0000000000000100">
