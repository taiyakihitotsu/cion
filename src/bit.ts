import type Peano from "./peano";

// -------------------------
// bit ops
type BitOr<B, C> = B extends `${infer BH}${infer BR}`
  ? C extends `${infer CH}${infer CR}`
    ? CH extends `1`
      ? `1${BitOr<BR, CR>}`
      : BH extends `1`
        ? `1${BitOr<BR, CR>}`
        : `0${BitOr<BR, CR>}`
    : ``
  : ``;
// test
const bitor1: BitOr<`1`, `1`> = `1`;
const bitor2: BitOr<`1`, `0`> = `1`;
const bitor3: BitOr<`0`, `1`> = `1`;
const bitor4: BitOr<`0`, `0`> = `0`;
const bitor5: BitOr<`010`, `000`> = `010`;
const bitor6: BitOr<`111`, `111`> = `111`;
const bitor7: BitOr<`110`, `110`> = `110`;
const bitor8: BitOr<`000`, `000`> = `000`;

type BitAnd<B, C> = B extends `${infer BH}${infer BR}`
  ? C extends `${infer CH}${infer CR}`
    ? CH extends `1`
      ? BH extends `1`
        ? `1${BitAnd<BR, CR>}`
        : `0${BitAnd<BR, CR>}`
      : `0${BitAnd<BR, CR>}`
    : ``
  : ``;
// test
const bitand1: BitAnd<`1`, `1`> = `1`;
const bitand2: BitAnd<`1`, `0`> = `0`;
const bitand3: BitAnd<`0`, `1`> = `0`;
const bitand4: BitAnd<`0`, `0`> = `0`;
const bitand5: BitAnd<`010`, `000`> = `000`;
const bitand6: BitAnd<`111`, `111`> = `111`;
const bitand7: BitAnd<`110`, `110`> = `110`;
const bitand8: BitAnd<`000`, `000`> = `000`;

type BitXor<B, C> = B extends `${infer BH}${infer BR}`
  ? C extends `${infer CH}${infer CR}`
    ? CH extends BH
      ? `0${BitXor<BR, CR>}`
      : `1${BitXor<BR, CR>}`
    : ``
  : ``;
// test
const bitxor1: BitXor<`1`, `1`> = `0`;
const bitxor2: BitXor<`1`, `0`> = `1`;
const bitxor3: BitXor<`0`, `1`> = `1`;
const bitxor4: BitXor<`0`, `0`> = `0`;
const bitxor5: BitXor<`010`, `000`> = `010`;
const bitxor6: BitXor<`111`, `111`> = `000`;
const bitxor7: BitXor<`110`, `110`> = `000`;
const bitxor8: BitXor<`000`, `000`> = `000`;
const bitxor9: BitXor<`101`, `001`> = `100`;
const bitxor10: BitXor<`00111`, `00101`> = `00010`;

type BitShiftLeftOne<B> = B extends `${infer H}`
  ? `${H}0` extends `${infer C}${infer D}`
    ? D
    : never
  : never;
// test
const bitshiftl0: BitShiftLeftOne<`1111`> = `1110`;
const bitshiftl1: BitShiftLeftOne<`0000`> = `0000`;
const bitshiftl2: BitShiftLeftOne<`1010`> = `0100`;
// todo : N pattern, with Peano

type BitNot<B> = B extends `0`
  ? `1`
  : B extends `1`
    ? `0`
    : B extends `${infer H}${infer T}`
      ? `${BitNot<H>}${BitNot<T>}`
      : never;
// test
const bitnot0: BitNot<"0"> = "1";
const bitnot1: BitNot<"1"> = "0";
const bitnot2: BitNot<"11000"> = "00111";

type BitEq<B, C> = B extends ""
  ? C extends ""
    ? true
    : false
  : B extends `${infer HB}${infer TB}`
    ? C extends `${infer HC}${infer TC}`
      ? C extends B
        ? B extends C
          ? BitEq<TB, TC>
          : false
        : false
      : false
    : false;
// test
const biteq0: BitEq<"0", "0"> = true;
const biteq1: BitEq<"0", "1"> = false;
const biteq2: BitEq<"1", "0"> = false;
const biteq3: BitEq<"1", "1"> = true;
const biteq4: BitEq<"00", "10"> = false;
const biteq5: BitEq<"11", "11"> = true;
const biteq6: BitEq<"01", "10"> = false;
const biteq7: BitEq<"", "1"> = false;
const biteq8: BitEq<"0", ""> = false;
// todo : does init arg need?
const biteq9: BitEq<"", ""> = true;

type BitLen<B, count = Peano.T0> = B extends `${infer HB}${infer TB}`
  ? HB extends "0" | "1"
    ? BitLen<TB, Peano.inc<count>>
    : never
  : count;
// test
const bitlen0: BitLen<"00000000"> = [[[[[[[[null]]]]]]]];

type BitGthan<B, C> = Peano.gthan<BitLen<B>, BitLen<C>>;
// test
const bitgthan0: BitGthan<"000", "000"> = false;
const bitgthan1: BitGthan<"000", "0001"> = false;
const bitgthan2: BitGthan<"0001", "000"> = true;
// todo : they should be an error.
const bitgthan3: BitGthan<"", "000"> = false;
const bitgthan4: BitGthan<"", ""> = false;
const bitgthan5: BitGthan<"1", ""> = false;

type _BitNeedFill<B, L> = Peano.min<BitLen<B>, L>;
// test
const bitneedfill0: _BitNeedFill<"00000000", [[[null]]]> = [[[[[null]]]]];

type BitPadding<B extends string, P = Peano.T0, F = "0"> = P extends Peano.T0
  ? B
  : F extends "0" | "1"
    ? BitPadding<`${F}${B}`, Peano.dec<P>, F>
    : never;
// test
const bitpadding0: BitPadding<"10101", [null]> = "010101";
const bitpadding1: BitPadding<"10101", [[null]]> = "0010101";
const bitpadding2: BitPadding<"10101", [[null]], "1"> = "1110101";

type BitUniform<B extends string, C extends string> = BitGthan<
  B,
  C
> extends true
  ? [B, BitPadding<C, Peano.min<BitLen<B>, BitLen<C>>>]
  : [BitPadding<B, Peano.min<BitLen<C>, BitLen<B>>>, C];
// test
const bituniform0: BitUniform<"1111", "00000"> = ["01111", "00000"];
const bituniform1: BitUniform<"001111", "00000"> = ["001111", "000000"];
const bituniform2: BitUniform<"001111", "111100"> = ["001111", "111100"];
// todo : should be an error, and inconsistency in current.
// const bituniform3: BitUniform<'', '111100'> = ['000000', '111100']
// const bituniform4: BitUniform<'001111', ''> = [never, '']

type BitCut<B, P = Peano.T0> = P extends Peano.T0
  ? B
  : B extends `${infer H}${infer T}`
    ? BitCut<T, Peano.dec<P>>
    : never;
// test
const bitcut0: BitCut<"11111", Peano.T0> = "11111";
const bitcut1: BitCut<"11111", [null]> = "1111";
// const bitcut2: BitCut<"11111", [null]> = "111" // err
// const bitcut3: BitCut<"", [null]> = null as never

// CONSTANTS.
type T8 = [[[[[[[[Peano.T0]]]]]]]];
type T16 = Peano.mul<T8, [[null]]>;
type T32 = Peano.mul<T16, [[null]]>;
type MAX = T8;
type _Zero = BitPadding<"0", T32>;

// todo
type BitIsZero<B extends string> = BitUniform<_Zero, B> extends [
  infer Z,
  infer U,
]
  ? BitEq<_Zero, U>
  : never;
// test
const bitiszero0: BitIsZero<"111"> = false;
const bitiszero1: BitIsZero<"000"> = true;

type BitFill<B extends string, N = MAX> = BitPadding<B, T32> extends infer _tB
  ? BitCut<_tB, Peano.min<BitLen<_tB>, MAX>>
  : never;
// test
const bitfill0: BitFill<"1111", MAX> = "00001111";
const bitfill1: BitFill<"0000", MAX> = "00000000";
const bitfill2: BitFill<"111", MAX> = "00000111";
const bitfill3: BitFill<"11", MAX> = "00000011";
const bitfill4: BitFill<"1", MAX> = "00000001";
// fixme : want to spit an error
const bitfill5: BitFill<"", MAX> = "00000000";

// note : unsinged
type _BitAdd<B, C> = BitXor<B, C> extends infer _Xor
  ? BitAnd<B, C> extends infer _And
    ? BitShiftLeftOne<_And> extends string & infer _Carry extends string
      ? BitIsZero<
          BitUniform<_Carry, "0"> extends [infer _C extends string, infer _R]
            ? _C
            : never
        > extends true
        ? _Xor
        : _BitAdd<_Xor, _Carry>
      : 0
    : 1
  : 2;

// test
const _bitadd0: _BitAdd<"00111", "00101"> = "01100";
const _bitadd1: _BitAdd<"00110", "00001"> = "00111";
const _bitadd2: _BitAdd<"00000", "00000"> = "00000";
const _bitadd3: _BitAdd<"11111", "11111"> = "11110";

type BitAdd<B extends string, C extends string, M = MAX> = BitFill<B, M> extends infer _tB ? BitFill<C, M> extends infer _tC ? _BitAdd<_tB, _tC> : never : never
// test
// 7,5,12
// 6,1,7
// 0,0,0
// 31,31,62
const bitadd0: BitAdd<"00111", "00101"> = "00001100";
const bitadd1: BitAdd<"00110", "00001"> = "00000111";
const bitadd2: BitAdd<"00000", "00000"> = "00000000";
const bitadd3: BitAdd<"11111", "11111"> = "00111110"; // shift.
