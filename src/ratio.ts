import * as Bit from './bit.js'
import * as decimal from './decimal.js'
import * as u from './util.js'

// -----------
// -- util
// -----------
export type IntBitString = string // [todo]
export type Nat = IntBitString // [todo]
export type Ratio = [IntBitString, IntBitString]
export type Number = Nat | Ratio
export type DivByZero = 'nil'
export type IntZero = '0000000000000000'
export type IntOne = '0000000000000001'
export type RatioZero = [IntZero, IntOne]
export type RatioOne =  [IntOne, IntOne]

export type Scaling<
  X extends Ratio
, Y extends Ratio> =
  [X, Y] extends [ [ infer xc extends IntBitString
                            , infer xm extends IntBitString ]
                 , [ infer yc extends IntBitString
                            , infer ym extends IntBitString ] ]
    ? xm extends ym
      ? [X, Y]
    : Bit.BitMul<xm, ym> extends infer SM
      ? [ [Bit.BitMul<xc, ym>, SM]
        , [Bit.BitMul<yc, xm>, SM]]
    : never
  : never

// [note] to debug
export type DecimalRatio<
  Z extends [Ratio, Ratio] | Ratio> =
  Z extends [ [ infer xc extends IntBitString
              , infer xm extends IntBitString ]
            , [ infer yc extends IntBitString
              , infer ym extends IntBitString ] ]
    ? [ [decimal.BtoD<xc>, decimal.BtoD<xm>]
      , [decimal.BtoD<yc>, decimal.BtoD<ym>]]
  : Z extends [ infer xc extends IntBitString
              , infer xm extends IntBitString]
    ? [ decimal.BtoD<xc>, decimal.BtoD<xm> ]
  : never

export type ForceRatio<
  Z extends IntBitString | Ratio> =
  Z extends IntBitString
    ? [Z, decimal.DtoB<'1'>]
  : Z extends [infer xc extends IntBitString, infer xm extends IntBitString]
    ? xc extends decimal.DtoB<'0'>
      ? [xc, decimal.DtoB<'1'>]
    : xm extends decimal.DtoB<'0'>
      ? DivByZero
    : Z
  : never

// [note]
// This uses `BitDiv` which truncates the decimal part, so it behaves like a step function.
// [note]
// this is trunc, not floor.
export type Trunc<Z extends IntBitString | Ratio> = ForceNat<Z>
export type ForceNat<
  Z extends IntBitString | Ratio> =
  Z extends [ infer xc extends IntBitString
            , infer xm extends IntBitString]
    ? xm extends decimal.DtoB<'0'>
      ? DivByZero
    : Bit.BitDiv<xc, xm>
  : Z extends IntBitString
    ? Z
  : never

// [todo]
const test0: decimal.BtoD<Bit.BitDiv<decimal.DtoB<'11'>, decimal.DtoB<'2'>>> = '5'
const test1: decimal.BtoD<Bit.BitDiv<decimal.DtoB<'11'>, decimal.DtoB<'20'>>> = '0'
const test2: decimal.BtoD<Bit.BitDiv<decimal.DtoB<'10'>, decimal.DtoB<'3'>>> = '3'
const test3: decimal.BtoD<Bit.BitDiv<decimal.DtoB<'10'>, decimal.DtoB<'-3'>>> = '-3'

export type Floor<
  Z extends IntBitString | Ratio> =
  Z extends IntBitString
    ? Z
  : Z extends Ratio & [infer X extends IntBitString, infer Y extends IntBitString]
    ? [GCM<X,Y>, Bit.BitDiv<X,Y>] extends [infer tGCM extends IntBitString, infer tDiv extends IntBitString]
      ? [Bit.BitEq<tGCM, Y>, Bit.BitLT<X, '0'> & Bit.BitLT<Y, '0'>] extends [false, never]
        ? Bit.BitDec<tDiv>
      : tDiv
    : never
  : never

export type RatioStr<
  XY extends Ratio> =
  XY extends [infer x extends IntBitString
             , infer y extends IntBitString]
    ? Bit.BitEq<y, '0000000000000001'> extends true
      ? `${decimal.BitToDecimal<x>}`
    : `${decimal.BtoD<x>}/${decimal.BtoD<y>}`
  : never

export type SimplifyStr<
  X extends Number> =
  X extends Nat
    ? `${decimal.BtoD<X>}`
  : X extends Ratio
    ? Commonize<X> extends infer Commonized extends Ratio
      ? RatioStr<Commonized>
    : never
  : never

// [todo] roughly
export type _GCM<
  x extends IntBitString
, y extends IntBitString
, i extends IntBitString = y> =
  i extends '0000000000000001'
    ? i
  : [ Bit.BitMod<y, i>
    , Bit.BitMod<x, i>] extends ['0000000000000000', '0000000000000000']
    ? i
  : _GCM<x, y, Bit.BitDec<i>>
// greatest common measure
export type GCM<
  X extends IntBitString
, Y extends IntBitString> =
  Bit.BitLT<X, Y> extends true
    ? _GCM<Bit.BitAbs<X>, Bit.BitAbs<Y>>
  : _GCM<Bit.BitAbs<Y>, Bit.BitAbs<X>>

// least common multiple
export type LCD<
  Z extends IntBitString | Ratio> =
  Z extends IntBitString
    ? Z
  : Z extends [ infer xc extends IntBitString
              , infer xm extends IntBitString]
    ? Bit.BitIsZero<xc> extends true
      ? RatioZero
    : Bit.BitIsZero<xm> extends true
      ? 'nil'
    : GCM<xc, xm> extends infer gcm extends IntBitString
      ? [ Bit.BitLT<xc, '0000000000000000'>
        , Bit.BitLT<xm, '0000000000000000'> ] extends [true, true]
        ? [Bit.BitDiv<Bit.BitAbs<xc>, gcm>, Bit.BitDiv<Bit.BitAbs<xm>, gcm>]
      : [ Bit.BitLT<xc, '0000000000000000'>
        , Bit.BitLT<xm, '0000000000000000'> ] extends [false, true]
        ? [Bit.BitDiv<Bit.BitRevSign<xc>, gcm>, Bit.BitDiv<Bit.BitAbs<xm>, gcm>]
      : [Bit.BitDiv<xc, gcm>, Bit.BitDiv<xm, gcm>]
    : 'nil'
  : never

export type Commonize<
  S extends Ratio> =
  S extends [infer c extends IntBitString, infer d extends IntBitString]
    ? Bit.BitIsZero<c> extends true
      ? RatioZero
    : GCM<c,d> extends infer bc extends IntBitString
      ? [Bit.BitDiv<c, bc>, Bit.BitDiv<d, bc>, Bit.BitLT<c, '0000000000000000'>, Bit.BitLT<d, '0000000000000000'>] extends [infer cr extends IntBitString, infer dr extends IntBitString, infer cs extends boolean, infer ds extends boolean]
        ? cs & ds extends never
          ? ds extends true
            ? [Bit.BitRevSign<cr>, Bit.BitRevSign<dr>]
          : [cr, dr]
        : [Bit.BitAbs<cr>, Bit.BitAbs<dr>]
      : never
    : never
  : never

// -----------
// -- main
// -----------

export type Add<
  X extends Ratio
, Y extends Ratio> =
  Scaling<X, Y> extends [ [ infer xc extends IntBitString
                            , infer xm extends IntBitString ]
                        , [ infer yc extends IntBitString
                            , infer ym extends IntBitString ] ]
    ? [Bit.BitAdd<xc, yc>, xm]
  : never

export type Sub<
  X extends Ratio
, Y extends Ratio> =
  Scaling<X, Y> extends [ [ infer xc extends IntBitString
                            , infer xm extends IntBitString ]
                        , [ infer yc extends IntBitString
                            , infer ym extends IntBitString ] ]
    ? [Bit.BitSub<xc, yc>, xm]
  : never

export type Mul<
  X extends Ratio
, Y extends Ratio> =
  [X, Y] extends [ [ infer xc extends IntBitString
                            , infer xm extends IntBitString ]
                        , [ infer yc extends IntBitString
                            , infer ym extends IntBitString ] ]
    ? Bit.BitIsZero<xc> | Bit.BitIsZero<yc> extends false
      ? [ Bit.BitMul<xc, yc>
        , Bit.BitMul<xm, ym>]
    : RatioZero
  : never

export type Div<
  X extends Ratio
, Y extends Ratio> =
  [X, Y] extends [ [ infer xc extends IntBitString
                            , infer xm extends IntBitString ]
                        , [ infer yc extends IntBitString
                            , infer ym extends IntBitString ] ]
    ? '0000000000000000' extends yc | xm
      ? DivByZero
    : [Bit.BitMul<xc, ym>, Bit.BitMul<xm, yc>]
  : never

// -------------------------------
// -- than : >, <, >=, <=, =
// --------------------------------

export type BitR<
  xc extends IntBitString
, yc extends IntBitString
, r extends '>' | '<' | '>=' | '<=' | '='> =
  r extends '>'
    ? Bit.BitGT<xc, yc>
  : r extends '<'
    ? Bit.BitLT<xc, yc>
  : r extends '>='
    ? Bit.BitGTE<xc, yc>
  : r extends '<='
    ? Bit.BitLTE<xc, yc>
  : r extends '='
    ? Bit.BitEq<xc, yc>
  : never

export type Relation<
  X extends Number
, Y extends Number
, r extends '>' | '<' | '>=' | '<=' | '='> =
  [X, Y] extends [infer x extends Nat, infer y extends Nat]
    ? BitR<x,y,r>
  : [X, Y] extends [infer x extends Ratio, infer y extends Ratio]
    ? Scaling<x, y> extends [ [ infer xc extends IntBitString
                            , infer xm extends IntBitString ]
                        , [ infer yc extends IntBitString
                            , infer ym extends IntBitString ] ]
      ? BitR<xc,yc,r>
    : never
  : never

// -------------------
// -- abs
// -------------------

export type Abs<
  X extends Number> =
  X extends IntBitString
    ? Bit.BitAbs<X>
  : X extends [infer t  extends IntBitString, infer d extends IntBitString]
    ? Commonize<[Bit.BitAbs<t>, Bit.BitAbs<d>]>
  : never

export type Not<
  X extends Number> =
  X extends IntBitString
    ? Bit.BitRevSign<X>
  : X extends Ratio & [infer t  extends IntBitString, infer d extends IntBitString]
    ? Commonize<[Bit.BitRevSign<t>, d]>
  : never

// ---------------
// --check
// -----------

export type IsZero<
  X extends Number> =
  X extends IntBitString
    ? Bit.BitIsZero<X>
  : X extends Ratio & [infer t extends IntBitString, infer _]
    ? Bit.BitIsZero<t>
  : never

export type IsInt<
  X extends Number> =
  X extends IntBitString
    ? true
  : X extends Ratio
    ? Commonize<X> extends [infer _t, infer u extends IntBitString]
      ? u extends IntOne
        ? true
      : false
    : false
  : false

export type IsNat<
  X extends Number> =
  [u.Equal<IsInt<X>, true>, u.Equal<Relation<X, RatioZero, '>='>, true>] extends [true, true]
    ? true
  : false

export * as ratio from './ratio.js'
