import * as Bit from './bit'
import * as decimal from './decimal'

// -----------
// -- util
// -----------
export type BitString = string
export type Nat = BitString
export type Ratio = [BitString, BitString]
export type Number = Nat | Ratio
export type DivByZero = 'nil'
export type RatioZero = ['0000000000000000', '0000000000000001']
export type RatioOne =  ['0000000000000001', '0000000000000001']

export type Scaling<
  X extends Ratio
, Y extends Ratio> =
  [X, Y] extends [ [ infer xc extends BitString
                            , infer xm extends BitString ]
                 , [ infer yc extends BitString
                            , infer ym extends BitString ] ]
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
  Z extends [ [ infer xc extends BitString
              , infer xm extends BitString ]
            , [ infer yc extends BitString
              , infer ym extends BitString ] ]
    ? [ [decimal.BtoD<xc>, decimal.BtoD<xm>]
      , [decimal.BtoD<yc>, decimal.BtoD<ym>]]
  : Z extends [ infer xc extends BitString
              , infer xm extends BitString]
    ? [ decimal.BtoD<xc>, decimal.BtoD<xm> ]
  : never

export type ForceRatio<
  Z extends BitString | Ratio> =
  Z extends BitString
    ? [Z, decimal.DtoB<'1'>]
  : Z extends [infer xc extends BitString, infer xm extends BitString]
    ? xc extends decimal.DtoB<'0'>
      ? [xc, decimal.DtoB<'1'>]
    : xm extends decimal.DtoB<'0'>
      ? DivByZero
    : Z
  : never

// [note]
// This uses `BitDiv` which truncates the decimal part, so it behaves like a step function.
export type ForceNat<
  Z extends BitString | Ratio> =
  Z extends [ infer xc extends BitString
            , infer xm extends BitString]
    ? xm extends decimal.DtoB<'0'>
      ? DivByZero
    : Bit.BitDiv<xc, xm>
  : Z extends BitString
    ? Z
  : never

export type RatioStr<
  XY extends Ratio> =
  XY extends [infer x extends BitString
             , infer y extends BitString]
    ? Bit.BitEq<y, '0000000000000001'> extends true
      ? `${decimal.BitToDecimal<x>}`
    : `${decimal.BtoD<x>}/${decimal.BtoD<y>}`
  : never

// [todo] roughly
export type _GCM<
  x extends BitString
, y extends BitString
, i extends BitString = y> =
  i extends '0000000000000001'
    ? i
  : [ Bit.BitMod<y, i>
    , Bit.BitMod<x, i>] extends ['0000000000000000', '0000000000000000']
    ? i
  : _GCM<x, y, Bit.BitDec<i>>
// greatest common measure
export type GCM<
  X extends BitString
, Y extends BitString> =
  Bit.BitLT<X, Y> extends true
    ? _GCM<Bit.BitAbs<X>, Bit.BitAbs<Y>>
  : _GCM<Bit.BitAbs<Y>, Bit.BitAbs<X>>
// least common multiple
export type LCD<
  Z extends BitString | Ratio> =
  Z extends BitString
    ? Z
  : Z extends [ infer xc extends BitString
              , infer xm extends BitString]
    ? Bit.BitIsZero<xc> extends true
      ? RatioZero
    : Bit.BitIsZero<xm> extends true
      ? 'nil'
    : GCM<xc, xm> extends infer gcm extends BitString
      ? [ Bit.BitLT<xc, '0000000000000000'>
        , Bit.BitLT<xm, '0000000000000000'> ] extends [true, true]
        ? [Bit.BitDiv<Bit.BitAbs<xc>, gcm>, Bit.BitDiv<Bit.BitAbs<xm>, gcm>]
      : [ Bit.BitLT<xc, '0000000000000000'>
        , Bit.BitLT<xm, '0000000000000000'> ] extends [false, true]
        ? [Bit.BitDiv<Bit.BitRevSign<xc>, gcm>, Bit.BitDiv<Bit.BitAbs<xm>, gcm>]
      : [Bit.BitDiv<xc, gcm>, Bit.BitDiv<xm, gcm>]
    : 'nil'
  : never

// [note] this is not for Ratio to Nat.
export type Commonize<
  S extends Ratio> =
  S extends [infer c extends BitString, infer d extends BitString]
    ? Bit.BitIsZero<c> extends true
      ? RatioZero
    : GCM<c,d> extends infer bc extends BitString
      ? [Bit.BitDiv<c, bc>, Bit.BitDiv<d, bc>, Bit.BitLT<c, '0000000000000000'>, Bit.BitLT<d, '0000000000000000'>] extends [infer cr extends BitString, infer dr extends BitString, infer cs extends boolean, infer ds extends boolean]
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
  Scaling<X, Y> extends [ [ infer xc extends BitString
                            , infer xm extends BitString ]
                        , [ infer yc extends BitString
                            , infer ym extends BitString ] ]
    ? [Bit.BitAdd<xc, yc>, xm]
  : never

export type Sub<
  X extends Ratio
, Y extends Ratio> =
  Scaling<X, Y> extends [ [ infer xc extends BitString
                            , infer xm extends BitString ]
                        , [ infer yc extends BitString
                            , infer ym extends BitString ] ]
    ? [Bit.BitSub<xc, yc>, xm]
  : never

export type Mul<
  X extends Ratio
, Y extends Ratio> =
  [X, Y] extends [ [ infer xc extends BitString
                            , infer xm extends BitString ]
                        , [ infer yc extends BitString
                            , infer ym extends BitString ] ]
    ? Bit.BitIsZero<xc> | Bit.BitIsZero<yc> extends false
      ? [ Bit.BitMul<xc, yc>
        , Bit.BitMul<xm, ym>]
    : RatioZero
  : never

export type Div<
  X extends Ratio
, Y extends Ratio> =
  [X, Y] extends [ [ infer xc extends BitString
                            , infer xm extends BitString ]
                        , [ infer yc extends BitString
                            , infer ym extends BitString ] ]
    ? '0000000000000000' extends yc | xm
      ? DivByZero
    : [Bit.BitMul<xc, ym>, Bit.BitMul<xm, yc>]
  : never


// -------------------------------
// -- than : >, <, >=, <=, =
// --------------------------------

export type BitR<
  xc extends BitString
, yc extends BitString
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
    ? Scaling<x, y> extends [ [ infer xc extends BitString
                            , infer xm extends BitString ]
                        , [ infer yc extends BitString
                            , infer ym extends BitString ] ]
      ? BitR<xc,yc,r>
    : never
  : never

export * as ratio from './ratio'
