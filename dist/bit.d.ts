import type Peano from "./peano";
declare namespace Bit {
    type MAX = Peano.T16;
    type _Zero = BitPadding<"0", MAX>;
    const Pad8 = "00000000";
    const Pad16 = "0000000000000000";
    const CurPad = "00000000";
    type Min1 = '1111111111111111';
    type BitOr<B, C> = B extends `${infer BH}${infer BR}` ? C extends `${infer CH}${infer CR}` ? CH extends `1` ? `1${BitOr<BR, CR>}` : BH extends `1` ? `1${BitOr<BR, CR>}` : `0${BitOr<BR, CR>}` : `` : ``;
    type BitAnd<B, C> = B extends `${infer BH}${infer BR}` ? C extends `${infer CH}${infer CR}` ? CH extends `1` ? BH extends `1` ? `1${BitAnd<BR, CR>}` : `0${BitAnd<BR, CR>}` : `0${BitAnd<BR, CR>}` : `` : ``;
    type BitXor<B, C> = B extends `${infer BH}${infer BR}` ? C extends `${infer CH}${infer CR}` ? CH extends BH ? `0${BitXor<BR, CR>}` : `1${BitXor<BR, CR>}` : `` : ``;
    type BitShiftLeftOne<B> = B extends `${infer H}` ? `${H}0` extends `${infer C}${infer D}` ? D : never : never;
    type BitShiftLeft<B, N> = N extends Peano.T0 ? B : BitShiftLeft<BitShiftLeftOne<B>, Peano.dec<N>>;
    type BitNot<B> = B extends `0` ? `1` : B extends `1` ? `0` : B extends `${infer H}${infer T}` ? `${BitNot<H>}${BitNot<T>}` : never;
    type BitEq<B, C> = B extends "" ? C extends "" ? true : false : B extends `${infer HB}${infer TB}` ? C extends `${infer HC}${infer TC}` ? C extends B ? B extends C ? BitEq<TB, TC> : false : false : false : false;
    type BitLen<B, count = Peano.T0> = B extends `${infer HB}${infer TB}` ? HB extends "0" | "1" ? BitLen<TB, Peano.inc<count>> : never : count;
    type BitLenGthan<B, C> = Peano.gthan<BitLen<B>, BitLen<C>>;
    type _BitNeedFill<B, L> = Peano.min<BitLen<B>, L>;
    type BitPadding<B extends string, P = Peano.T0, F = "0"> = P extends Peano.T0 ? B : F extends "0" | "1" ? BitPadding<`${F}${B}`, Peano.dec<P>, F> : never;
    type BitUniform<B extends string, C extends string> = BitLenGthan<B, C> extends true ? [B, BitPadding<C, Peano.min<BitLen<B>, BitLen<C>>>] : [BitPadding<B, Peano.min<BitLen<C>, BitLen<B>>>, C];
    type BitCut<B, P = Peano.T0> = P extends Peano.T0 ? B : B extends `${infer H}${infer T}` ? BitCut<T, Peano.dec<P>> : never;
    type BitIsZero<B extends string> = BitUniform<_Zero, B> extends [
        infer Z,
        infer U
    ] ? BitEq<_Zero, U> : never;
    type BitFill<B extends string, M = MAX, tB extends string = BitPadding<B, M>> = BitCut<tB, Peano.min<BitLen<tB>, M>>;
    type _BitAdd<B, C> = BitXor<B, C> extends infer _Xor ? BitAnd<B, C> extends infer _And ? BitShiftLeftOne<_And> extends string & infer _Carry extends string ? BitIsZero<BitUniform<_Carry, "0"> extends [infer _C extends string, infer _R] ? _C : never> extends true ? _Xor : _BitAdd<_Xor, _Carry> : 0 : 1 : 2;
    type BitAdd<B extends string, C extends string, M = MAX> = BitFill<B, M> extends infer _tB ? BitFill<C, M> extends infer _tC ? _BitAdd<_tB, _tC> : never : never;
    type BitSub<B extends string, C extends string, M = MAX> = BitAdd<BitFill<B, M>, BitAdd<BitNot<BitFill<C, M>>, BitFill<"1", M>>>;
    type BitGTE<B extends string, C extends string> = BitSub<B, C> extends `${infer H}${infer _}` ? H extends '1' ? false : true : never;
    type BitGT<B extends string, C extends string> = BitGTE<B, C> extends true ? B extends C ? C extends B ? false : true : true : false;
    type BitLT<B extends string, C extends string> = BitGTE<B, C> extends true ? false : true;
    type BitLTE<B extends string, C extends string> = BitGT<B, C> extends true ? false : true;
    type BitMul<B extends string, C extends string, M = MAX, R extends string = BitFill<"0", M>, tB extends string = BitFill<B, M>, tC extends string = BitFill<C, M>, N = Peano.dec<M>> = tB extends `${infer H}${infer T}` ? H extends "/" ? R : H extends "0" ? BitMul<B, C, M, R, `${T}/`, tC, Peano.dec<N>> : H extends "1" ? BitMul<B, C, M, BitAdd<R, BitShiftLeft<tC, N>>, `${T}/`, tC, Peano.dec<N>> : never : never;
    type _BitShiftRight<B extends string, M = MAX, N = Peano.T1, D = Peano.min<M, N>, R extends string = ""> = D extends Peano.T0 ? R extends "" ? B : R : B extends `${infer H}${infer T}` ? _BitShiftRight<`${T}`, M, N, Peano.dec<D>, `${R}${H}`> : R;
    type BitRevSign<S extends string> = BitMul<S, Min1>;
    type _BitDiv<B extends string, C extends string, Ret extends string = "00000000"> = BitLT<B, C> extends true ? Ret : _BitDiv<BitSub<B, C>, C, BitAdd<Ret, "00000001">>;
    type Nil = [`prim`, `nil`];
    type BitDiv<B extends string, C extends string> = BitIsZero<C> extends true ? Nil : BitFill<B, MAX> extends `${infer bh}${infer br}` ? BitFill<C, MAX> extends `${infer ch}${infer cr}` ? _BitDiv<bh extends '1' ? BitRevSign<`${bh}${br}`> : `${bh}${br}`, ch extends '1' ? BitRevSign<`${ch}${cr}`> : `${ch}${cr}`> extends infer dd ? '0' | '1' extends bh | ch ? BitFill<BitRevSign<dd extends string ? dd : never>, MAX> : BitFill<dd extends string ? dd : never, MAX> : never : never : never;
    type _BitMod<B extends string, C extends string, Ret extends string = B> = BitLT<Ret, C> extends true ? Ret : _BitMod<B, C, BitSub<Ret, C>>;
    type BitMod<B extends string, C extends string> = BitIsZero<C> extends true ? Nil : BitFill<_BitMod<B, C>, MAX>;
}
export default Bit;
