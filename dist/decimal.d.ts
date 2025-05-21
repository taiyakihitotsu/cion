import Bit from "./bit";
import Peano from "./peano";
import type Util from './util';
declare namespace Decimal {
    type D10 = '0000000000001010';
    type D100 = '0000000001100100';
    type D1000 = '0000001111101000';
    type D10000 = '0010011100010000';
    type MapMul<V extends string[], B extends string, R extends string[] = []> = V extends [infer H, ...infer T] ? Bit.BitMul<H extends string ? H : never, B> extends infer M ? T['length'] extends 0 ? [...R, M] : MapMul<T extends string[] ? T : never, B, [...R, M extends string ? M : never]> : never : never;
    type DecimalTable1 = ['0', '1', '10', '11', '100', '101', '110', '111', '1000', '1001'];
    type DecimalTable2 = MapMul<DecimalTable1, D10>;
    type DecimalTable3 = MapMul<DecimalTable1, D100>;
    type DecimalTable4 = MapMul<DecimalTable1, D1000>;
    type DecimalTable5 = MapMul<['00', '01', '10', '11', '00', '00', '00', '00', '00', '00'], D10000>;
    type DecimalTables = {
        0: DecimalTable1;
        1: DecimalTable2;
        2: DecimalTable3;
        3: DecimalTable4;
        4: DecimalTable5;
    };
    type StrLen<S extends string, I = null> = S extends `${infer _}${infer T}` ? StrLen<T, Peano.inc<I>> : I;
    type PeanoToDecimal<N> = N extends null ? 0 : N extends [null] ? 1 : N extends [[null]] ? 2 : N extends [[[null]]] ? 3 : N extends [[[[null]]]] ? 4 : N extends [[[[[null]]]]] ? 5 : N extends [[[[[[null]]]]]] ? 6 : N extends [[[[[[[null]]]]]]] ? 7 : N extends [[[[[[[[null]]]]]]]] ? 8 : N extends [[[[[[[[[null]]]]]]]]] ? 9 : never;
    type PeanoLimited = [[[[[null]]]]];
    type DeciMaxes = ['3', '2', '7', '6', '7'];
    type DeciOvers = [
        '4' | '5' | '6' | '7' | '8' | '9',
        '3' | '4' | '5' | '6' | '7' | '8' | '9',
        '8' | '9',
        '7' | '8' | '9',
        '8' | '9'
    ];
    type Digit1 = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';
    type DecimalToBitError0 = 'DecimalToBitError0';
    type DecimalToBitError11 = 'DecimalToBitError11';
    export type _DecimalToBit<S extends string, IsLimited extends boolean = false, Ret extends string = '0'> = true extends Peano.gethan<StrLen<S>, [PeanoLimited]> ? {
        error: DecimalToBitError0;
        message: 'greater than the max of unsigned-16-bit-number.';
    } : true extends Util.Equal<StrLen<S>, PeanoLimited> | IsLimited ? S extends `${infer F}${infer R}` ? F extends DeciOvers[PeanoToDecimal<Peano.min<PeanoLimited, StrLen<S>>>] ? {
        error: DecimalToBitError0;
        message: 'greater than the max of unsigned-16-bit-number.';
    } : F extends Digit1 ? PeanoToDecimal<Peano.dec<StrLen<S>>> extends infer D ? _DecimalToBit<R, F extends DeciMaxes[PeanoToDecimal<Peano.min<PeanoLimited, StrLen<S>>>] ? true : false, Bit.BitAdd<Ret, D extends keyof DecimalTables ? F extends keyof DecimalTables[D] ? DecimalTables[D][F] : '' : ''>> : never : never : Ret : S extends `${infer F}${infer R}` ? F extends Digit1 ? PeanoToDecimal<Peano.dec<StrLen<S>>> extends infer D ? _DecimalToBit<R, false, Bit.BitAdd<Ret, D extends keyof DecimalTables ? F extends keyof DecimalTables[D] ? DecimalTables[D][F] : 'never' : 'never'>> : never : never : Ret;
    export type DecimalToBit<S extends string> = S extends `${infer H}${infer T}` ? H extends '-' ? _DecimalToBit<T> extends infer retT ? retT extends string ? Bit.BitSub<'1000000000000000', retT> extends `${infer _}${infer rT}` ? `1${rT}` : never : retT : never : _DecimalToBit<S> : DecimalToBitError11;
    type DigitTable = ['1', D10, D100, D1000, D10000];
    type _DigitKeys = [4, 3, 2, 1, 0];
    type DigitKidx = 0 | 1 | 2 | 3 | 4;
    type _BitToDecimal<S extends string, Ret extends string = '0', Keys = _DigitKeys, Cul = null> = Keys extends [infer K, ...infer R] ? Bit.BitSub<S, DigitTable[K extends DigitKidx ? K : never]> extends infer u ? Bit.BitLTE<u extends string ? u : never, '0'> extends true ? _BitToDecimal<S, `${Ret}${PeanoToDecimal<Cul>}`, R> : _BitToDecimal<u extends string ? u : never, Ret, Keys, Peano.inc<Cul>> : never : Ret extends string ? Ret : never;
    type TrimZero<S extends string> = S extends `${infer H}${infer T}` ? H extends '0' ? TrimZero<T> : S : S;
    export type rBitToDecimal<S extends string> = _BitToDecimal<S> extends string & infer s ? TrimZero<s extends string ? s : never> extends infer trimed ? trimed extends '' ? '0' : trimed : never : never;
    export type BitToDecimal<S extends string> = Bit.BitFill<S, Peano.T16> extends `${infer H}${infer T}` ? H extends '1' ? `-${rBitToDecimal<Bit.BitAdd<'1', Bit.BitSub<'0111111111111111', `0${T}`>>>}` : rBitToDecimal<S> : never;
    export type IsBitExpr<S extends string> = S extends `${infer H}${infer T}` ? H extends '0' | '1' ? T extends '' ? H extends '0' | '1' ? true : false : IsBitExpr<T> : false : false;
    export {};
}
export default Decimal;
