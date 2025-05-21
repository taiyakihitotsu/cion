import type Bit from './bit';
import type Decimal from './decimal';
declare namespace Compiler {
    export type SPad<S extends string> = S extends ` ${infer SS}` ? SS : ` ${S}`;
    type _rec<T> = T extends {
        r: never;
    } ? never : T extends {
        r: {
            r: {
                r: {
                    r: {
                        r: {
                            r: {
                                r: {
                                    r: infer U;
                                };
                            };
                        };
                    };
                };
            };
        };
    } ? {
        r: _rec<U>;
    } : T extends {
        r: {
            r: {
                r: {
                    r: infer U;
                };
            };
        };
    } ? {
        r: _rec<U>;
    } : T extends {
        r: {
            r: infer U;
        };
    } ? {
        r: _rec<U>;
    } : T extends {
        r: infer U;
    } ? U : T;
    type Rec<T> = T extends {
        r: unknown;
    } ? Rec<_rec<T>> : T;
    type recp<Sexpr, R extends string[] = [], Str extends string = ""> = Sexpr extends ` (${infer U}` ? {
        r: recp<` ${U}`, [...R, '(']>;
    } : Sexpr extends ` {${infer U}` ? {
        r: recp<` ${U}`, [...R, '{']>;
    } : Sexpr extends ` [${infer U}` ? {
        r: recp<` ${U}`, [...R, '[']>;
    } : Sexpr extends ` "${infer U}` ? {
        r: recp<` ${U}`, [...R, '"']>;
    } : Sexpr extends ` ${infer fU} ${infer Next}` ? fU extends `${infer ffU}}` ? ffU extends '' ? {
        r: recp<` ${Next}`, [...R, '}']>;
    } : {
        r: recp<` ${ffU} } ${Next}`, R>;
    } : fU extends `${infer ffU}]` ? ffU extends '' ? {
        r: recp<` ${Next}`, [...R, ']']>;
    } : {
        r: recp<` ${ffU} ] ${Next}`, R>;
    } : fU extends `${infer ffU})` ? ffU extends '' ? {
        r: recp<` ${Next}`, [...R, ')']>;
    } : {
        r: recp<` ${ffU} ) ${Next}`, R>;
    } : fU extends `${infer ffU}"` ? {
        r: recp<` ${Next}`, [...R, ffU, '"']>;
    } : {
        r: recp<` ${Next}`, [...R, fU]>;
    } : Sexpr extends ` ${infer U})` ? {
        r: recp<` ${U} ) `, R>;
    } : Sexpr extends ` ${infer U}}` ? {
        r: recp<` ${U} } `, R>;
    } : Sexpr extends ` ${infer U}]` ? {
        r: recp<` ${U} ] `, R>;
    } : Sexpr extends ` ${infer U}"` ? {
        r: recp<` ${U} " `, R>;
    } : {
        r: R;
    };
    export type SParser<Sexpr> = Rec<recp<Sexpr>>;
    type SIsNum<S, Top extends boolean = true> = S extends `${infer H}${infer R}` ? H extends '-' ? Top extends true ? SIsNum<R, false> extends true ? true : false : false : H extends '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ? R extends '' ? true : SIsNum<R, false> : false : false;
    export type SSymlator<MSym> = MSym extends `${infer H}${infer R}` ? H extends "'" | '"' ? [`prim`, MSym] : SIsNum<MSym> extends true ? H extends '-' ? ['prim', Bit.BitRevSign<Decimal.DecimalToBit<R>>] : ['prim', Decimal.DecimalToBit<MSym>] : MSym extends 'if' | 'let' | 'fn' ? MSym : MSym extends 'true' ? [`prim`, true] : MSym extends 'false' ? [`prim`, false] : [`sym`, MSym] : never;
    export type SCompiler<Parsed extends Array<unknown>, Current extends Array<unknown> = [], Stack extends Array<Array<unknown>> = [], StrStack extends string = "", IsLetVec extends boolean = false> = Parsed extends [] ? Current : Parsed extends [infer H, ...infer R] ? R extends [] ? H extends '"' ? [`prim`, `${StrStack}"`] : H extends "}" ? ['map', Current] : Current : StrStack extends "" ? H extends ')' | ']' ? SCompiler<R, Stack extends unknown[] ? [...Stack[0], Current extends ['vec', never] ? ['vec'] : Current] : never, Stack extends [infer _, ...infer R extends unknown[][]] ? R : never> : H extends '}' ? SCompiler<R, Stack extends Array<unknown> ? [...Stack[0], ['map', Current]] : never, Stack extends [infer _, ...infer R extends unknown[][]] ? R : never> : H extends '(' ? SCompiler<R, [], [Current, ...Stack]> : H extends '[' ? SCompiler<R, IsLetVec extends true ? [] : ['vec'], [Current, ...Stack], StrStack> : H extends '{' ? SCompiler<R, [], [Current, ...Stack]> : H extends `:${infer _}` ? SCompiler<R, [...Current, [`key`, H]], Stack> : H extends '"' ? SCompiler<R, Current, Stack, `${StrStack}${H}`> : SCompiler<R, [...Current, SSymlator<H>], Stack, StrStack, H extends 'let' | 'fn' ? true : false> : H extends '"' ? SCompiler<R, [...Current, [`prim`, `${StrStack}"`]], Stack, ""> : H extends string ? SCompiler<R, Current, Stack, StrStack extends '"' ? `${StrStack}${H}` : `${StrStack} ${H}`> : never : never;
    type CloseBracket<S extends string, B extends string> = B extends 'map' ? `{${S}}` : B extends 'vec' ? `[${S}]` : B extends 'list' ? `(${S})` : S;
    type GetErrorStr<K extends string, AST> = K extends keyof AST ? AST[K] extends string ? AST[K] : '' : '';
    export type _Unparse<AST, Type extends 'vec' | 'list' | 'map' | 'atom' = 'list'> = AST extends infer H ? H extends ['prim' | 'sym' | 'key', infer r0 extends boolean | string] ? r0 extends string ? Decimal.IsBitExpr<r0> extends true ? Decimal.BitToDecimal<r0> : `${r0}` : `${r0}` : H extends ['vec', ...infer r] ? r extends [] ? '[]' : _Unparse<r, 'vec'> : H extends ['map', infer r] ? _Unparse<r, 'map'> : H extends ['fn', infer r0, infer r1] ? CloseBracket<`fn ${_Unparse<r0, 'vec'>} ${_Unparse<r1>}`, 'list'> : H extends ['let', infer r0, infer r1] ? CloseBracket<`let ${_Unparse<r0, 'vec'>} ${_Unparse<r1>}`, 'list'> : H extends ['if', infer r0, infer r1, ...infer r2] ? CloseBracket<`if ${_Unparse<r0>} ${_Unparse<r1>}${r2 extends [] ? '' : ' '}${_Unparse<r2, 'atom'>}`, Type> : H extends [infer H extends unknown[], ...infer T] ? CloseBracket<`${_Unparse<H>}${T extends [] ? '' : ' '}${_Unparse<T, 'atom'>}`, Type> : H extends [] ? '' : `{error: "${GetErrorStr<'error', AST>}", message: "${GetErrorStr<'message', AST>}"}` : never;
    export type Unparse<AST> = _Unparse<AST> extends infer r ? r extends '' ? 'nil' : r : never;
    export {};
}
export default Compiler;
