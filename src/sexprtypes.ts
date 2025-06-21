// import type {LetVal,LetArg,LetForm,Each,Atom,TMap,Sexpr,TNil,Keyword,Sym,PrimString,PrimBoolean,PrimTestNumber,PrimNumber,Prim,Args,Fn,IFn,Vector,Var,Env,TNotMatch,IfForm} from './sexprtypes.ts'
// import {VNil,VNotMatch} from './sexprtypes.ts'

export type LetVal = string | Each | Each[];
export type LetArg = [Sym, LetVal];
// todo : too ugly.
export type LetForm = [`let`, (Sym | LetVal)[] | [Sym[], LetVal[]], Each | Each[] | Sexpr];
// test let
export const larttest: LetArg = [[`sym`, `t`], `test`];

export type Each = LetForm | IfForm | Atom
export type Atom = ['map', Atom[]] | Sym | Prim | Fn | Vector | Keyword | TNil
export type TMap = Exclude<Atom, Sym | Prim | Fn | Vector | Keyword | TNil>

export type Sexpr = Array<Each | Each[] | Sexpr>;
export type  TNil = ['prim', 'nil']
export const VNil: TNil = ['prim', 'nil']

export type Keyword = [`key`, string]
export type Sym = [`sym`, string];
export type PrimString  = ['prim', string]
export type PrimBoolean = ['prim', boolean]
export type PrimTestNumber  = ['prim', number] // note : this is only used in test.
export type BitString = string
export type RatioString = [string, string]
export type NumString = BitString | RatioString
export type PrimNumber = PrimNut | PrimRatio
export type PrimNut   = ['prim', BitString]
export type PrimRatio = ['prim', RatioString]
export type Prim = PrimString | PrimBoolean | PrimNumber | PrimTestNumber
export type Args = Sym[];
export type Fn = [`fn`, Args, Each | Each[] | Sexpr | Sexpr[]]
export type IFn = Fn | Keyword | TMap
export type Vector = [`vec`, ...(Sexpr | LetForm | Atom)[]] | [`vec`];
export type Var =
{ name: string
    , value: string | Atom }

export type Env = [] | Var[];

export type  TNotMatch = "NotMatch";
export const VNotMatch = "NotMatch";

export type IfForm = [`if`, Each | Sexpr, Each | Sexpr, Each | Sexpr];
