export type LetVal = string | Each | Each[];

export type EvenTuple<
  Type extends unknown[]
, Tape extends string = '----/----/'
, R extends unknown[] = []> =
  Tape extends ''
    ? R
  : Tape extends `${infer _}${infer Next}`
    ? R | EvenTuple<Type, Next, [...R, ...Type]>
  : never

export type LetArg = EvenTuple<[Sym | Sym[], Sexpr|LetVal]>
export type LetForm = [`let`, LetArg, Each | Each[] | Sexpr];

export type Each = LetForm | IfForm | Atom
export type Atom = TMap | Sym | Prim | Fn | Vector | Keyword | TNil

/**
```typescript
(fn [] {:a (+ 10 20)})
```

Cion internally evaluates this sexpr to `{:a (+ 10 20)}` via AST at the first.
Then this not-evaluated AST, `(+ 10 20)`, passes into `Eval` as-is.
So, at the time, `TMap` must includes `Sexpr` as members to prevent `EvalError11`.
*/
export type TMap = ['map', (Atom | Sexpr)[]]
// export type TMap = Exclude<Atom, Sym | Prim | Fn | Vector | Keyword | TNil>

export type Sexpr = Array<Each | Each[] | Sexpr>;
export type NilLiteral = 'nil'
export type  TNil = ['prim', NilLiteral]
export const VNil: TNil = ['prim', 'nil'] as const

export type Keyword = [`key`, string]
export type Sym = [`sym`, string];
export type PrimString  = ['prim', string]
export type PrimBoolean = ['prim', boolean]
export type PrimTestNumber  = ['prim', number] // note : this is only used in test.
export type BitString = string
export type RatioString = [string, string]
export type NumString = BitString | RatioString
export type PrimNumber = PrimNat | PrimRatio
export type PrimNat   = ['prim', BitString]
export type PrimRatio = ['prim', RatioString]
export type Prim = PrimString | PrimBoolean | PrimNumber | PrimTestNumber
export type Args = Sym[];
export type Fn = [`fn`, Args, Each | Each[] | Sexpr | Sexpr[]]
export type IFn = Fn | Keyword | TMap
export type VecEmpty = ['vec']
export type Vector = [`vec`, ...(Sexpr | LetForm | Atom)[]] | VecEmpty;
export type Var =
{ name: string
    , value: string | Atom }

export type Env = [] | Var[];

export type TTrue = ['prim', true]
export type TFalse = ['prim', false]

export type  TNotMatch = "NotMatch";
export const VNotMatch = "NotMatch";

export type Falsy = TNil | ['prim', false] | {error: unknown}

export type IfForm = [`if`, Each | Sexpr, Each | Sexpr, Each | Sexpr];
