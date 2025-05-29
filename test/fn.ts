import type { Eval } from '../src/index'
import type {LetVal,LetArg,LetForm,Each,Atom,TMap,Sexpr,TNil,Keyword,Sym,PrimString,PrimBoolean,PrimTestNumber,PrimNumber,Prim,Args,Fn,Vector,Var,Env,TNotMatch,IfForm} from '../src/sexprtypes'

const testttt0: Eval<[['fn', [['sym', 'a'], ['sym', 'b']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]]]> = {sexpr: ['NotMatch'], error: 'ReadingError0', message: 'sexpr is not atom list.'}
const testttt2: Eval<[['fn', [['sym', 'a'], ['sym', 'b']], [['sym', '+'], ['sym', 'a'], ['sym', 'b']]], ['prim', '10'], ['prim', '11']]> = ['prim', '0000000000000101']

const testfnlispeq0: Eval<
  [
    [`fn`, [[`sym`, `a`]], [[`sym`, `eq`], [`sym`, `a`], [`prim`, `'1'`]]],
    [`prim`, `'0'`],
  ]
> = [`prim`, false];

const testfnlispeq1: Eval<
  [
    [`fn`, [[`sym`, `a`]], [[`sym`, `eq`], [`sym`, `a`], [`prim`, `'1'`]]],
    [`prim`, `'1'`],
  ]
> = [`prim`, true];

const testakj: Sexpr = [['fn', [['sym', 'aaa']], [['sym', 'not'], [['sym', '>'], ['prim', '0000000000000010'], ['sym', 'aaa']]]], ['prim', '0000000000000010']]

const jktejkst: Sexpr | LetForm = ['let', [['sym', 'aaa'], ['prim', '0000000000000010']], [['sym', 'not'], [['sym', '>'], ['prim', '0000000000000010'], ['sym', 'aaa']]]]

const jltesta: Eval<[['fn', [['sym', 'aaa']], [['sym', 'not'], [['sym', '>'], ['prim', '0000000000000010'], ['sym', 'aaa']]]], ['prim', '0000000000000010']]> = ['prim', true]

// multiarg fn test
const testmultiargfn0: Eval<
  [
    [
      `fn`,
      [[`sym`, `a`], [`sym`, `b`]],
      [[`sym`, `str`], [`sym`, `a`], [`sym`, `b`]],
    ],
    [`prim`, `'0'`],
    [`prim`, `'1'`],
  ]
> = [`prim`, `'01'`];
