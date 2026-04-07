import type Cion from '../../src/index.js'
import type { Eval } from '../../src/index.js'
import type {Equal} from '../../src/util.js'

type IfTruePrimTest = [`if`, [`prim`, true], [`prim`, true], [`prim`, false]];
type IfFalsePrimTest = [`if`, [`prim`, false], [`prim`, true], [`prim`, false]];
type IfRecTrueTest = [`if`, IfTruePrimTest, [`prim`, true], [`prim`, false]];
type IfRecFalseTest = [`if`, IfFalsePrimTest, [`prim`, true], [`prim`, false]];

type IfRetFnPattern = [

  `if`,

  IfFalsePrimTest,

  [`fn`, [[`sym`, `ifa`]], [[`sym`, `AppendP`], [`sym`, `ifa`]]],

  [`fn`, [[`sym`, `ifa`]], [[`sym`, `AppendP`], [`sym`, `ifa`]]],

];

const if_test_0 : true = {} as Equal<[`prim`, true]
,Eval<IfTruePrimTest>>

const if_test_1 : true = {} as Equal<[`prim`, false]
,Eval<IfFalsePrimTest>>

const if_test_2 : true = {} as Equal<[`prim`, true]
,Eval<IfRecTrueTest>>

const if_test_3 : true = {} as Equal<[`prim`, false]
,Eval<IfRecFalseTest>>

const if_test_4 : true = {} as Equal<[`prim`, "'this_is_true'"]
,Cion.RawLisp<"(let [a 'a'] (if (eq a 'a') 'this_is_true' 'this_is_false'))">>

const if_test_5 : true = {} as Equal<['prim', '0000000000000001']
,Cion.RawLisp<"(if true 1 2)">>

const if_test_6 : true = {} as Equal<['prim', `'astrbstr'`]
,Cion.RawLisp<"(if true (let [a 'astr' b 'bstr'] (str a b)) 11)">>
