import type { regexCompiler as rc } from '../../src/regex-compiler'
import type { regexEval as re } from '../../src/regex-eval'
import type { regexConst } from '../../src/regex-const'
import type { Equal } from '../../src/util'

// --------------------
// -- Simple Pattern
// --------------------

// .
const _test_adutap0: rc.Comp<'a.x'>['tapes'] = ['a', '.', 'x']
const test_adutap0a: false = {} as Equal<[], re.TapeEval<'axrest', typeof _test_adutap0>>
const test_adutap0a2: true = {} as Equal<['axx', 'rest', []], re.TapeEval<'axxrest', typeof _test_adutap0>>
const test_adutap0b: true = {} as Equal<['a.x', 'rest', []], re.TapeEval<'a.xrest', typeof _test_adutap0>>
const _test_adutap1: rc.Comp<'a..x'>['tapes'] = ['a', '.', '.', 'x']
const test_adutap1a: false = {} as Equal<[], re.TapeEval<'axrest', typeof _test_adutap1>>
const test_adutap1a2: true = {} as Equal<['axxx', 'rest', []], re.TapeEval<'axxxrest', typeof _test_adutap1>>
const test_adutap1b: true = {} as Equal<['aedx', 'rest', []], re.TapeEval<'aedxrest', typeof _test_adutap1>>

// escape \\
const _test_ewtap0: rc.Comp<`a\\[a-z\\]`>['tapes'] = ['a', '\\[', 'a-z', '\\]']
const test_ewtap0a: true = {} as Equal<['a[a-z]', 'rest', []], re.TapeEval<'a[a-z]rest', typeof _test_ewtap0>>
const test_ewtap0a2: false = {} as Equal<[], re.TapeEval<'a[a-zrest', typeof _test_ewtap0>>

// ?
const _test_xntap0: rc.Comp<'ax?'>['tapes'] = ['a', ['?', 'x']]
const test_xntap0a: true = {} as Equal<['ax', 'rest', []], re.TapeEval<'axrest', typeof _test_xntap0>>
const test_xntap0a2: true = {} as Equal<['ax', 'xrest', []], re.TapeEval<'axxrest', typeof _test_xntap0>>
const test_xntap0b: true = {} as Equal<['a', '3rest', []], re.TapeEval<'a3rest', typeof _test_xntap0>>
const test_xntap0: true = {} as Equal<['a', 'rest', []], re.TapeEval<'arest', typeof _test_xntap0>>

// ? ?
const _test_yeetap0: rc.Comp<'ax?x?'>['tapes'] = ['a', ['?', 'x'], ['?', 'x']]
const test_yeetap0a: true = {} as Equal<['ax', 'rest', []], re.TapeEval<'axrest', typeof _test_yeetap0>>
const test_yeetap0a2: true = {} as Equal<['axx', 'rest', []], re.TapeEval<'axxrest', typeof _test_yeetap0>>
const test_yeetap0a3: true = {} as Equal<['axx', 'xxrest', []], re.TapeEval<'axxxxrest', typeof _test_yeetap0>>
const test_yeetap0b: true = {} as Equal<['a', '3rest', []], re.TapeEval<'a3rest', typeof _test_yeetap0>>
const test_yeetap0_2: true = {} as Equal<['a', 'rest', []], re.TapeEval<'arest', typeof _test_yeetap0>>

// *
const _test_gntap0: rc.Comp<'ax*'>['tapes'] = ['a', ['*', 'x']]
const test_gntap0a: true = {} as Equal<['ax', 'rest', []], re.TapeEval<'axrest', typeof _test_gntap0>>
const test_gntap0b: true = {} as Equal<['a', '3rest', []], re.TapeEval<'a3rest', typeof _test_gntap0>>
const test_gntap0c: true = {} as Equal<['a', 'rest', []], re.TapeEval<'arest', typeof _test_gntap0>>
const test_gntap0_2: true = {} as Equal<['axxxx', 'rest', []], re.TapeEval<'axxxxrest', typeof _test_gntap0>>
// +

// -----------------------
// -- chara-class
// ------------------------
const _test_ntap0: rc.Comp<'a[123]'>['tapes'] = ['a', ['chara-class', ['1', '2', '3']]]
const test_ntap0a: true = {} as Equal<['a2', 'rest', []], re.TapeEval<'a2rest', typeof _test_ntap0>>
const test_ntap0b: true = {} as Equal<['a3', 'rest', []], re.TapeEval<'a3rest', typeof _test_ntap0>>
const test_ntap0: false = {} as Equal<[], re.TapeEval<'a4rest', typeof _test_ntap0>>

// chara-class: negation
// [note]
//   Negation Chara Class is compiled with `UtoT`,
//     which cannot save the order of Union Type in the return Tuple.
type _test_nntap0 = rc.Comp<'a[^a-Z0-9]x'>['tapes']
const test_nntap0b: false = {} as Equal<[], re.TapeEval<'a3xrest', _test_nntap0>>
const test_nntap0: true = {} as Equal<['a?x', 'rest', []], re.TapeEval<'a?xrest', _test_nntap0>>

// chara-class: range
const _test_kjtap0: rc.Comp<'a[1a-z]'>['tapes'] = ['a', ['chara-class', ['1', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']]]
const test_kjtap0a: true = {} as Equal<['ab', 'rest', []], re.TapeEval<'abrest', typeof _test_kjtap0>>
const test_kjtap0b: false = {} as Equal<[], re.TapeEval<'a3rest', typeof _test_kjtap0>>
const test_kjtap0c: true = {} as Equal<['a1', 'rest', []], re.TapeEval<'a1rest', typeof _test_kjtap0>>

// -------------------------
// -- chara-class with .
// -------------------------
const _test_awe3tap0: rc.Comp<'a[1a-z.]'>['tapes'] = ['a', ['chara-class', ['1', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '.']]]
const test_awe3tap0a: true = {} as Equal<['ab', 'rest', []], re.TapeEval<'abrest', typeof _test_awe3tap0>>
const test_awe3tap0b: true = {} as Equal<'3rest', re.TapeEval<'a3rest', typeof _test_awe3tap0>['dumpString']>
const test_awe3tap0c: true = {} as Equal<['a1', 'rest', []], re.TapeEval<'a1rest', typeof _test_awe3tap0>>

// chara-class with ?
const _test_ntap01: rc.Comp<'a[123]?'>['tapes'] = ['a', ['?', ['chara-class', ['1', '2', '3']]]]
const test_ntap01a: true = {} as Equal<['a2', 'rest', []], re.TapeEval<'a2rest', typeof _test_ntap01>>
const test_ntap01b: true = {} as Equal<['a', 'rest', []], re.TapeEval<'arest', typeof _test_ntap01>>
const test_ntap01: false = {} as Equal<[], re.TapeEval<'xrest', typeof _test_ntap01>>

// chara-class with *
const _test_mtap01: rc.Comp<'a[123]*'>['tapes'] = ['a', ['*', ['chara-class', ['1', '2', '3']]]]
const test_mtap01a: true = {} as Equal<['a223', 'rest', []], re.TapeEval<'a223rest', typeof _test_mtap01>>
const test_mtap01b: true = {} as Equal<['a', 'rest', []], re.TapeEval<'arest', typeof _test_mtap01>>

// chara-class with +
const _test_nntap01: rc.Comp<'a[123]+'>['tapes'] = ['a', ['chara-class', ['1', '2', '3']], ['*', ['chara-class', ['1', '2', '3']]]]
const test_nntap01a: true = {} as Equal<['a223', 'rest', []], re.TapeEval<'a223rest', typeof _test_nntap01>>
const test_nntap01b: true = {} as Equal<['a2', 'rest', []], re.TapeEval<'a2rest', typeof _test_nntap01>>
const test_nntap01c: false = {} as Equal<[], re.TapeEval<'arest', typeof _test_nntap01>>

// group
const _test_sstap01: rc.Comp<'a(zd)'>['tapes'] = ['a', ['group', ['z', 'd']]]
const test_sstap01a: true = {} as Equal<['azd', 'rest', []], re.TapeEval<'azdrest', typeof _test_sstap01>>
const test_sstap01b: false = {} as Equal<[], re.TapeEval<'azrest', typeof _test_sstap01>>

// group with ?
const _test_sjtap01: rc.Comp<'a(zd)?'>['tapes'] = ['a', ['?', ['group', ['z', 'd']]]]
const test_sjtap01a: true = {} as Equal<['azd', 'rest', []], re.TapeEval<'azdrest', typeof _test_sjtap01>>
const test_sjtap01b: true = {} as Equal<['a', 'rest', []], re.TapeEval<'arest', typeof _test_sjtap01>>

// group with *
const _test_sutap01: rc.Comp<'a(zd)*'>['tapes'] = ['a', ['*', ['group', ['z', 'd']]]]
const test_sutap01a: true = {} as Equal<['azd', 'rest', []], re.TapeEval<'azdrest', typeof _test_sutap01>>
const test_sutap01b: true = {} as Equal<['a', 'rest', []], re.TapeEval<'arest', typeof _test_sutap01>>
const test_sutap01c: true = {} as Equal<['azdzd', 'rest', []], re.TapeEval<'azdzdrest', typeof _test_sutap01>>

// group with +
const _test_setap01: rc.Comp<'a(zd)+'>['tapes'] = ['a', ['group', ['z', 'd']], ['*', ['group', ['z', 'd']]]]
const test_setap01a: true = {} as Equal<['azd', 'rest', []], re.TapeEval<'azdrest', typeof _test_setap01>>
const test_setap01b: false = {} as Equal<[], re.TapeEval<'arest', typeof _test_setap01>>
const test_setap01c: true = {} as Equal<['azdzd', 'rest', []], re.TapeEval<'azdzdrest', typeof _test_setap01>>

// or-group
const _test_yntap0: rc.Comp<'a(x|y)'>['tapes'] = ['a', ['or-group', ['x'], ['y']]]
const test_yntap0a: true = {} as Equal<['ax', 'rest', []], re.TapeEval<'axrest', typeof _test_yntap0>>
const test_yntap0b: false = {} as Equal<[], re.TapeEval<'a3rest', typeof _test_yntap0>>
const test_yntap0c: true = {} as Equal<['ay', 'rest', []], re.TapeEval<'ayrest', typeof _test_yntap0>>
const test_yntap0: true = {} as Equal<['ay', 'yrest', []], re.TapeEval<'ayyrest', typeof _test_yntap0>>

const _test_mnytap0: rc.Comp<'a(x|yy)'>['tapes'] = ['a', ['or-group', ['x'], ['y', 'y']]]
const test_mnytap0a: true = {} as Equal<['ax', 'rest', []], re.TapeEval<'axrest', typeof _test_mnytap0>>
const test_mnytap0b: true = {} as Equal<['ayy', 'rest', []], re.TapeEval<'ayyrest', typeof _test_mnytap0>>
const test_mnytap0c: false = {} as Equal<[], re.TapeEval<'ayrest', typeof _test_mnytap0>>

const _test_xnymntap0: rc.Comp<'a(xx|yy)'>['tapes'] = ['a', ['or-group', ['x', 'x'], ['y', 'y']]]
const test_xnymntap0a: true = {} as Equal<['axx', 'rest', []], re.TapeEval<'axxrest', typeof _test_xnymntap0>>
const test_xnymntap0b: true = {} as Equal<['ayy', 'rest', []], re.TapeEval<'ayyrest', typeof _test_xnymntap0>>
const test_xnymntap0c: false = {} as Equal<[], re.TapeEval<'ayrest', typeof _test_xnymntap0>>

// or-group with ?
const _test_jjdtap0: rc.Comp<'a(x|y)?'>['tapes'] = ['a', ['?', ['or-group', ['x'], ['y']]]]
const test_jjdtap0a: true = {} as Equal<['ax', 'xrest', []], re.TapeEval<'axxrest', typeof _test_jjdtap0>>
const test_jjdtap0aa: true = {} as Equal<['ax', 'rest', []], re.TapeEval<'axrest', typeof _test_jjdtap0>>
const test_jjdtap0b: true = {} as Equal<['a', '3xrest', []], re.TapeEval<'a3xrest', typeof _test_jjdtap0>>
const test_jjdtap0c: true = {} as Equal<['ay', 'xrest', []], re.TapeEval<'ayxrest', typeof _test_jjdtap0>>
const _test_jkdtap0: rc.Comp<'a(x|y)?x'>['tapes'] = ['a', ['?', ['or-group', ['x'], ['y']]], 'x']
const test_jkdtap0a: true = {} as Equal<['axx', 'rest', []], re.TapeEval<'axxrest', typeof _test_jkdtap0>>
const test_jkdtap0aa: true = {} as Equal<['ax', 'rest', []], re.TapeEval<'axrest', typeof _test_jkdtap0>>
const test_jkdtap0b: false = {} as Equal<[], re.TapeEval<'a3xrest', typeof _test_jkdtap0>>
const test_jkdtap0c: true = {} as Equal<['ayx', 'rest', []], re.TapeEval<'ayxrest', typeof _test_jkdtap0>>
const _test_mmdtap0: rc.Comp<'a(x|yy)?x'>['tapes'] = ['a', ['?', ['or-group', ['x'], ['y', 'y']]], 'x']
const test_mmdtap0a: true = {} as Equal<['ax', 'rest', []], re.TapeEval<'axrest', typeof _test_mmdtap0>>
const test_mmdtap0b: false = {} as Equal<[], re.TapeEval<'ayyrest', typeof _test_mmdtap0>>
const test_mmdtap0b1: true = {} as Equal<['ayyx', 'rest', []], re.TapeEval<'ayyxrest', typeof _test_mmdtap0>>
const test_mmdtap0c: false = {} as Equal<[], re.TapeEval<'ayrest', typeof _test_mmdtap0>>
const _test_xnydtap0: rc.Comp<'a(xx|yy)?x'>['tapes'] = ['a', ['?', ['or-group', ['x', 'x'], ['y', 'y']]], 'x']
const test_xnydtap0a: true = {} as Equal<['ax', 'xrest', []], re.TapeEval<'axxrest', typeof _test_xnydtap0>>
const test_xnydtap0b: false = {} as Equal<[], re.TapeEval<'ayyrest', typeof _test_xnydtap0>>
const test_xnydtap0b1: true = {} as Equal<['ayyx', 'rest', []], re.TapeEval<'ayyxrest', typeof _test_xnydtap0>>
const test_xnydtap0c: false = {} as Equal<[], re.TapeEval<'ayrest', typeof _test_xnydtap0>>

// or-group with *
const _test_sketap0: rc.Comp<'a(xx|yy)*x'>['tapes'] = ['a', ['*', ['or-group', ['x', 'x'], ['y', 'y']]], 'x']
const test_sketap0a: false = {} as Equal<[], re.TapeEval<'axxrest', typeof _test_sketap0>> // greedy
const test_sketap0a1: true = {} as Equal<['axxx', 'rest', []], re.TapeEval<'axxxrest', typeof _test_sketap0>>
const test_sketap0a2: false = {} as Equal<[], re.TapeEval<'axxxxrest', typeof _test_sketap0>> // greedy
const test_sketap0a3: true = {} as Equal<['axxxxx', 'rest', []], re.TapeEval<'axxxxxrest', typeof _test_sketap0>>
const test_sketap0a4: false = {} as Equal<[], re.TapeEval<'axxxxxxrest', typeof _test_sketap0>> // greedy
const test_sketap0b: false = {} as Equal<[], re.TapeEval<'ayyrest', typeof _test_sketap0>>
const test_sketap0b1: true = {} as Equal<['ayyx', 'rest', []], re.TapeEval<'ayyxrest', typeof _test_sketap0>>
const test_sketap0c: true = {} as Equal<['ax', 'rest', []], re.TapeEval<'axrest', typeof _test_sketap0>>

// or-group with +
const _test_redtap0: rc.Comp<'a(xx|yy)+x'>['tapes'] = ['a', ['or-group', ['x', 'x'], ['y', 'y']], ['*', ['or-group', ['x', 'x'], ['y', 'y']]], 'x']
const test_redtap0a: false = {} as Equal<[], re.TapeEval<'axxrest', typeof _test_redtap0>> // greedy
const test_redtap0a1: true = {} as Equal<['axxx', 'rest', []], re.TapeEval<'axxxrest', typeof _test_redtap0>>
const test_redtap0a2: false = {} as Equal<[], re.TapeEval<'axxxxrest', typeof _test_redtap0>> // greedy
const test_redtap0a3: true = {} as Equal<['axxxxx', 'rest', []], re.TapeEval<'axxxxxrest', typeof _test_redtap0>>
const test_redtap0a4: false = {} as Equal<[], re.TapeEval<'axxxxxxrest', typeof _test_redtap0>> // greedy
const test_redtap0b: false = {} as Equal<[], re.TapeEval<'ayyrest', typeof _test_redtap0>>
const test_redtap0b1: true = {} as Equal<['ayyx', 'rest', []], re.TapeEval<'ayyxrest', typeof _test_redtap0>>
const test_redtap0c: false = {} as Equal<[], re.TapeEval<'axrest', typeof _test_redtap0>>

// chara-class (with times)
const _test_bluetap0: rc.Comp<'a[1-3d]{2,4}x'>['tapes'] = ['a', [['chara-class', ['1', '2', '3', 'd']], ['chara-class', ['1', '2', '3', 'd']], ['?', ['chara-class', ['1', '2', '3', 'd']]], ['?', ['chara-class', ['1', '2', '3', 'd']]]], 'x']
const test_bluetap0a: true = {} as Equal<['a12x', 'rest', []], re.TapeEval<'a12xrest', typeof _test_bluetap0>>
const test_bluetap0b: true = {} as Equal<['a12d3x', 'rest', []], re.TapeEval<'a12d3xrest', typeof _test_bluetap0>>
const test_bluetap0c: false = {} as Equal<[], re.TapeEval<'a1xrest', typeof _test_bluetap0>>
const test_bluetap0d: false = {} as Equal<[], re.TapeEval<'a123ddrest', typeof _test_bluetap0>>

// group (with times)
const _test_qeuitap0: rc.Comp<'a(xyz){2,4}x'>['tapes'] = ['a', [['group', ['x', 'y', 'z']], ['group', ['x', 'y', 'z']], ['?', ['group', ['x', 'y', 'z']]], ['?', ['group', ['x', 'y', 'z']]]], 'x']
const _test_qeuitap00: ['a', ['group', ['x', 'y', 'z']], ['group', ['x', 'y', 'z']], ['?', ['group', ['x', 'y', 'z']]], ['?', ['group', ['x', 'y', 'z']]], 'x'] = ['a', ['group', ['x', 'y', 'z']], ['group', ['x', 'y', 'z']], ['?', ['group', ['x', 'y', 'z']]], ['?', ['group', ['x', 'y', 'z']]], 'x']
const test_qeuitap0a: false = {} as Equal<[], re.TapeEval<'axyzxrest', typeof _test_qeuitap0>>
const test_qeuitap0b: true = {} as Equal<['axyzxyzx', 'rest', []], re.TapeEval<'axyzxyzxrest', typeof _test_qeuitap0>>
const test_qeuitap0c: true = {} as Equal<['axyzxyzxyzx', 'rest', []], re.TapeEval<'axyzxyzxyzxrest', typeof _test_qeuitap0>>
const test_qeuitap0d: true = {} as Equal<['axyzxyzxyzxyzx', 'rest', []], re.TapeEval<'axyzxyzxyzxyzxrest', typeof _test_qeuitap0>>
const test_qeuitap0e: true = {} as Equal<['axyzxyzxyzxyzx', 'yzxrest', []], re.TapeEval<'axyzxyzxyzxyzxyzxrest', typeof _test_qeuitap0>>
const test_qeuitap0: false = {} as Equal<[], re.TapeEval<'axyzrest', typeof _test_qeuitap0>>

// or-group (with times)
//
// [note]
//   `or-group`
//
const _test_akdtap0: rc.Comp<'a(xx|yy){2,4}x'>['tapes'] = ['a', [['or-group', ['x', 'x'], ['y', 'y']], ['or-group', ['x', 'x'], ['y', 'y']], ['?', ['or-group', ['x', 'x'], ['y', 'y']]], ['?', ['or-group', ['x', 'x'], ['y', 'y']]]], 'x']
const test_akdtap0a: false = {} as Equal<[], re.TapeEval<'axxrest', typeof _test_akdtap0>> // greedy
const test_akdtap0a1: false = {} as Equal<[], re.TapeEval<'axxxrest', typeof _test_akdtap0>>
const test_akdtap0a2: false = {} as Equal<[], re.TapeEval<'axxxxrest', typeof _test_akdtap0>> // greedy
const test_akdtap0a3: true = {} as Equal<['axxxxx', 'rest', []], re.TapeEval<'axxxxxrest', typeof _test_akdtap0>>
const test_akdtap0a4: false = {} as Equal<[], re.TapeEval<'axxxxxxrest', typeof _test_akdtap0>> // greedy
const test_akdtap0b: false = {} as Equal<[], re.TapeEval<'ayyrest', typeof _test_akdtap0>>
const test_akdtap0b1: false = {} as Equal<[], re.TapeEval<'ayyxrest', typeof _test_akdtap0>>
const test_akdtap0b2: true = {} as Equal<['ayyyyx', 'rest', []], re.TapeEval<'ayyyyxrest', typeof _test_akdtap0>>
const test_akdtap0b3: true = {} as Equal<['ayyyyyyx', 'rest', []], re.TapeEval<'ayyyyyyxrest', typeof _test_akdtap0>>
const test_akdtap0b4: true = {} as Equal<['ayyyyyyyyx', 'rest', []], re.TapeEval<'ayyyyyyyyxrest', typeof _test_akdtap0>>
const test_akdtap0b4yyxy: true = {} as Equal<['ayyyyxxyyx', 'rest', []], re.TapeEval<'ayyyyxxyyxrest', typeof _test_akdtap0>>
const test_akdtap0b5: true = {} as Equal<'yyxrest', re.TapeEval<'ayyyyyyyyyyxrest', typeof _test_akdtap0>['dumpString']>
const test_akdtap0b5x: true = {} as Equal<'yyxrest', re.TapeEval<'ayyyyyyxxyyxrest', typeof _test_akdtap0>['dumpString']>
const test_akdtap0c: true = {} as Equal<'xrest', re.TapeEval<'axrest', typeof _test_akdtap0>['dumpString']>

// -------------------------
// --- Nested Pattern
// -------------------------

// group in group (with times)
const _test_nested_group_times: rc.Comp<'a((xy){2,3}){2}z'>['tapes'] = [
  'a',
  [['group', [[['group', ['x', 'y']], ['group', ['x', 'y']],  ['?', ['group', ['x', 'y']]]]]], ['group', [[['group', ['x', 'y']], ['group', ['x', 'y']],  ['?', ['group', ['x', 'y']]]]]]],
  'z'
]
const test_nested_group_times_0: true = {} as Equal<['axyxyxyxyxyxyz', 'rest', []], re.TapeEval<'axyxyxyxyxyxyzrest', typeof _test_nested_group_times>>
const test_nested_group_times_0a: true = {} as Equal<['axyxyxyxyxyz', 'rest', []], re.TapeEval<'axyxyxyxyxyzrest', typeof _test_nested_group_times>>
const test_nested_group_times_0b: true = {} as Equal<'xyxyxyxyzrest', re.TapeEval<'axyxyxyxyzrest', typeof _test_nested_group_times>['dumpString']>
const test_nested_group_times_1: true = {} as Equal<'xyxyzrest', re.TapeEval<'axyxyzrest', typeof _test_nested_group_times>['dumpString']>

// or-group in or-group (with times)
const _test_or_group_nested_times: rc.Comp<'a((x|y){2}){2}z'>['tapes'] = [
  'a',
  [['group', [[['or-group', ['x'], ['y']], ['or-group', ['x'], ['y']]]]],
   ['group', [[['or-group', ['x'], ['y']], ['or-group', ['x'], ['y']]]]]]
  ,'z'
]
const test_or_group_nested_times_0: true = {} as Equal<['axyxyz', 'rest', []], re.TapeEval<'axyxyzrest', typeof _test_or_group_nested_times>>
const test_or_group_nested_times_1: true = {} as Equal<['axxyyz', 'zrest', []], re.TapeEval<'axxyyzzrest', typeof _test_or_group_nested_times>>
const test_or_group_nested_times_2: true = {} as Equal<'xzrest', re.TapeEval<'axzrest', typeof _test_or_group_nested_times>['dumpString']>

// group in or-group (with times)
const _test_group_in_or_group_times: rc.Comp<'a((xy)|z){2}q'>['tapes'] = [
  'a',
  [['or-group', [['group', ['x', 'y']]], ['z']], 
   ['or-group', [['group', ['x', 'y']]], ['z']]]
  ,'q'
]

const test_group_in_or_group_times_0: true = {} as Equal<['axyxyq', 'rest', []], re.TapeEval<'axyxyqrest', typeof _test_group_in_or_group_times>>
const test_group_in_or_group_times_1: true = {} as Equal<['axyzq', 'rest', []], re.TapeEval<'axyzqrest', typeof _test_group_in_or_group_times>>
const test_group_in_or_group_times_2: true = {} as Equal<['azzq', 'rest', []], re.TapeEval<'azzqrest', typeof _test_group_in_or_group_times>>
const test_group_in_or_group_times_3: true = {} as Equal<'zqrest', re.TapeEval<'azqrest', typeof _test_group_in_or_group_times>['dumpString']>

// or-group in group (with times)
const _test_or_group_in_group_times: rc.Comp<'a((x|y)){3}e'>['tapes'] = [
  'a',
  [['group', [['or-group', ['x'], ['y']]]], ['group', [['or-group', ['x'], ['y']]]], ['group', [['or-group', ['x'], ['y']]]]]
  ,'e'
]

const test_or_group_in_group_times_0: true = {} as Equal<['axyxe', 'erest', []], re.TapeEval<'axyxeerest', typeof _test_or_group_in_group_times>>
const test_or_group_in_group_times_1: true = {} as Equal<['axyye', 'erest', []], re.TapeEval<'axyyeerest', typeof _test_or_group_in_group_times>>
const test_or_group_in_group_times_2: true = {} as Equal<'yerest', re.TapeEval<'ayerest', typeof _test_or_group_in_group_times>['dumpString']>

// chara-class in group (with times)
const _test_class_in_group_times: rc.Comp<'a([xy]){3}z'>['tapes'] = [
  'a',
  [['group', [['chara-class', ['x', 'y']]]],['group', [['chara-class', ['x', 'y']]]],['group', [['chara-class', ['x', 'y']]]]],
  'z'
]

const test_class_in_group_times_0: true = {} as Equal<['axyxz', 'yrest', []], re.TapeEval<'axyxzyrest', typeof _test_class_in_group_times>>
const test_class_in_group_times_1: true = {} as Equal<'xyzrest', re.TapeEval<'axyzrest', typeof _test_class_in_group_times>['dumpString']>
const test_class_in_group_times_2: true = {} as Equal<['axxyz', 'rest', []], re.TapeEval<'axxyzrest', typeof _test_class_in_group_times>>


// chara-class in group in or-group
const _test_cc_in_group__in_or_group_times: rc.Comp<'a(([a-d])|z){2}q'>['tapes'] = [
  'a',
  [['or-group', [['group', [['chara-class', ['a', 'b', 'c', 'd']]]]], ['z']], 
['or-group', [['group', [['chara-class', ['a', 'b', 'c', 'd']]]]], ['z']]]
  ,'q'
]
const pretets: re.TapeEval<'drest', [['group', [['chara-class', ['a', 'b', 'c', 'd']]]]]> = ['d', 'rest', []]
const test_cc_in_group__in_or_group_times_0: re.TapeEval<'abdqrest', typeof _test_cc_in_group__in_or_group_times> = ['abdq', 'rest', []]
const test_cc_in_group__in_or_group_times_1: re.TapeEval<'adzqrest', typeof _test_cc_in_group__in_or_group_times> = ['adzq', 'rest', []]
const test_cc_in_group__in_or_group_times_2: re.TapeEval<'azzqrest', typeof _test_cc_in_group__in_or_group_times> = ['azzq', 'rest', []]
const test_cc_in_group__in_or_group_times_3: re.TapeEval<'azqrest', typeof _test_cc_in_group__in_or_group_times>['dumpString'] = 'zqrest'
type __ngts = rc.Comp<`(([^d-Z0-9${regexConst.SigStr}]|65)){3}`>['tapes']
const test_cc_in_group__in_or_group_times_4: re.TapeEval<'abcrest', __ngts> = ['abc', 'rest', []]
type __ngts32 = rc.Comp<`(([^d-Z0-9${regexConst.SigStr}])|65)`>['tapes']
const test_cc_in_group__in_or_group_times_423: re.TapeEval<'abcrest', __ngts32> = ['a', 'bcrest', []]
type __ngts321 = rc.Comp<`(([^d-Z0-9${regexConst.SigStr}]+))`>['tapes']
const test_cc_in_group__in_or_group_times_423a: re.TapeEval<'abcrest', __ngts321> = ['abc', 'rest', []]
type __ngts2 = rc.Comp<`(([^d-Z0-9${regexConst.SigStr}]+)|65)`>['tapes']
const test_cc_in_group__in_or_group_times_42: re.TapeEval<'abcrest', __ngts2> = ['abc', 'rest', []]



// -- email
// -------------------
// 
// [original]
// ^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$
//
// ```
// export type mailRegex = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>
// ```
 
// -- first
// '(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))'
type tapefstHalf = rc.Comp<'(([^<>()[\\].,;: @"]+))'>['tapes']
const tapeaacd: true = {} as Equal<['Zzzzzz', '@gmail.com', []], re.TapeEval<'Zzzzzz@gmail.com', tapefstHalf>>
const tapeaacd2: true = {} as Equal<['Zzz', '.zzz@gmail.com', []], re.TapeEval<'Zzz.zzz@gmail.com', tapefstHalf>>

type tapefstHalf0 = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.+)))'>['tapes']
const tapeaacd0: true = {} as Equal<'Zzzzzz@gmail.com', re.TapeEval<'Zzzzzz@gmail.com', tapefstHalf0>['dumpString']>
const tapeaacd20: true = {} as Equal<['Zzz.', 'zzz@gmail.com', []], re.TapeEval<'Zzz.zzz@gmail.com', tapefstHalf0>>

type xtapefstHalf = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)))'>['tapes']
const xtapefaacd0: true = {} as Equal<'Zzzzzz@gmail.com', re.TapeEval<'Zzzzzz@gmail.com', xtapefstHalf>['dumpString']>
const xtapefaacd20: true = {} as Equal<['Zzz.zzz', '@gmail.com', []], re.TapeEval<'Zzz.zzz@gmail.com', xtapefstHalf>>

type ytapefstHalf = rc.Comp<'([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)'>['tapes']
const ytapefaacd0: true = {} as Equal<['Zzzzzz', '@gmail.com', []], re.TapeEval<'Zzzzzz@gmail.com', ytapefstHalf>>
const ytapefaacd20: true = {} as Equal<['Zzz.zzz', '@gmail.com', []], re.TapeEval<'Zzz.zzz@gmail.com', ytapefstHalf>>

type ztapefstHalf = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))'>['tapes']
const ztapefaacd0: true = {} as Equal<['Zzzzzz', '@gmail.com', []], re.TapeEval<'Zzzzzz@gmail.com', ztapefstHalf>>
const ztapefaacd20: true = {} as Equal<['Zzz.zzz', '@gmail.com', []], re.TapeEval<'Zzz.zzz@gmail.com', ztapefstHalf>>

// -- second
// '((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'
type atapesndHalf = rc.Comp<'([a-zA-Z0-9-]+\\.)'>['tapes']
const atapefaacd0: true = {} as Equal<['gmail.', 'com', []], re.TapeEval<'gmail.com', atapesndHalf>>
const atapefaacd20: true = {} as Equal<['gmail.', 'com', []], re.TapeEval<'gmail.com', atapesndHalf>>

type btapesndHalf = rc.Comp<'(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,})'>['tapes']
const btapefaacd0: true = {} as Equal<['gmail.com', '', []], re.TapeEval<'gmail.com', btapesndHalf>>
const btapefaacd20: true = {} as Equal<['gmail.com', '', []], re.TapeEval<'gmail.com', btapesndHalf>>

type ctapesndHalf = rc.Comp<'(\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])'>['tapes']
const ctapefaacd0: true = {} as Equal<'gmail.com', re.TapeEval<'gmail.com', ctapesndHalf>['dumpString']>
const ctapefaacd20: true = {} as Equal<'gmail.com', re.TapeEval<'gmail.com', ctapesndHalf>['dumpString']>
const cctapefaacd0: true = {} as Equal<['[123.456.789.1]', '', []], re.TapeEval<'[123.456.789.1]', ctapesndHalf>>
const cctapefaacd20: true = {} as Equal<['[123.456.789.101]', '', []], re.TapeEval<'[123.456.789.101]', ctapesndHalf>>
const cctapefaacd30: true = {} as Equal<'[123.456.789]', re.TapeEval<'[123.456.789]', ctapesndHalf>['dumpString']>

type dtapesndHalf = rc.Comp<'((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>['tapes']
const dtapefaacd0: true = {} as Equal<['gmail.com', '', []], re.TapeEval<'gmail.com', dtapesndHalf>>
const dtapefaacd20: true = {} as Equal<['gmail.com', '', []], re.TapeEval<'gmail.com', dtapesndHalf>>
const cdtapefaacd0: true = {} as Equal<['[123.456.789.1]', '', []], re.TapeEval<'[123.456.789.1]', dtapesndHalf>>
const cdtapefaacd20: true = {} as Equal<['[123.456.789.101]', '', []], re.TapeEval<'[123.456.789.101]', dtapesndHalf>>
const cdtapefaacd30: true = {} as Equal<'[123.456.789]', re.TapeEval<'[123.456.789]', dtapesndHalf>['dumpString']>

// -- all
export type mailRegex = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>['tapes']

const test_mailreg_x: true = {} as Equal<['Zzz.zzz@gmail.com', '', []], re.TapeEval<'Zzz.zzz@gmail.com', mailRegex>>
const test_mailreg_xx: true = {} as Equal<['Zzzzzz@gmail.com', '', []], re.TapeEval<'Zzzzzz@gmail.com', mailRegex>>
const test_mailreg_xxx: true = {} as Equal<['Zz1235z.zzz@gmail.com', '', []], re.TapeEval<'Zz1235z.zzz@gmail.com', mailRegex>>
const test_mailreg_xxxx: true = {} as Equal<['Zzz.zzz@[123.456.789.101]', '', []], re.TapeEval<'Zzz.zzz@[123.456.789.101]', mailRegex>>
const test_mailreg_xxxxx: true = {} as Equal<['Zzz.zzz{user}@[123.456.789.101]', '', []], re.TapeEval<'Zzz.zzz{user}@[123.456.789.101]', mailRegex>>
