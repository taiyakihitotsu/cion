import type { regexCompiler as rc } from '../src/regex-compiler'
import type { regexEval as re } from '../src/regex-eval'
import type { regexConst } from '../src/regex-const'
import type * as testrc from '../test/regex-compiler'

// --------------------
// -- Simple Pattern
// --------------------

// .
const _test_adutap0: rc.Comp<'a.x'>['tapes'] = ['a', '.', 'x']
// @ts-expect-errors:
const test_adutap0a: re.TapeEval<'axrest', typeof _test_adutap0> = []
const test_adutap0a2: re.TapeEval<'axxrest', typeof _test_adutap0> = ['axx', 'rest', []]
const test_adutap0b: re.TapeEval<'a.xrest', typeof _test_adutap0> = ['a.x', 'rest', []]
const _test_adutap1: rc.Comp<'a..x'>['tapes'] = ['a', '.', '.', 'x']
// @ts-expect-errors:
const test_adutap1a: re.TapeEval<'axrest', typeof _test_adutap1> = []
const test_adutap1a2: re.TapeEval<'axxxrest', typeof _test_adutap1> = ['axxx', 'rest', []]
const test_adutap1b: re.TapeEval<'aedxrest', typeof _test_adutap1> = ['aedx', 'rest', []]
// escape \\
const _test_ewtap0: rc.Comp<`a\\[a-z\\]`>['tapes'] = ['a', '\\[', 'a-z', '\\]']
const test_ewtap0a: re.TapeEval<'a[a-z]rest', typeof _test_ewtap0> = ['a[a-z]', 'rest', []]
// @ts-expect-errors:
const test_ewtap0a2: re.TapeEval<'a[a-zrest', typeof _test_ewtap0> = []
// ?
const _test_xntap0: rc.Comp<'ax?'>['tapes'] = ['a', ['?', 'x']]
const test_xntap0a: re.TapeEval<'axrest', typeof _test_xntap0> = ['ax', 'rest', []]
const test_xntap0a2: re.TapeEval<'axxrest', typeof _test_xntap0> = ['ax', 'xrest', []]
const test_xntap0b: re.TapeEval<'a3rest', typeof _test_xntap0> = ['a', '3rest', []]
const test_xntap0: re.TapeEval<'arest', typeof _test_xntap0> = ['a', 'rest', []]
// ? ?
const _test_yeetap0: rc.Comp<'ax?x?'>['tapes'] = ['a', ['?', 'x'], ['?', 'x']]
const test_yeetap0a: re.TapeEval<'axrest', typeof _test_yeetap0> = ['ax', 'rest', []]
const test_yeetap0a2: re.TapeEval<'axxrest', typeof _test_yeetap0> = ['axx', 'rest', []]
const test_yeetap0a3: re.TapeEval<'axxxxrest', typeof _test_yeetap0> = ['axx', 'xxrest', []]
const test_yeetap0b: re.TapeEval<'a3rest', typeof _test_yeetap0> = ['a', '3rest', []]
const test_yeetap0: re.TapeEval<'arest', typeof _test_yeetap0> = ['a', 'rest', []]
// *
const _test_gntap0: rc.Comp<'ax*'>['tapes'] = ['a', ['*', 'x']]
const test_gntap0a: re.TapeEval<'axrest', typeof _test_gntap0> = ['ax', 'rest', []]
const test_gntap0b: re.TapeEval<'a3rest', typeof _test_gntap0> = ['a', '3rest', []]
const test_gntap0c: re.TapeEval<'arest', typeof _test_gntap0> = ['a', 'rest', []]
const test_gntap0: re.TapeEval<'axxxxrest', typeof _test_gntap0> = ['axxxx', 'rest', []]
// +

// -----------------------
// -- chara-class
// ------------------------
const _test_ntap0: rc.Comp<'a[123]'>['tapes'] = ['a', ['chara-class', ['1', '2', '3']]]
const test_ntap0a: re.TapeEval<'a2rest', typeof _test_ntap0> = ['a2', 'rest', []]
const test_ntap0b: re.TapeEval<'a3rest', typeof _test_ntap0> = ['a3', 'rest', []]
// @ts-expect-errors:
const test_ntap0: re.TapeEval<'a4rest', typeof _test_ntap0> = []
// chara-class: negation
// [note]
//   Negation Chara Class is compiled with `UtoT`,
//     which cannot save the order of Union Type in the return Tuple.
type _test_nntap0 = rc.Comp<'a[^a-Z0-9]x'>['tapes']
// @ts-expect-errors:
const test_nntap0b: re.TapeEval<'a3xrest', _test_nntap0> = []
const test_nntap0: re.TapeEval<'a?xrest', _test_nntap0> = ['a?x', 'rest', []]
// chara-class: range
const _test_kjtap0: rc.Comp<'a[1a-z]'>['tapes'] = ['a', ['chara-class', ['1', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']]]
const test_kjtap0a: re.TapeEval<'abrest', typeof _test_kjtap0> = ['ab', 'rest', []]
// @ts-expect-errors:
const test_kjtap0b: re.TapeEval<'a3rest', typeof _test_kjtap0> = []
const test_kjtap0c: re.TapeEval<'a1rest', typeof _test_kjtap0> = ['a1', 'rest', []]

// -------------------------
// -- chara-class with .
// -------------------------
const _test_awe3tap0: rc.Comp<'a[1a-z.]'>['tapes'] = ['a', ['chara-class', ['1', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '.']]]
const test_awe3tap0a: re.TapeEval<'abrest', typeof _test_awe3tap0> = ['ab', 'rest', []]
const test_awe3tap0b: re.TapeEval<'a3rest', typeof _test_awe3tap0>['dumpString'] = '3rest'
const test_awe3tap0c: re.TapeEval<'a1rest', typeof _test_awe3tap0> = ['a1', 'rest', []]


// chara-class with ?
const _test_ntap01: rc.Comp<'a[123]?'>['tapes'] = ['a', ['?', ['chara-class', ['1', '2', '3']]]]
const test_ntap01a: re.TapeEval<'a2rest', typeof _test_ntap01> = ['a2', 'rest', []]
const test_ntap01b: re.TapeEval<'arest', typeof _test_ntap01> = ['a', 'rest', []]
// @ts-expect-errors:
const test_ntap01: re.TapeEval<'xrest', typeof _test_ntap01> = []
// chara-class with *
const _test_mtap01: rc.Comp<'a[123]*'>['tapes'] = ['a', ['*', ['chara-class', ['1', '2', '3']]]]
const test_mtap01a: re.TapeEval<'a223rest', typeof _test_mtap01> = ['a223', 'rest', []]
const test_mtap01b: re.TapeEval<'arest', typeof _test_mtap01> = ['a', 'rest', []]
// chara-class with +
const _test_nntap01: rc.Comp<'a[123]+'>['tapes'] = ['a', ['chara-class', ['1', '2', '3']], ['*', ['chara-class', ['1', '2', '3']]]]
const test_nntap01a: re.TapeEval<'a223rest', typeof _test_nntap01> = ['a223', 'rest', []]
const test_nntap01b: re.TapeEval<'a2rest', typeof _test_nntap01> = ['a2', 'rest', []]
// @ts-expect-errors:
const test_nntap01c: re.TapeEval<'arest', typeof _test_nntap01> = []
// group
const _test_sstap01: rc.Comp<'a(zd)'>['tapes'] = ['a', ['group', ['z', 'd']]]
const test_sstap01a: re.TapeEval<'azdrest', typeof _test_sstap01> = ['azd', 'rest', []]
// @ts-expect-errors:
const test_sstap01b: re.TapeEval<'azrest', typeof _test_sstap01> = []
// group with ?
const _test_sjtap01: rc.Comp<'a(zd)?'>['tapes'] = ['a', ['?', ['group', ['z', 'd']]]]
const test_sjtap01a: re.TapeEval<'azdrest', typeof _test_sjtap01> = ['azd', 'rest', []]
const test_sjtap01b: re.TapeEval<'arest', typeof _test_sjtap01> = ['a', 'rest', []]
// group with *
const _test_sutap01: rc.Comp<'a(zd)*'>['tapes'] = ['a', ['*', ['group', ['z', 'd']]]]
const test_sutap01a: re.TapeEval<'azdrest', typeof _test_sutap01> = ['azd', 'rest', []]
const test_sutap01b: re.TapeEval<'arest', typeof _test_sutap01> = ['a', 'rest', []]
const test_sutap01c: re.TapeEval<'azdzdrest', typeof _test_sutap01> = ['azdzd', 'rest', []]
// group with +
const _test_setap01: rc.Comp<'a(zd)+'>['tapes'] = ['a', ['group', ['z', 'd']], ['*', ['group', ['z', 'd']]]]
const test_setap01a: re.TapeEval<'azdrest', typeof _test_setap01> = ['azd', 'rest', []]
// @ts-expect-errors:
const test_setap01b: re.TapeEval<'arest', typeof _test_setap01> = []
const test_setap01c: re.TapeEval<'azdzdrest', typeof _test_setap01> = ['azdzd', 'rest', []]
// or-group
const _test_yntap0: rc.Comp<'a(x|y)'>['tapes'] = ['a', ['or-group', ['x'], ['y']]]
const test_yntap0a: re.TapeEval<'axrest', typeof _test_yntap0> = ['ax', 'rest', []]
// @ts-expect-errors:
const test_yntap0b: re.TapeEval<'a3rest', typeof _test_yntap0> = []
const test_yntap0c: re.TapeEval<'ayrest', typeof _test_yntap0> = ['ay', 'rest', []]
const test_yntap0:  re.TapeEval<'ayyrest', typeof _test_yntap0> = ['ay', 'yrest', []]
const _test_mnytap0: rc.Comp<'a(x|yy)'>['tapes'] = ['a', ['or-group', ['x'], ['y', 'y']]]
const test_mnytap0a: re.TapeEval<'axrest', typeof _test_mnytap0> = ['ax', 'rest', []]
const test_mnytap0b: re.TapeEval<'ayyrest', typeof _test_mnytap0> = ['ayy', 'rest', []]
// @ts-expect-errors:
const test_mnytap0c: re.TapeEval<'ayrest', typeof _test_mnytap0> = []
const _test_xnymntap0: rc.Comp<'a(xx|yy)'>['tapes'] = ['a', ['or-group', ['x', 'x'], ['y', 'y']]]
const test_xnymntap0a: re.TapeEval<'axxrest', typeof _test_xnymntap0> = ['axx', 'rest', []]
const test_xnymntap0b: re.TapeEval<'ayyrest', typeof _test_xnymntap0> = ['ayy', 'rest', []]
// @ts-expect-errors:
const test_xnymntap0c: re.TapeEval<'ayrest', typeof _test_xnymntap0> = []
// or-group with ?
const _test_jjdtap0: rc.Comp<'a(x|y)?'>['tapes'] = ['a', ['?', ['or-group', ['x'], ['y']]]]
const test_jjdtap0a: re.TapeEval<'axxrest', typeof _test_jjdtap0> = ['ax', 'xrest', []]
const test_jjdtap0aa: re.TapeEval<'axrest', typeof _test_jjdtap0> = ['ax', 'rest', []]
const test_jjdtap0b: re.TapeEval<'a3xrest', typeof _test_jjdtap0> = ['a', '3xrest', []]
const test_jjdtap0c: re.TapeEval<'ayxrest', typeof _test_jjdtap0> = ['ay', 'xrest', []]
const _test_jkdtap0: rc.Comp<'a(x|y)?x'>['tapes'] = ['a', ['?', ['or-group', ['x'], ['y']]], 'x']
const test_jkdtap0a: re.TapeEval<'axxrest', typeof _test_jkdtap0> = ['axx', 'rest', []]
const test_jkdtap0aa: re.TapeEval<'axrest', typeof _test_jkdtap0> = ['ax', 'rest', []]
// @ts-expect-errors:
const test_jkdtap0b: re.TapeEval<'a3xrest', typeof _test_jkdtap0> = []
const test_jkdtap0c: re.TapeEval<'ayxrest', typeof _test_jkdtap0> = ['ayx', 'rest', []]
const _test_mmdtap0: rc.Comp<'a(x|yy)?x'>['tapes'] = ['a', ['?', ['or-group', ['x'], ['y', 'y']]], 'x']
const test_mmdtap0a: re.TapeEval<'axrest', typeof _test_mmdtap0> = ['ax', 'rest', []]
// @ts-expect-errors:
const test_mmdtap0b: re.TapeEval<'ayyrest', typeof _test_mmdtap0> = []
const test_mmdtap0b1: re.TapeEval<'ayyxrest', typeof _test_mmdtap0> = ['ayyx', 'rest', []]
// @ts-expect-errors:
const test_mmdtap0c: re.TapeEval<'ayrest', typeof _test_mmdtap0> = []
const _test_xnydtap0: rc.Comp<'a(xx|yy)?x'>['tapes'] = ['a', ['?', ['or-group', ['x', 'x'], ['y', 'y']]], 'x']
const test_xnydtap0a: re.TapeEval<'axxrest', typeof _test_xnydtap0> = ['ax', 'xrest', []]
// @ts-expect-errors:
const test_xnydtap0b: re.TapeEval<'ayyrest', typeof _test_xnydtap0> = []
const test_xnydtap0b1: re.TapeEval<'ayyxrest', typeof _test_xnydtap0> = ['ayyx', 'rest', []]
// @ts-expect-errors:
const test_xnydtap0c: re.TapeEval<'ayrest', typeof _test_xnydtap0> = []
// or-group with *
const _test_sketap0: rc.Comp<'a(xx|yy)*x'>['tapes'] = ['a', ['*', ['or-group', ['x', 'x'], ['y', 'y']]], 'x']
// @ts-expect-errors:
const test_sketap0a: re.TapeEval<'axxrest', typeof _test_sketap0> = [] // greedy
const test_sketap0a1: re.TapeEval<'axxxrest', typeof _test_sketap0> = ['axxx', 'rest', []]
// @ts-expect-errors:
const test_sketap0a2: re.TapeEval<'axxxxrest', typeof _test_sketap0> = [] // greedy
const test_sketap0a3: re.TapeEval<'axxxxxrest', typeof _test_sketap0> = ['axxxxx', 'rest', []]
// @ts-expect-errors:
const test_sketap0a4: re.TapeEval<'axxxxxxrest', typeof _test_sketap0> = [] // greedy
// @ts-expect-errors:
const test_sketap0b: re.TapeEval<'ayyrest', typeof _test_sketap0> = []
const test_sketap0b1: re.TapeEval<'ayyxrest', typeof _test_sketap0> = ['ayyx', 'rest', []]
const test_sketap0c: re.TapeEval<'axrest', typeof _test_sketap0> = ['ax', 'rest', []]
// or-group with +
const _test_redtap0: rc.Comp<'a(xx|yy)+x'>['tapes'] = ['a', ['or-group', ['x', 'x'], ['y', 'y']], ['*', ['or-group', ['x', 'x'], ['y', 'y']]], 'x']
// @ts-expect-errors:
const test_redtap0a: re.TapeEval<'axxrest', typeof _test_redtap0> = [] // greedy
const test_redtap0a1: re.TapeEval<'axxxrest', typeof _test_redtap0> = ['axxx', 'rest', []]
// @ts-expect-errors:
const test_redtap0a2: re.TapeEval<'axxxxrest', typeof _test_redtap0> = [] // greedy
const test_redtap0a3: re.TapeEval<'axxxxxrest', typeof _test_redtap0> = ['axxxxx', 'rest', []]
// @ts-expect-errors:
const test_redtap0a4: re.TapeEval<'axxxxxxrest', typeof _test_redtap0> = [] // greedy
// @ts-expect-errors:
const test_redtap0b: re.TapeEval<'ayyrest', typeof _test_redtap0> = []
const test_redtap0b1: re.TapeEval<'ayyxrest', typeof _test_redtap0> = ['ayyx', 'rest', []]
// @ts-expect-errors:
const test_redtap0c: re.TapeEval<'axrest', typeof _test_redtap0> = []
// chara-class (with times)
const _test_bluetap0: rc.Comp<'a[1-3d]{2,4}x'>['tapes'] = ['a', [['chara-class', ['1', '2', '3', 'd']], ['chara-class', ['1', '2', '3', 'd']], ['?', ['chara-class', ['1', '2', '3', 'd']]], ['?', ['chara-class', ['1', '2', '3', 'd']]]], 'x']
const test_bluetap0a: re.TapeEval<'a12xrest', typeof _test_bluetap0> = ['a12x', 'rest', []] 
const test_bluetap0b: re.TapeEval<'a12d3xrest', typeof _test_bluetap0> = ['a12d3x', 'rest', []] 
// @ts-expect-errors:
const test_bluetap0c: re.TapeEval<'a1xrest', typeof _test_bluetap0> = []
// @ts-expect-errors:
const test_bluetap0d: re.TapeEval<'a123ddrest', typeof _test_bluetap0> = [] 
// group (with times)
const _test_qeuitap0: rc.Comp<'a(xyz){2,4}x'>['tapes'] = ['a', [['group', ['x', 'y', 'z']], ['group', ['x', 'y', 'z']], ['?', ['group', ['x', 'y', 'z']]], ['?', ['group', ['x', 'y', 'z']]]], 'x']
// const _test_qeuitap0: ['a', ['group', ['x', 'y', 'z']], ['group', ['x', 'y', 'z']], ['?', ['group', ['x', 'y', 'z']]], ['?', ['group', ['x', 'y', 'z']]], 'x'] = ['a', ['group', ['x', 'y', 'z']], ['group', ['x', 'y', 'z']], ['?', ['group', ['x', 'y', 'z']]], ['?', ['group', ['x', 'y', 'z']]], 'x']
// @ts-expect-errors:
const test_qeuitap0a: re.TapeEval<'axyzxrest', typeof _test_qeuitap0> = []
const test_qeuitap0b: re.TapeEval<'axyzxyzxrest', typeof _test_qeuitap0> = ['axyzxyzx', 'rest', []]
const test_qeuitap0c: re.TapeEval<'axyzxyzxyzxrest', typeof _test_qeuitap0> = ['axyzxyzxyzx', 'rest', []]
const test_qeuitap0d: re.TapeEval<'axyzxyzxyzxyzxrest', typeof _test_qeuitap0> = ['axyzxyzxyzxyzx', 'rest', []]
const test_qeuitap0e: re.TapeEval<'axyzxyzxyzxyzxyzxrest', typeof _test_qeuitap0> = ['axyzxyzxyzxyzx', 'yzxrest', []]
// @ts-expect-errors:
const test_qeuitap0: re.TapeEval<'axyzrest', typeof _test_qeuitap0> = []
// or-group (with times)
//
// [note]
//   `or-group`
//
const _test_akdtap0: rc.Comp<'a(xx|yy){2,4}x'>['tapes'] = ['a', [['or-group', ['x', 'x'], ['y', 'y']], ['or-group', ['x', 'x'], ['y', 'y']], ['?', ['or-group', ['x', 'x'], ['y', 'y']]], ['?', ['or-group', ['x', 'x'], ['y', 'y']]]], 'x']
// @ts-expect-errors:
const test_akdtap0a: re.TapeEval<'axxrest', typeof _test_akdtap0> = [] // greedy
// @ts-expect-errors:
const test_akdtap0a1: re.TapeEval<'axxxrest', typeof _test_akdtap0> = []
// @ts-expect-errors:
const test_akdtap0a2: re.TapeEval<'axxxxrest', typeof _test_akdtap0> = [] // greedy
const test_akdtap0a3: re.TapeEval<'axxxxxrest', typeof _test_akdtap0> = ['axxxxx', 'rest', []]
// @ts-expect-errors:
const test_akdtap0a4: re.TapeEval<'axxxxxxrest', typeof _test_akdtap0> = [] // greedy
// @ts-expect-errors:
const test_akdtap0b: re.TapeEval<'ayyrest', typeof _test_akdtap0> = []
// @ts-expect-errors:
const test_akdtap0b1: re.TapeEval<'ayyxrest', typeof _test_akdtap0> = []
const test_akdtap0b2: re.TapeEval<'ayyyyxrest', typeof _test_akdtap0> = ['ayyyyx', 'rest', []]
const test_akdtap0b3: re.TapeEval<'ayyyyyyxrest', typeof _test_akdtap0> = ['ayyyyyyx', 'rest', []]
const test_akdtap0b4: re.TapeEval<'ayyyyyyyyxrest', typeof _test_akdtap0> = ['ayyyyyyyyx', 'rest', []]
const test_akdtap0b4yyxy: re.TapeEval<'ayyyyxxyyxrest', typeof _test_akdtap0> = ['ayyyyxxyyx', 'rest', []]
const test_akdtap0b5: re.TapeEval<'ayyyyyyyyyyxrest', typeof _test_akdtap0>['dumpString'] = 'yyxrest'
const test_akdtap0b5x: re.TapeEval<'ayyyyyyxxyyxrest', typeof _test_akdtap0>['dumpString'] = 'yyxrest'
const test_akdtap0c: re.TapeEval<'axrest', typeof _test_akdtap0>['dumpString'] = 'xrest'



// -------------------------
// --- Nested Pattern
// -------------------------

// group in group (with times)
const _test_nested_group_times: rc.Comp<'a((xy){2,3}){2}z'>['tapes'] = [
  'a',
  [['group', [[['group', ['x', 'y']], ['group', ['x', 'y']],  ['?', ['group', ['x', 'y']]]]]], ['group', [[['group', ['x', 'y']], ['group', ['x', 'y']],  ['?', ['group', ['x', 'y']]]]]]],
  'z'
]
const test_nested_group_times_0: re.TapeEval<'axyxyxyxyxyxyzrest', typeof _test_nested_group_times> = ['axyxyxyxyxyxyz', 'rest', []]
const test_nested_group_times_0a: re.TapeEval<'axyxyxyxyxyzrest', typeof _test_nested_group_times> = ['axyxyxyxyxyz', 'rest', []]
const test_nested_group_times_0b: re.TapeEval<'axyxyxyxyzrest', typeof _test_nested_group_times>['dumpString'] = 'xyxyxyxyzrest'
const test_nested_group_times_1: re.TapeEval<'axyxyzrest', typeof _test_nested_group_times>['dumpString'] = 'xyxyzrest'

// or-group in or-group (with times)
const _test_or_group_nested_times: rc.Comp<'a((x|y){2}){2}z'>['tapes'] = [
  'a',
  [['group', [[['or-group', ['x'], ['y']], ['or-group', ['x'], ['y']]]]],
   ['group', [[['or-group', ['x'], ['y']], ['or-group', ['x'], ['y']]]]]]
  ,'z'
]
const test_or_group_nested_times_0: re.TapeEval<'axyxyzrest', typeof _test_or_group_nested_times> = ['axyxyz', 'rest', []]
const test_or_group_nested_times_1: re.TapeEval<'axxyyzzrest', typeof _test_or_group_nested_times> = ['axxyyz', 'zrest', []]
const test_or_group_nested_times_2: re.TapeEval<'axzrest', typeof _test_or_group_nested_times>['dumpString'] = 'xzrest'
// group in or-group (with times)
const _test_group_in_or_group_times: rc.Comp<'a((xy)|z){2}q'>['tapes'] = [
  'a',
  [['or-group', [['group', ['x', 'y']]], ['z']], 
   ['or-group', [['group', ['x', 'y']]], ['z']]]
  ,'q'
]

const test_group_in_or_group_times_0: re.TapeEval<'axyxyqrest', typeof _test_group_in_or_group_times> = ['axyxyq', 'rest', []]
const test_group_in_or_group_times_1: re.TapeEval<'axyzqrest', typeof _test_group_in_or_group_times> = ['axyzq', 'rest', []]
const test_group_in_or_group_times_2: re.TapeEval<'azzqrest', typeof _test_group_in_or_group_times> = ['azzq', 'rest', []]
const test_group_in_or_group_times_3: re.TapeEval<'azqrest', typeof _test_group_in_or_group_times>['dumpString'] = 'zqrest'

// or-group in group (with times)
const _test_or_group_in_group_times: rc.Comp<'a((x|y)){3}e'>['tapes'] = [
  'a',
  [['group', [['or-group', ['x'], ['y']]]], ['group', [['or-group', ['x'], ['y']]]], ['group', [['or-group', ['x'], ['y']]]]]
  ,'e'
]

const test_or_group_in_group_times_0: re.TapeEval<'axyxeerest', typeof _test_or_group_in_group_times> = ['axyxe', 'erest', []]
const test_or_group_in_group_times_1: re.TapeEval<'axyyeerest', typeof _test_or_group_in_group_times> = ['axyye', 'erest', []]
const test_or_group_in_group_times_2: re.TapeEval<'ayerest', typeof _test_or_group_in_group_times>['dumpString'] = 'yerest'

// chara-class in group (with times)

const _test_class_in_group_times: rc.Comp<'a([xy]){3}z'>['tapes'] = [
  'a',
  [['group', [['chara-class', ['x', 'y']]]],['group', [['chara-class', ['x', 'y']]]],['group', [['chara-class', ['x', 'y']]]]],
  'z'
]

const test_class_in_group_times_0: re.TapeEval<'axyxzyrest', typeof _test_class_in_group_times> = ['axyxz', 'yrest', []]
const test_class_in_group_times_1: re.TapeEval<'axyzrest', typeof _test_class_in_group_times>['dumpString'] = 'xyzrest'
const test_class_in_group_times_2: re.TapeEval<'axxyzrest', typeof _test_class_in_group_times> = ['axxyz', 'rest', []]

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
type  tapefstHalf = rc.Comp<'(([^<>()[\\].,;: @"]+))'>['tapes']
const tapeaacd: re.TapeEval<'Zzzzzz@gmail.com', tapefstHalf> = ['Zzzzzz', '@gmail.com', []]
const tapeaacd2: re.TapeEval<'Zzz.zzz@gmail.com', tapefstHalf> = ['Zzz', '.zzz@gmail.com', []]

type  tapefstHalf0 = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.+)))'>['tapes']
const tapeaacd0: re.TapeEval<'Zzzzzz@gmail.com', tapefstHalf0>['dumpString'] = 'Zzzzzz@gmail.com'
const tapeaacd20: re.TapeEval<'Zzz.zzz@gmail.com', tapefstHalf0> = ['Zzz.', 'zzz@gmail.com', []]

type  xtapefstHalf = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)))'>['tapes']
const xtapefaacd0: re.TapeEval<'Zzzzzz@gmail.com', xtapefstHalf>['dumpString'] = 'Zzzzzz@gmail.com'
const xtapefaacd20: re.TapeEval<'Zzz.zzz@gmail.com', xtapefstHalf> = ['Zzz.zzz', '@gmail.com', []]

type  ytapefstHalf = rc.Comp<'([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)'>['tapes']
const ytapefaacd0: re.TapeEval<'Zzzzzz@gmail.com', ytapefstHalf> = ['Zzzzzz', '@gmail.com', []]
const ytapefaacd20: re.TapeEval<'Zzz.zzz@gmail.com', ytapefstHalf> = ['Zzz.zzz', '@gmail.com', []]

type  ztapefstHalf = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))'>['tapes']
const ztapefaacd0: re.TapeEval<'Zzzzzz@gmail.com', ztapefstHalf> = ['Zzzzzz', '@gmail.com', []]
const ztapefaacd20: re.TapeEval<'Zzz.zzz@gmail.com', ztapefstHalf> = ['Zzz.zzz', '@gmail.com', []]

// -- second
// '((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'
type atapesndHalf = rc.Comp<'([a-zA-Z0-9-]+\\.)'>['tapes']
const atapefaacd0: re.TapeEval<'gmail.com', atapesndHalf> = ['gmail.', 'com', []]
const atapefaacd20: re.TapeEval<'gmail.com', atapesndHalf> = ['gmail.', 'com', []]

type btapesndHalf = rc.Comp<'(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,})'>['tapes']
const btapefaacd0: re.TapeEval<'gmail.com', btapesndHalf> = ['gmail.com', '', []]
const btapefaacd20: re.TapeEval<'gmail.com', btapesndHalf> = ['gmail.com', '', []]

type ctapesndHalf = rc.Comp<'(\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])'>['tapes']
const ctapefaacd0: re.TapeEval<'gmail.com', ctapesndHalf>['dumpString'] = 'gmail.com'
const ctapefaacd20: re.TapeEval<'gmail.com', ctapesndHalf>['dumpString'] = 'gmail.com'
const cctapefaacd0: re.TapeEval<'[123.456.789.1]', ctapesndHalf> = ['[123.456.789.1]', '', []]
const cctapefaacd20: re.TapeEval<'[123.456.789.101]', ctapesndHalf> = ['[123.456.789.101]', '', []]
const cctapefaacd30: re.TapeEval<'[123.456.789]', ctapesndHalf>['dumpString'] = '[123.456.789]'

type dtapesndHalf = rc.Comp<'((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>['tapes']
const dtapefaacd0: re.TapeEval<'gmail.com', dtapesndHalf> = ['gmail.com', '', []]
const dtapefaacd20: re.TapeEval<'gmail.com', dtapesndHalf> = ['gmail.com', '', []]
const cdtapefaacd0: re.TapeEval<'[123.456.789.1]', dtapesndHalf> = ['[123.456.789.1]', '', []]
const cdtapefaacd20: re.TapeEval<'[123.456.789.101]', dtapesndHalf> = ['[123.456.789.101]', '', []]
const cdtapefaacd30: re.TapeEval<'[123.456.789]', dtapesndHalf>['dumpString'] = '[123.456.789]'

// -- all
export type mailRegex = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>['tapes']
const test_mailreg_x: re.TapeEval<'Zzz.zzz@gmail.com', mailRegex> = ['Zzz.zzz@gmail.com', '', []]
const test_mailreg_xx: re.TapeEval<'Zzzzzz@gmail.com', mailRegex> = ['Zzzzzz@gmail.com', '', []]
const test_mailreg_xxx: re.TapeEval<'Zz1235z.zzz@gmail.com', mailRegex> = ['Zz1235z.zzz@gmail.com', '', []]
const test_mailreg_xxxx: re.TapeEval<'Zzz.zzz@[123.456.789.101]', mailRegex> = ['Zzz.zzz@[123.456.789.101]', '', []]
const test_mailreg_xxxxx: re.TapeEval<'Zzz.zzz{user}@[123.456.789.101]', mailRegex> = ['Zzz.zzz{user}@[123.456.789.101]', '', []]
