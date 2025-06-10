import type { regex } from '../src/regex'
import type * as Bit from '../src/bit'
import type { tZero, tOne, tTwo } from '../src/strutil'
import { vZero, vOne, vTwo } from '../src/strutil'
import type { Decimal } from '../src/decimal'

// --------------------
// -- main test
// --------------------

const evaltest_comp0: regex.ReadTape<'ssssssss', regex.Comp<'ssss'>> = ['ssss', 'ssss']

const test_compmm0: regex.CompMinMax<`{0,2}rest`> = [vZero, vTwo, 'rest']
const test_compmm1: regex.CompMinMax<`{1,2}rest`> = [vOne, vTwo, 'rest']
const test_compmm2: regex.CompMinMax<`{2,2}rest`> = [vTwo, vTwo, 'rest']
const test_compmm3: regex.CompMinMax<`{2,}rest`>  = [vTwo, '<', 'rest']
const test_compmm4: regex.CompMinMax<`{2}rest`>   = [vTwo, '=', 'rest']
const test_compmm0a: regex.CompMinMax<`{0,15}rest`> = [vZero, '0000000000001111', 'rest']
const test_compmm1a: regex.CompMinMax<`{15,16}rest`> = ['0000000000001111', '0000000000010000', 'rest']
const test_compmm2a: regex.CompMinMax<`{15}rest`> = ['0000000000001111', '=', 'rest']
const test_compmm3a: regex.CompMinMax<`{15,}rest`>  = ['0000000000001111', '<', 'rest']

// [todo] : test
const test_Transminmax0: regex.ExpandMinMax<['x'], Decimal.DtoB<'0'>, Decimal.DtoB<'3'>> = [[['x'], '?'],[['x'], '?'],[['x'], '?']]
const test_Transminmax1: regex.ExpandMinMax<['x'], Decimal.DtoB<'2'>, Decimal.DtoB<'3'>> = [['x'], ['x'],[['x'], '?']]
const test_Transminmax2: regex.ExpandMinMax<['x'], Decimal.DtoB<'3'>, '='> = [['x'], ['x'], ['x']]
const test_Transminmax3: regex.ExpandMinMax<['x'], Decimal.DtoB<'3'>, '<'> = [['x'], ['x'], ['x'],[['x'], '*']]

// test
const xtest_mloop0: regex.OrMatch<'xxxxx', ['xx']> = ['xx', 'xxx']
const xtest_mloop1: regex.OrMatch<'xxxxx', ['xx']> = ['xx', 'xxx']
const xtest_mloop1a: regex.OrMatch<'xxxxx', ['xx']> = ['xx', 'xxx']
const xtxest_mloop2: regex.OrMatch<'xxxxx', ['x']>  = ['x', 'xxxx']
const xtest_mloop3: regex.OrMatch<'xxxxxx', ['x']> = ['x', 'xxxxx']
const xtest_mloop2a: regex.OrMatch<'xxxxx', ['x']> = ['x', 'xxxx']

// test
const test_comp0: regex.Comp<'ssss'> = {condition: '', tape: [['s'],['s'],['s'],['s']]}
const test_comp0a: regex.Comp<'s(ss)s'> = {condition: '', tape: [['s'], ['ss'], ['s']]}
const test_comp0b: regex.Comp<'s(s|x)s'> = {condition: '', tape: [['s'], ['s', 'x'], ['s']]}
const test_comp0xa: regex.Comp<'s[ss]s'> = {condition: '', tape: [['s'], ['s', 's'], ['s']]}
const test_comp0xb: regex.Comp<'s[sx]s'> = {condition: '', tape: [['s'], ['s', 'x'], ['s']]}
const test_comp0y: regex.Comp<'ss+ss'> = {condition: '', tape: [['s'],['s'], [['s'], '*'] ,['s'],['s']]}
const test_comp0ya: regex.Comp<'s(ss)s'> = {condition: '', tape: [['s'], ['ss'], ['s']]}
const test_comp0yaa: regex.Comp<'s(ss)+s'> = {condition: '', tape: [['s'], ['ss'], [['ss'], '*'], ['s']]}
const test_comp0yaaa: regex.Comp<'s(ss|x)+s'> = {condition: '', tape: [['s'], ['ss', 'x'], [['ss', 'x'], '*'], ['s']]}
const test_comp0yb: regex.Comp<'s(s|x)s'> = {condition: '', tape: [['s'], ['s', 'x'], ['s']]}
const test_comp0yxa: regex.Comp<'s[ss]s'> = {condition: '', tape: [['s'], ['s', 's'], ['s']]}
const test_comp0yxb: regex.Comp<'s[sx]s'> = {condition: '', tape: [['s'], ['s', 'x'], ['s']]}
const test_comp0yxba: regex.Comp<'s[sx]?s'> = {condition: '', tape: [['s'], [['s', 'x'], '?'], ['s']]}
// -- ^
const test_fcomp0yb:  regex.Comp<'^s(s|x)s'> = {condition: '^', tape: [['s'], ['s', 'x'], ['s']]}
const test_fcomp0yxa: regex.Comp<'^s[ss]s'> = {condition: '^', tape: [['s'], ['s', 's'], ['s']]}
const test_fcomp0yxb: regex.Comp<'^s[sx]s'> = {condition: '^', tape: [['s'], ['s', 'x'], ['s']]}
// -- $
const test_tcomp0yb:  regex.Comp<'s(s|x)s$'> = {condition: '$', tape: [['s'], ['s', 'x'], ['s']]}
const test_tcomp0yxb: regex.Comp<'s[sx]s$'> = {condition: '$', tape: [['s'], ['s', 'x'], ['s']]}
const test_tcomp0yxx: regex.Comp<'^s[sx]s$'> = {condition: '^$', tape: [['s'], ['s', 'x'], ['s']]}
// -- {n,m}
const test_nmcompa0: regex.Comp<'s[sx]s{1,2}ss'> = {condition: '', tape: [['s'], ['s', 'x'], ['s'], [['s'], '?'], ['s'], ['s']]}
const test_nmcompa1: regex.Comp<'s[sx]{1,2}ss'> = {condition: '', tape: [['s'], ['s', 'x'], [['s', 'x'], '?'], ['s'], ['s']]}
const test_nmcompa0x: regex.Comp<'s[sx]s{2,2}ss'> = {condition: '', tape: [['s'], ['s', 'x'], ['s'], ['s'], ['s'], ['s']]}
const test_nmcompa1x: regex.Comp<'s[sx]{2}ss'> = {condition: '', tape: [['s'], ['s', 'x'], ['s', 'x'], ['s'], ['s']]}
const test_nmcompa0y: regex.Comp<'s[sx]s{2,}ss'> = {condition: '', tape: [['s'], ['s', 'x'], ['s'], ['s'], [['s'], '*'], ['s'], ['s']]}
const test_nmcompa1y: regex.Comp<'s[sx]{15,16}ss'> = {condition: '', tape: [['s'], ['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'], [['s', 'x'], '?'], ['s'], ['s']]}
const test_nmcompa1yy: regex.Comp<'s[sx]{15,}ss'> = {condition: '', tape: [['s'], ['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'], [['s', 'x'], '*'], ['s'], ['s']]}
const test_nmcompa1yy0: regex.Comp<'s[sx]{15}ss'> = {condition: '', tape: [['s'], ['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'],['s', 'x'], ['s'], ['s']]}

const test_nmcompa1ya: regex.Comp<'s\\d{1,2}ss'> = {condition: '', tape: [['s'], ['\\d'], [['\\d'], '?'], ['s'], ['s']]}
// todo : any
const test_nmcompa1yb: regex.Comp<'s[\\d]{15,16}ss'> = {condition: '', tape: [['s'], ['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],[['\\d'], '?'], ['s'], ['s']]}
// todo : any
const test_nmcompa1yc: regex.Comp<'s(\\d){15,16}ss'> = {condition: '', tape: [['s'], ['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],['\\d'],[['\\d'], '?'], ['s'], ['s']]}
// todo :any check
const test_nmcompa1yd: regex.Comp<'s(\\d|\\w){15,16}ss'> = {condition: '', tape: [['s'], ['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],['\\d', '\\w'],[['\\d', '\\w'], '?'], ['s'], ['s']]}
const test_nmcompa1yd1: regex.Comp<'s(\\d|w|s){15,16}ss'> =  {condition: '', tape: [['s'], ['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],['\\d', 'w', 's'],[['\\d', 'w', 's'], '?'], ['s'], ['s']]}
const test_nmcompa1ye: regex.Comp<'s[\\dab]{15,16}ss'> = {condition: '', tape: [['s'], ['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],['\\d', 'a', 'b'],[['\\d', 'a', 'b'], '?'], ['s'], ['s']]}
const test_nmcompa1yf: regex.Comp<'s[a\\db]{15,16}ss'> = {condition: '', tape: [['s'], ['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],['a', '\\d', 'b'],[['a', '\\d', 'b'], '?'], ['s'], ['s']]}

const evaltest_comp0a: regex.ReadTape<'ssssssss', regex.Comp<'s(ss)s'>> = ['ssss', 'ssss']
const evaltest_comp0b: regex.ReadTape<'ssssssss', regex.Comp<'s(s|x)s'>> = ['sss', 'sssss']
const evaltest_comp0xa: regex.ReadTape<'ssssssss', regex.Comp<'s[ss]s'>> = ['sss', 'sssss']
const evaltest_comp0xb: regex.ReadTape<'ssssssss', regex.Comp<'s[sx]s'>> = ['sss', 'sssss']
const evaltest_comp0g: regex.ReadTape<'ssssxssss', regex.Comp<'ssss'>> = ['ssss', 'xssss']
const evaltest_comp0g1: regex.ReadTape<'ssssxssss', regex.Comp<'ssx'>> = []
const evaltest_comp0ga: regex.ReadTape<'ssssxssss', regex.Comp<'s(ss)s'>> = ['ssss', 'xssss']
const evaltest_comp0gb: regex.ReadTape<'ssssxssss', regex.Comp<'s(s|x)s'>> = ['sss', 'sxssss']
const evaltest_comp0gxa: regex.ReadTape<'ssssxssss', regex.Comp<'s[ss]s'>> = ['sss', 'sxssss']
const evaltest_comp0gxb: regex.ReadTape<'ssssxssss', regex.Comp<'s[sx]s'>> = ['sss', 'sxssss']

const evaltest_comp0by: regex.ReadTape<'sssssxuss', regex.Comp<'ss+xu'>> = ['sssssxu', 'ss']
const evaltest_comp0byy: regex.ReadTape<'sssssssssssssssssxu', regex.Comp<'sss+xu'>> = ['sssssssssssssssssxu', '']
const evaltest_comp0y: regex.ReadTape<'ssssssss', regex.Comp<'ss+ss'>> = [] // illegal as greedy
const evaltest_comp0yy: regex.ReadTape<'ssssssss', regex.Comp<'sss+ss'>> = [] // illegal as greedy
const evaltest_comp0ya: regex.ReadTape<'ssssssss', regex.Comp<'s(ss)s'>> = ['ssss', 'ssss']

// todo
const evaltest_comp0yaa: regex.ReadTape<'ssssssss', regex.Comp<'s(ss)+s'>> = []
const _evaltest_comp0yaa: regex.Comp<'s(ss)+s'> = {condition: '', tape: [['s'], ['ss'], [['ss'], '*'], ['s']]}

// todo
const evaltest_comp0yaaa: regex.ReadTape<'ssssssss', regex.Comp<'s(ss|x)+s'>> = []
const _evaltest_comp0yaaa: regex.Comp<'s(ss|x)+s'> = {condition: '', tape: [['s'], ['ss', 'x'], [['ss', 'x'], '*'], ['s']]}

const evaltest_comp0yb: regex.ReadTape<'ssssssss', regex.Comp<'s(s|x)s'>> = ['sss', 'sssss']
const evaltest_comp0yxa: regex.ReadTape<'ssssssss', regex.Comp<'s[ss]s'>> = ['sss', 'sssss']
const evaltest_comp0yxb: regex.ReadTape<'ssssssss', regex.Comp<'s[sx]s'>> = ['sss', 'sssss']

const evaltestq_comp0: regex.ReadTape<'ssssssss', regex.Comp<'sss?s'>> = ['ssss', 'ssss']
const evaltestq_comp0a: regex.ReadTape<'ssssssss', regex.Comp<'s(ss)?s'>> = ['ssss', 'ssss']
const evaltestq_comp0b: regex.ReadTape<'ssssssss', regex.Comp<'s(s|x)?s'>> = ['sss', 'sssss']
const evaltestq_comp0xa: regex.ReadTape<'ssssssss', regex.Comp<'s[ss]?s'>> = ['sss', 'sssss']
const evaltestq_comp0xb: regex.ReadTape<'ssssssss', regex.Comp<'s[sx]?s'>> = ['sss', 'sssss']
const evaltestq_comp0g: regex.ReadTape<'ssssxssss', regex.Comp<'ss?ss'>> = ['ssss', 'xssss']
const evaltestq_comp0ga: regex.ReadTape<'ssssxssss', regex.Comp<'s(ss)?s'>> = ['ssss', 'xssss']
const evaltestq_comp0gb: regex.ReadTape<'ssssxssss', regex.Comp<'s(s|x)?s'>> = ['sss', 'sxssss']
const evaltestq_comp0gxa: regex.ReadTape<'ssssxssss', regex.Comp<'s[ss]?s'>> = ['sss', 'sxssss']
const evaltestq_comp0gxb: regex.ReadTape<'ssssxssss', regex.Comp<'s[sx]?s'>> = ['sss', 'sxssss']

const _evaltestq_comp0: regex.Comp<'sss?s'> = {condition: '', tape: [['s'],['s'],[['s'],'?'], ['s']]}
const _evaltestq_comp0a: regex.Comp<'s(ss)?s'> = {condition: '', tape: [['s'],[['ss'],'?'], ['s']]}
const _evaltestq_comp0b: regex.Comp<'s(s|x)?s'> = {condition: '', tape: [['s'],[['s', 'x'],'?'], ['s']]}
const _evaltestq_comp0xa: regex.Comp<'s[ss]?s'> = {condition: '', tape: [['s'],[['s', 's'],'?'], ['s']]}
const _evaltestq_comp0xb: regex.Comp<'s[sx]?s'> = {condition: '', tape: [['s'],[['s', 'x'],'?'], ['s']]}
const _evaltestq_comp0g:  regex.Comp<'ss?ss'> = {condition: '', tape: [['s'],[['s'],'?'], ['s'],['s']]}
const _evaltestq_comp0ga: regex.Comp<'s(ss)?s'> = {condition: '', tape: [['s'],[['ss'],'?'], ['s']]}
const _evaltestq_comp0gb: regex.Comp<'s(s|xx)?s'> = {condition: '', tape: [['s'],[['s', 'xx'],'?'], ['s']]}

const evaltestqf_comp0: regex.ReadTape<'ssssssss', regex.Comp<'ssx?s'>> = ['sss', 'sssss']
const evaltestqf_comp0a: regex.ReadTape<'ssssssss', regex.Comp<'s(ab)?s'>> = ['ss', 'ssssss']
const evaltestqf_comp0b: regex.ReadTape<'ssssssss', regex.Comp<'s(y|x)?s'>> = ['ss', 'ssssss']
const evaltestqf_comp0xa: regex.ReadTape<'ssssssss', regex.Comp<'s[jg]?s'>> = ['ss', 'ssssss']
const evaltestqf_comp0xb: regex.ReadTape<'ssssssss', regex.Comp<'s[kx]?s'>> = ['ss', 'ssssss']
const evaltestqf_comp0g: regex.ReadTape<'ssssxssss', regex.Comp<'sn?ss'>> = ['sss', 'sxssss']
const evaltestqf_comp0ga: regex.ReadTape<'ssssxssss', regex.Comp<'s(xd)?s'>> = ['ss', 'ssxssss']
const evaltestqf_comp0gb: regex.ReadTape<'ssssxssss', regex.Comp<'s(d|x)?s'>> = ['ss', 'ssxssss']
const evaltestqf_comp0gxa: regex.ReadTape<'ssssxssss', regex.Comp<'s[ed]?s'>> = ['ss', 'ssxssss']
const evaltestqf_comp0gxb: regex.ReadTape<'ssssxssss', regex.Comp<'sss[cx]?s'>> = ['ssss', 'xssss']
// todo : head
// const evaltestqf_comp0gxc: regex.ReadTape<'ssssxssss', regex.Comp<'ssss[cx]?s'>> = ['ssssxs', 'sss']
const evaltestqf_comp0gxc: regex.ReadTape<'ssssxssss', regex.Comp<'ssss[cx]?s'>> = ['ssssxs', 'sss']
const _evaltestqf_comp0gxb: regex.Comp<'sss[cx]?s'> = {condition: '', tape: [['s'],['s'],['s'],[['c', 'x'], '?'],['s'],]}
const _evaltestqf_comp0gxc: regex.Comp<'ssss[cx]?s'> = {condition: '', tape: [['s'],['s'],['s'],['s'],[['c', 'x'], '?'],['s'],]}

const evaltesttms_comp0: regex.ReadTape<'ssssssss', regex.Comp<'ss{1,2}x?s'>> = ['ssss', 'ssss']

const evaltesttms_comp0a1: regex.ReadTape<'ssabssss', regex.Comp<'s(ab){1,1}s'>> = []
const evaltesttms_comp0a2: regex.ReadTape<'ssabssss', regex.Comp<'s(ab){1,2}s'>> = []
const evaltesttms_comp0a3: regex.ReadTape<'ssabssss', regex.Comp<'ss(ab){0,2}s'>> = ['ssabs', 'sss']
const evaltesttms_comp0a4: regex.ReadTape<'ssssss', regex.Comp<'ss(ab){0,2}s'>> = ['sss', 'sss']
const evaltesttms_comp0a1x: regex.ReadTape<'ssababssss', regex.Comp<'ss(ab){1,1}s'>> = []
const evaltesttms_comp0a2x: regex.ReadTape<'ssababssss', regex.Comp<'ss(ab){1,2}s'>> = ['ssababs', 'sss']
const evaltesttms_comp0a3x: regex.ReadTape<'ssabssss', regex.Comp<'ss(ab){0,2}s'>> = ['ssabs', 'sss']
const evaltesttms_comp0a4x: regex.ReadTape<'ssssss', regex.Comp<'ss(ab){0,2}s'>> = ['sss', 'sss']
const evaltesttms_comp0b: regex.ReadTape<'ssssssss', regex.Comp<'s(y|x){1,2}s'>> = []
const evaltesttms_comp0bb: regex.ReadTape<'ssssssss', regex.Comp<'s(y|x){0,2}s'>> = ['ss', 'ssssss']
const evaltesttms_comp0xa: regex.ReadTape<'ssssssss', regex.Comp<'s[jg]{0,1}s'>> = ['ss', 'ssssss']
const evaltesttms_comp0xb: regex.ReadTape<'ssssssss', regex.Comp<'s[kx]{0,1}s'>> = ['ss', 'ssssss']
const evaltesttms_comp0g: regex.ReadTape<'ssssxssss', regex.Comp<'sn{0,1}ss'>> = ['sss', 'sxssss']
const evaltesttms_comp0ga: regex.ReadTape<'ssssxssss', regex.Comp<'s(xd){0,1}s'>> = ['ss', 'ssxssss']
const evaltesttms_comp0gb: regex.ReadTape<'ssssxssss', regex.Comp<'s(d|x)?s'>> = ['ss', 'ssxssss']
const evaltesttms_comp0gxa: regex.ReadTape<'ssssxssss', regex.Comp<'s[ed]?s'>> = ['ss', 'ssxssss']
const evaltesttms_comp0gxb: regex.ReadTape<'ssssxssss', regex.Comp<'sss[cx]?s'>> = ['ssss', 'xssss']


// now testing this
const evaltesttms_comp0bbz0s: regex.ReadTape<'syxsssssss', regex.Comp<'s(y|x){0,2}s'>> = ['syxs', 'ssssss']
const evaltesttms_comp0bbz0b: regex.ReadTape<'sxysssssss', regex.Comp<'s(y|x){0,2}s'>> = ['sxys', 'ssssss']
const _evaltesttms_comp0bbz0s: regex.Comp<'s(y|x){0,2}s'> = {condition: '', tape: [['s'], [['y', 'x'], '?'], [['y', 'x'], '?'], ['s']]}
const evaltesttms_comp0bbz0d: regex.ReadTape<'syxsssssss', regex.Comp<'s(y|x){1,2}s'>> = ['syxs', 'ssssss']
const evaltesttms_comp0bbz0e: regex.ReadTape<'sxysssssss', regex.Comp<'s(y|x){1,2}s'>> = ['sxys', 'ssssss']
const _evaltesttms_comp0bbz0d: regex.Comp<'s(y|x){1,2}s'> = {condition: '', tape: [['s'], ['y', 'x'], [['y', 'x'], '?'], ['s']]}
const evaltesttms_comp0bbz0y: regex.ReadTape<'syxsssssss', regex.Comp<'s(x|y){0,2}s'>> = ['syxs', 'ssssss']
const evaltesttms_comp0bbz0x: regex.ReadTape<'sxysssssss', regex.Comp<'s(x|y){0,2}s'>> = ['sxys', 'ssssss']
const evaltesttms_comp0bbz0w: regex.ReadTape<'syxsssssss', regex.Comp<'s(x|y){1,2}s'>> = ['syxs', 'ssssss']
const evaltesttms_comp0bbz0z: regex.ReadTape<'sxysssssss', regex.Comp<'s(x|y){1,2}s'>> = ['sxys', 'ssssss']
//this
const evaltesttms_comp0a0d: regex.ReadTape<'ssabssss', regex.Comp<'ss(ab){1,1}s'>> = ['ssabs', 'sss']
const evaltesttms_comp0a0e: regex.ReadTape<'sabssss', regex.Comp<'s(ab){1,1}s'>> = ['sabs', 'sss']

const evaltesttms_comp0bbz1: regex.ReadTape<'sxxsssssss', regex.Comp<'s(y|x){0,2}s'>> = ['sxxs', 'ssssss']
const evaltesttms_comp0bbz2: regex.ReadTape<'sxxsssssss', regex.Comp<'s(x|y){0,2}s'>> = ['sxxs', 'ssssss']
const evaltesttms_comp0bbz3: regex.ReadTape<'syysssssss', regex.Comp<'s(y|x){0,2}s'>> = ['syys', 'ssssss']

const evaltesttms_comp0vvz1: regex.ReadTape<'sxxsssssss', regex.Comp<'s(y|x){0,2}s'>> = ['sxxs', 'ssssss']
const evaltesttms_comp0vvz2: regex.ReadTape<'sxxsssssss', regex.Comp<'s(x|y){0,2}s'>> = ['sxxs', 'ssssss']
const evaltesttms_comp0vbz3: regex.ReadTape<'syysssssss', regex.Comp<'s(z|d){0,2}s'>> = []
const evaltesttms_comp0vbz3a: regex.ReadTape<'ssyysssssss', regex.Comp<'s(z|d){0,2}s'>> = ['ss', 'yysssssss']
const evaltesttms_comp0vbz3d: regex.ReadTape<'sssyysssssss', regex.Comp<'s(z|d){0,2}s'>> = ['ss', 'syysssssss']
const evaltesttms_comp0vbz3e: regex.ReadTape<'esssyysssssss', regex.Comp<'s(z|d){0,2}s'>> = []
const evaltesttms_comp0vbz4: regex.ReadTape<'ssssssss', regex.Comp<'s(z|d){0,2}s'>> = ['ss', 'ssssss']
const evaltesttms_comp0vbz5: regex.ReadTape<'szds', regex.Comp<'s(z|d){0,2}s'>> = ['szds', '']

// regex loop
const evaltesttms_comp0vbzf0: regex.RegexFind<'szds', 's(z|d){0,2}s'> = ['szds', '']
const evaltesttms_comp0vbzf2: regex.RegexFind<'xszds', 's(z|d){0,2}s'> = ['szds', '']
const evaltesttms_comp0vbzf2a: regex.RegexFind<'xsds', 's(z|d){0,2}s'> = ['sds', '']
const evaltesttms_comp0vbzf4: regex.RegexFind<'xszds', 's(z|d){0,2}s$'> = ['szds', '']
const evaltesttms_comp0vbzf4b: regex.RegexFind<'xszds', '^s(z|d){0,2}s'> = []
const evaltesttms_comp0vbzf6: regex.RegexFind<'xszds', 's(z|d){0,2}s'> = ['szds', '']

const evaltesttms_compdke0: regex.RegexFind<'xk?2', 'xk[d\\?]2'> = ['xk?2', '']
const evaltesttms_compdke1: regex.RegexFind<'xk?2', 'x[k\\?]{0,2}2'> = ['xk?2', '']
const evaltesttms_compdke2: regex.RegexFind<'xk?2', 'x(k\\?)2'> = ['xk?2', '']
const evaltesttms_compdke3: regex.RegexFind<'ddd12d1d', 'ddd\\d\\dd\\dd'> = ['ddd12d1d', '']
const evaltesttms_compdke4: regex.RegexFind<'xkd32', 'xk(d\\d){1,2}2'> = ['xkd32', '']
const evaltesttms_compdke5: regex.RegexFind<'xkd32', 'xk(d|\\d)2'> = []
const evaltesttms_compdke6: regex.RegexFind<'xkd345?2', 'xk(d|\\d){1,4}\\?2'> = ['xkd345?2', '']

const evaltesttms_regg3x: regex.RegexFind<'sz1s', 'sz\\d'> = ['sz1', 's']
const evaltesttms_comp0vbzfx0:  regex.RegexFind<'szds', 's(z|\\d){0,2}s'>  = []
const evaltesttms_comp0vbzfx0a: regex.RegexFind<'szds', 's[z\\d]{0,2}s'>   = []
const evaltesttms_comp0vbzfx2:  regex.RegexFind<'xszds', 's[zd\\d]{0,2}s'> = ['szds', '']
const evaltesttms_comp0vbzfx2a: regex.RegexFind<'s12s', 's[zd\\d]{0,2}s'>  = ['s12s', '']
const evaltesttms_comp0vbzfx2b: regex.RegexFind<'s12s', 's[zd12]{0,2}s'>   = ['s12s', '']
const evaltesttms_comp0vbzfx4:  regex.RegexFind<'xszds', 's(z|d){0,2}s$'>  = ['szds', '']
const evaltesttms_comp0vbzfx4b: regex.RegexFind<'xszds', '^s(z|d){0,2}s'>  = []
const evaltesttms_comp0vbzfx6:  regex.RegexFind<'xszds', 's(z|d){0,2}s'>   = ['szds', '']

const evaltesttms_comp0vbzfx61:  regex.RegexFind<'xs', 's[a-c]s'> = []
const _evaltesttms_comp0vbzfx61:  regex.Comp<'s[a-c]s'>   = {condition: '', tape: [['s'], ['a', 'b', 'c'], ['s']]}
const evaltesttms_comp0vbzfx61a:  regex.RegexFind<'scs', 's[a-c]s'> = ['scs', '']
const evaltesttms_comp0vbzfx61b:  regex.RegexFind<'sbs', 's[a-c]s'> = ['sbs', '']
const evaltesttms_comp0vbzfx61c:  regex.RegexFind<'sas', 's[a-c]s'> = ['sas', '']
const evaltesttms_comp0vbzfx61d:  regex.RegexFind<'sbacs', 's[abc]{0,3}s'> = ['sbacs', '']
const evaltesttms_comp0vbzfx61e:  regex.RegexFind<'sbacs', 's[abc]{1,3}s'> = ['sbacs', '']
const evaltesttms_comp0vbzfx61f:  regex.RegexFind<'sbacs', 's[abc]{2,3}s'> = ['sbacs', '']
const evaltesttms_comp0vbzfx61fb:  regex.RegexFind<'sabcs', 's[abc]{3,3}s'> = ['sabcs', '']
const evaltesttms_comp0vbzfx61g:  regex.RegexFind<'sbbacs', 's[abc]{2,4}s'> = ['sbbacs', '']
const evaltesttms_comp0vbzfx61h:  regex.RegexFind<'sbcs', 's[abc]{2,4}s'> = ['sbcs', '']
const evaltesttms_comp0vbzfx61f1:  regex.RegexFind<'sbas', 's[abc]+s'> = ['sbas', '']
const evaltesttms_comp0vbzfx61i:  regex.RegexFind<'sbbs', 's[abc]{2,4}s'> = ['sbbs', '']
const evaltesttms_comp0vbzfx61i1:  regex.RegexFind<'sbbbs', 's[abc]{2,4}s'> = ['sbbbs', '']
const evaltesttms_comp0vbzfx61i2:  regex.RegexFind<'sbbbbs', 's[abc]{2,4}s'> = ['sbbbbs', '']
const evaltesttms_comp0vbzfx61i3:  regex.RegexFind<'sbbbbbs', 's[abc]{2,4}s'> = []



type aaaa = regex.ReadInter<'a-b]s'>
