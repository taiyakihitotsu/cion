import type regex from '../src/regex'
import type Bit from '../src/bit'
import type { tZero, tOne, tTwo } from '../src/strutil'
import { vZero, vOne, vTwo } from '../src/strutil'

// type  Zero = '0000000000000000'
// type  One  = '0000000000000001'
// type  Two  = '0000000000000010'
// const Zero = '0000000000000000'
// const One  = '0000000000000001'
// const Two  = '0000000000000010'

// note : should test zero pattern.
const test_mloop0: regex.MatchLoop<'xxxxx', 'xx', 'char', tOne>     = [['xx', 'xxx'], vOne]
const test_mloop1: regex.MatchLoop<'xxxxx', 'xx', 'pattern'>       = [['xx', 'x'], '0000000000000010']
const test_mloop1a: regex.MatchLoop<'xxxxx', 'xx', 'pattern', tOne> = [['xx', 'xxx'], vOne]
const test_mloop2: regex.MatchLoop<'xxxxx', 'x', 'char'>           = [['x', ''], '0000000000000101']
const test_mloop3: regex.MatchLoop<'xxxxxx', 'x', 'char'>          = [['x', ''], '0000000000000110']
const test_mloop2a: regex.MatchLoop<'xxxxx', 'x', 'char', Bit.BitInc<tOne>> = [['x', 'xxx'], '0000000000000010']
const test_mloop3a: regex.MatchLoop<'xxxxxx', 'x', 'char', Bit.BitInc<Bit.BitInc<tOne>>> = [['x', 'xxx'], '0000000000000011']
const test_mloop4: regex.MatchLoop<'xxxyxxx', 'x', 'char'>                  = [['x', 'yxxx'], '0000000000000011']
const test_mloop5: regex.MatchLoop<'xxxyxxx', 'x', 'char', Bit.BitInc<tOne>> = [['x', 'xyxxx'], '0000000000000010']
// dot
const dot_test_mloop0: regex.MatchLoop<'xxxxx', 'xx', 'char', tOne>     = [['xx', 'xxx'], vOne]
const dot_test_mloop1: regex.MatchLoop<'xxxxx', 'xx', 'pattern'>       = [['xx', 'x'], '0000000000000010']
const dot_test_mloop1a: regex.MatchLoop<'xxxxx', 'xx', 'pattern', tOne> = [['xx', 'xxx'], vOne]
const dot_test_mloop2: regex.MatchLoop<'xxxxx', 'x', 'char'>           = [['x', ''], '0000000000000101']
const dot_test_mloop3: regex.MatchLoop<'xxxxxx', 'x', 'char'>          = [['x', ''], '0000000000000110']
const dot_test_mloop2a: regex.MatchLoop<'xxxxx', 'x', 'char', Bit.BitInc<tOne>> = [['x', 'xxx'], '0000000000000010']
const dot_test_mloop3a: regex.MatchLoop<'xxxxxx', 'x', 'char', Bit.BitInc<Bit.BitInc<tOne>>> = [['x', 'xxx'], '0000000000000011']
const dot_test_mloop4: regex.MatchLoop<'xxxyxxx', 'x', 'char'> = [['x', 'yxxx'], '0000000000000011']
const dot_test_mloop5: regex.MatchLoop<'xxxyxxx', 'x', 'char', Bit.BitInc<tOne>> = [['x', 'xyxxx'], '0000000000000010']

// test
const test_comp0: regex.Comp<'ssss'> = {condition: '', tape: [['s'],['s'],['s'],['s']]}
const test_comp0a: regex.Comp<'s(ss)s'> = {condition: '', tape: [['s'], ['ss'], ['s']]}
const test_comp0b: regex.Comp<'s(s|x)s'> = {condition: '', tape: [['s'], ['s', 'x'], ['s']]}
const test_comp0xa: regex.Comp<'s[ss]s'> = {condition: '', tape: [['s'], ['s', 's'], ['s']]}
const test_comp0xb: regex.Comp<'s[sx]s'> = {condition: '', tape: [['s'], ['s', 'x'], ['s']]}
const test_comp0y: regex.Comp<'ss+ss'> = {condition: '', tape: [['s'],['s'], [['s'], '*'] ,['s'],['s']]}
const test_comp0yy: regex.Comp<'sss+ss'> = {condition: '', tape: [['s'],['s'],['s'],[['s'], '*'] ,['s'],['s']]}
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
const test_tcomp0yxa: regex.Comp<'s[ss]s$'> = {condition: '$', tape: [['s'], ['s', 's'], ['s']]}
const test_tcomp0yxb: regex.Comp<'s[sx]s$'> = {condition: '$', tape: [['s'], ['s', 'x'], ['s']]}
const test_tcomp0yxx: regex.Comp<'^s[sx]s$'> = {condition: '^$', tape: [['s'], ['s', 'x'], ['s']]}
// -- {n,m}
const test_nmcompa0: regex.Comp<'s[sx]s{1,2}ss'> = {condition: '', tape: [['s'], ['s', 'x'], [['s'], [['0000000000000001', '0000000000000010'], 'times']], ['s'], ['s']]}
const test_nmcompa1: regex.Comp<'s[sx]{1,2}ss'> = {condition: '', tape: [['s'], [['s', 'x'], [['0000000000000001', '0000000000000010'], 'times']], ['s'], ['s']]}
const test_nmcompa0x: regex.Comp<'s[sx]s{1,15}ss'> = {condition: '', tape: [['s'], ['s', 'x'], [['s'], [['0000000000000001', '0000000000001111'], 'times']], ['s'], ['s']]}
const test_nmcompa1x: regex.Comp<'s[sx]{1,15}ss'> = {condition: '', tape: [['s'], [['s', 'x'], [['0000000000000001', '0000000000001111'], 'times']], ['s'], ['s']]}
const test_nmcompa0y: regex.Comp<'s[sx]s{15,16}ss'> = {condition: '', tape: [['s'], ['s', 'x'], [['s'], [['0000000000001111', '0000000000010000'], 'times']], ['s'], ['s']]}
const test_nmcompa1y: regex.Comp<'s[sx]{15,16}ss'> = {condition: '', tape: [['s'], [['s', 'x'], [['0000000000001111', '0000000000010000'], 'times']], ['s'], ['s']]}


// test
const evaltest_comp0: regex.ReadTape<'ssssssss', regex.Comp<'ssss'>> = ['s', 'ssss']
const evaltest_comp0a: regex.ReadTape<'ssssssss', regex.Comp<'s(ss)s'>> = ['s', 'ssss']
const evaltest_comp0b: regex.ReadTape<'ssssssss', regex.Comp<'s(s|x)s'>> = ['s', 'sssss']
const evaltest_comp0xa: regex.ReadTape<'ssssssss', regex.Comp<'s[ss]s'>> = ['s', 'sssss']
const evaltest_comp0xb: regex.ReadTape<'ssssssss', regex.Comp<'s[sx]s'>> = ['s', 'sssss']
const evaltest_comp0g: regex.ReadTape<'ssssxssss', regex.Comp<'ssss'>> = ['s', 'xssss']
const evaltest_comp0g1: regex.ReadTape<'ssssxssss', regex.Comp<'ssx'>> = []
const evaltest_comp0ga: regex.ReadTape<'ssssxssss', regex.Comp<'s(ss)s'>> = ['s', 'xssss']
const evaltest_comp0gb: regex.ReadTape<'ssssxssss', regex.Comp<'s(s|x)s'>> = ['s', 'sxssss']
const evaltest_comp0gxa: regex.ReadTape<'ssssxssss', regex.Comp<'s[ss]s'>> = ['s', 'sxssss']
const evaltest_comp0gxb: regex.ReadTape<'ssssxssss', regex.Comp<'s[sx]s'>> = ['s', 'sxssss']

const evaltest_comp0by: regex.ReadTape<'sssssxuss', regex.Comp<'ss+xu'>> = ['u', 'ss']
const evaltest_comp0byy: regex.ReadTape<'sssssssssssssssssxu', regex.Comp<'sss+xu'>> = ['u', '']
const evaltest_comp0y: regex.ReadTape<'ssssssss', regex.Comp<'ss+ss'>> = [] // illegal as greedy
const evaltest_comp0yy: regex.ReadTape<'ssssssss', regex.Comp<'sss+ss'>> = [] // illegal as greedy
const evaltest_comp0ya: regex.ReadTape<'ssssssss', regex.Comp<'s(ss)s'>> = ['s', 'ssss']
const evaltest_comp0yaa: regex.ReadTape<'ssssssss', regex.Comp<'s(ss)+s'>> = ['s', '']
const evaltest_comp0yaaa: regex.ReadTape<'ssssssss', regex.Comp<'s(ss|x)+s'>> = ['s', '']
const evaltest_comp0yb: regex.ReadTape<'ssssssss', regex.Comp<'s(s|x)s'>> = ['s', 'sssss']
const evaltest_comp0yxa: regex.ReadTape<'ssssssss', regex.Comp<'s[ss]s'>> = ['s', 'sssss']
const evaltest_comp0yxb: regex.ReadTape<'ssssssss', regex.Comp<'s[sx]s'>> = ['s', 'sssss']

const evaltestq_comp0: regex.ReadTape<'ssssssss', regex.Comp<'sss?s'>> = ['s', 'ssss']
const evaltestq_comp0a: regex.ReadTape<'ssssssss', regex.Comp<'s(ss)?s'>> = ['s', 'ssss']
const evaltestq_comp0b: regex.ReadTape<'ssssssss', regex.Comp<'s(s|x)?s'>> = ['s', 'sssss']
const evaltestq_comp0xa: regex.ReadTape<'ssssssss', regex.Comp<'s[ss]?s'>> = ['s', 'sssss']
const evaltestq_comp0xb: regex.ReadTape<'ssssssss', regex.Comp<'s[sx]?s'>> = ['s', 'sssss']
const evaltestq_comp0g: regex.ReadTape<'ssssxssss', regex.Comp<'ss?ss'>> = ['s', 'xssss']
const evaltestq_comp0ga: regex.ReadTape<'ssssxssss', regex.Comp<'s(ss)?s'>> = ['s', 'xssss']
const evaltestq_comp0gb: regex.ReadTape<'ssssxssss', regex.Comp<'s(s|x)?s'>> = ['s', 'sxssss']
const evaltestq_comp0gxa: regex.ReadTape<'ssssxssss', regex.Comp<'s[ss]?s'>> = ['s', 'sxssss']
const evaltestq_comp0gxb: regex.ReadTape<'ssssxssss', regex.Comp<'s[sx]?s'>> = ['s', 'sxssss']

const evaltestqf_comp0: regex.ReadTape<'ssssssss', regex.Comp<'ssx?s'>> = ['s', 'sssss']
const evaltestqf_comp0a: regex.ReadTape<'ssssssss', regex.Comp<'s(ab)?s'>> = ['s', 'ssssss']
const evaltestqf_comp0b: regex.ReadTape<'ssssssss', regex.Comp<'s(y|x)?s'>> = ['s', 'ssssss']
const evaltestqf_comp0xa: regex.ReadTape<'ssssssss', regex.Comp<'s[jg]?s'>> = ['s', 'ssssss']
const evaltestqf_comp0xb: regex.ReadTape<'ssssssss', regex.Comp<'s[kx]?s'>> = ['s', 'ssssss']
const evaltestqf_comp0g: regex.ReadTape<'ssssxssss', regex.Comp<'sn?ss'>> = ['s', 'sxssss']
const evaltestqf_comp0ga: regex.ReadTape<'ssssxssss', regex.Comp<'s(xd)?s'>> = ['s', 'ssxssss']
const evaltestqf_comp0gb: regex.ReadTape<'ssssxssss', regex.Comp<'s(d|x)?s'>> = ['s', 'ssxssss']
const evaltestqf_comp0gxa: regex.ReadTape<'ssssxssss', regex.Comp<'s[ed]?s'>> = ['s', 'ssxssss']
const evaltestqf_comp0gxb: regex.ReadTape<'ssssxssss', regex.Comp<'sss[cx]?s'>> = ['s', 'xssss']
const evaltestqf_comp0gxc: regex.ReadTape<'ssssxssss', regex.Comp<'ssss[cx]?s'>> = ['s', 'sss']

const evaltesttms_comp0: regex.ReadTape<'ssssssss', regex.Comp<'ss{1,2}x?s'>> = ['s', 'ssss']

const evaltesttms_comp0a1: regex.ReadTape<'ssabssss', regex.Comp<'s(ab){1,1}s'>> = []
const evaltesttms_comp0a2: regex.ReadTape<'ssabssss', regex.Comp<'s(ab){1,2}s'>> = []
const evaltesttms_comp0a3: regex.ReadTape<'ssabssss', regex.Comp<'ss(ab){0,2}s'>> = ['s', 'sss']
const evaltesttms_comp0a4: regex.ReadTape<'ssssss', regex.Comp<'ss(ab){0,2}s'>> = ['s', 'sss']
const evaltesttms_comp0a1x: regex.ReadTape<'ssababssss', regex.Comp<'ss(ab){1,1}s'>> = []
const evaltesttms_comp0a2x: regex.ReadTape<'ssababssss', regex.Comp<'ss(ab){1,2}s'>> = ['s', 'sss']
const evaltesttms_comp0a3x: regex.ReadTape<'ssabssss', regex.Comp<'ss(ab){0,2}s'>> = ['s', 'sss']
const evaltesttms_comp0a4x: regex.ReadTape<'ssssss', regex.Comp<'ss(ab){0,2}s'>> = ['s', 'sss']
const evaltesttms_comp0b: regex.ReadTape<'ssssssss', regex.Comp<'s(y|x){1,2}s'>> = []
const evaltesttms_comp0bb: regex.ReadTape<'ssssssss', regex.Comp<'s(y|x){0,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0xa: regex.ReadTape<'ssssssss', regex.Comp<'s[jg]{0,1}s'>> = ['s', 'ssssss']
const evaltesttms_comp0xb: regex.ReadTape<'ssssssss', regex.Comp<'s[kx]{0,1}s'>> = ['s', 'ssssss']
const evaltesttms_comp0g: regex.ReadTape<'ssssxssss', regex.Comp<'sn{0,1}ss'>> = ['s', 'sxssss']
const evaltesttms_comp0ga: regex.ReadTape<'ssssxssss', regex.Comp<'s(xd){0,1}s'>> = ['s', 'ssxssss']
const evaltesttms_comp0gb: regex.ReadTape<'ssssxssss', regex.Comp<'s(d|x)?s'>> = ['s', 'ssxssss']
const evaltesttms_comp0gxa: regex.ReadTape<'ssssxssss', regex.Comp<'s[ed]?s'>> = ['s', 'ssxssss']
const evaltesttms_comp0gxb: regex.ReadTape<'ssssxssss', regex.Comp<'sss[cx]?s'>> = ['s', 'xssss']
const evaltesttms_comp0gxc: regex.ReadTape<'ssssxssss', regex.Comp<'ssss[cx]?s'>> = ['s', 'sss']

// now testing this
const evaltesttms_comp0bbz0s: regex.ReadTape<'syxsssssss', regex.Comp<'s(y|x){0,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0bbz0b: regex.ReadTape<'sxysssssss', regex.Comp<'s(y|x){0,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0bbz0d: regex.ReadTape<'syxsssssss', regex.Comp<'s(y|x){1,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0bbz0e: regex.ReadTape<'sxysssssss', regex.Comp<'s(y|x){1,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0bbz0y: regex.ReadTape<'syxsssssss', regex.Comp<'s(x|y){0,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0bbz0x: regex.ReadTape<'sxysssssss', regex.Comp<'s(x|y){0,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0bbz0w: regex.ReadTape<'syxsssssss', regex.Comp<'s(x|y){1,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0bbz0z: regex.ReadTape<'sxysssssss', regex.Comp<'s(x|y){1,2}s'>> = ['s', 'ssssss']
//this
const evaltesttms_comp0a0d: regex.ReadTape<'ssabssss', regex.Comp<'ss(ab){1,1}s'>> = ['s', 'sss']
const evaltesttms_comp0a0e: regex.ReadTape<'sabssss', regex.Comp<'s(ab){1,1}s'>> = ['s', 'sss']

const evaltesttms_comp0bbz1: regex.ReadTape<'sxxsssssss', regex.Comp<'s(y|x){0,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0bbz2: regex.ReadTape<'sxxsssssss', regex.Comp<'s(x|y){0,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0bbz3: regex.ReadTape<'syysssssss', regex.Comp<'s(y|x){0,2}s'>> = ['s', 'ssssss']

const evaltesttms_comp0vvz1: regex.ReadTape<'sxxsssssss', regex.Comp<'s(y|x){0,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0vvz2: regex.ReadTape<'sxxsssssss', regex.Comp<'s(x|y){0,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0vbz3: regex.ReadTape<'syysssssss', regex.Comp<'s(z|d){0,2}s'>> = []
const evaltesttms_comp0vbz3a: regex.ReadTape<'ssyysssssss', regex.Comp<'s(z|d){0,2}s'>> = ['s', 'yysssssss']
const evaltesttms_comp0vbz3d: regex.ReadTape<'sssyysssssss', regex.Comp<'s(z|d){0,2}s'>> = ['s', 'syysssssss']
const evaltesttms_comp0vbz3e: regex.ReadTape<'esssyysssssss', regex.Comp<'s(z|d){0,2}s'>> = []
const evaltesttms_comp0vbz4: regex.ReadTape<'ssssssss', regex.Comp<'s(z|d){0,2}s'>> = ['s', 'ssssss']
const evaltesttms_comp0vbz5: regex.ReadTape<'szds', regex.Comp<'s(z|d){0,2}s'>> = ['s', '']
