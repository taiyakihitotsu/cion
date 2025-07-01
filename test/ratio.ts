import * as ratio from '../src/ratio'
import * as de from '../src/decimal'
import * as bit from '../src/bit'

const scaling_test0 : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'8'>, de.DtoB<'2'>]>> = [['10', '2'], ['8', '2']]
const scaling_test1 : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'8'>, de.DtoB<'3'>]>> = [['30', '6'], ['16', '6']]
const scaling_test2 : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'-10'>, de.DtoB<'2'>], [de.DtoB<'8'>, de.DtoB<'2'>]>> = [['-10', '2'], ['8', '2']]
const scaling_test2x : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'10'>, de.DtoB<'-2'>], [de.DtoB<'8'>, de.DtoB<'2'>]>> = [['20', '-4'], ['-16', '-4']]
const scaling_test2y : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'-10'>, de.DtoB<'-2'>], [de.DtoB<'8'>, de.DtoB<'2'>]>> = [['-20', '-4'], ['-16', '-4']]
const scaling_test2z : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'-10'>, de.DtoB<'2'>], [de.DtoB<'-8'>, de.DtoB<'2'>]>> = [['-10', '2'], ['-8', '2']]
const scaling_test2a : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'-10'>, de.DtoB<'2'>], [de.DtoB<'8'>, de.DtoB<'-2'>]>> = [['20', '-4'], ['16', '-4']]
const scaling_test2b : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'-10'>, de.DtoB<'2'>], [de.DtoB<'-8'>, de.DtoB<'-2'>]>> = [['20', '-4'], ['-16', '-4']]
const scaling_test2c : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'10'>, de.DtoB<'-2'>], [de.DtoB<'-8'>, de.DtoB<'2'>]>> = [['20', '-4'], ['16', '-4']]
const scaling_test2d : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'10'>, de.DtoB<'-2'>], [de.DtoB<'8'>, de.DtoB<'-2'>]>> = [['10', '-2'], ['8', '-2']]
const scaling_test2e : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'10'>, de.DtoB<'-2'>], [de.DtoB<'-8'>, de.DtoB<'-2'>]>> = [['10', '-2'], ['-8', '-2']]
const scaling_test2f : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'-10'>, de.DtoB<'-2'>], [de.DtoB<'-8'>, de.DtoB<'2'>]>> = [['-20', '-4'], ['16', '-4']]
const scaling_test2g : ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'-10'>, de.DtoB<'-2'>], [de.DtoB<'8'>, de.DtoB<'-2'>]>> = [['-10', '-2'], ['8', '-2']]
const _sc : de.BtoD<bit.BitMul<de.DtoB<'-2'>, de.DtoB<'-2'>>> = '4'

const lcd_test0: ratio.RatioStr<ratio.LCD<[de.DtoB<'-10'>, de.DtoB<'2'>]>> = '-5'
const lcd_test1: ratio.RatioStr<ratio.LCD<[de.DtoB<'-10'>, de.DtoB<'-2'>]>> = '5'
const lcd_test0x: ratio.RatioStr<ratio.LCD<[de.DtoB<'10'>, de.DtoB<'-2'>]>> = '-5'
const lcd_test1x: ratio.RatioStr<ratio.LCD<[de.DtoB<'10'>, de.DtoB<'2'>]>> = '5'
const lcd_test03: ratio.RatioStr<ratio.LCD<[de.DtoB<'-10'>, de.DtoB<'3'>]>> = '-10/3'
const lcd_test13: ratio.RatioStr<ratio.LCD<[de.DtoB<'-10'>, de.DtoB<'-3'>]>> = '10/3'
const lcd_test03x: ratio.RatioStr<ratio.LCD<[de.DtoB<'10'>, de.DtoB<'-3'>]>> = '-10/3'
const lcd_test13x: ratio.RatioStr<ratio.LCD<[de.DtoB<'10'>, de.DtoB<'3'>]>> = '10/3'
const alcd_test0: ratio.RatioStr<ratio.LCD<[de.DtoB<'-2'>, de.DtoB<'10'>]>> = '-1/5'
const alcd_test1: ratio.RatioStr<ratio.LCD<[de.DtoB<'-2'>, de.DtoB<'-10'>]>> = '1/5'
const alcd_test0x: ratio.RatioStr<ratio.LCD<[de.DtoB<'2'>, de.DtoB<'-10'>]>> = '-1/5'
const alcd_test1x: ratio.RatioStr<ratio.LCD<[de.DtoB<'2'>, de.DtoB<'10'>]>> = '1/5'
const alcd_test03: ratio.RatioStr<ratio.LCD<[de.DtoB<'-3'>, de.DtoB<'10'>]>> = '-3/10'
const alcd_test13: ratio.RatioStr<ratio.LCD<[de.DtoB<'-3'>, de.DtoB<'-10'>]>> = '3/10'
const alcd_test03x: ratio.RatioStr<ratio.LCD<[de.DtoB<'3'>, de.DtoB<'-10'>]>> = '-3/10'
const alcd_test13x: ratio.RatioStr<ratio.LCD<[de.DtoB<'3'>, de.DtoB<'10'>]>> = '3/10'

const ggg_test0: de.BtoD<ratio.GCM<de.DtoB<'21'>, de.DtoB<'21'>>> = '21'
const ggg_test1: de.BtoD<ratio.GCM<de.DtoB<'-21'>, de.DtoB<'21'>>> = '21'
const ggg_test2: de.BtoD<ratio.GCM<de.DtoB<'7'>, de.DtoB<'5'>>> = '1'
const ggg_test3: de.BtoD<ratio.GCM<de.DtoB<'7'>, de.DtoB<'15'>>> = '1'
const ggg_test4: de.BtoD<ratio.GCM<de.DtoB<'7'>, de.DtoB<'0'>>> = '7'
const ggg_test5: de.BtoD<ratio.GCM<de.DtoB<'9'>, de.DtoB<'3'>>> = '3'
const ggg_test6: de.BtoD<ratio.GCM<de.DtoB<'9'>, de.DtoB<'30'>>> = '3'
const ggg_test: de.BtoD<ratio.GCM<de.DtoB<'8'>, de.DtoB<'2'>>> = '2'

const abs_test0: de.BtoD<ratio.Abs<de.DtoB<'9'>>> = '9'
const abs_test1: de.BtoD<ratio.Abs<de.DtoB<'-9'>>> = '9'
const abs_test1e: de.BtoD<ratio.Abs<de.DtoB<'0'>>> = '0'
const abs_test2a: ratio.RatioStr<ratio.Abs<[de.DtoB<'9'>, de.DtoB<'3'>]>> = '3'
const abs_test2b: ratio.RatioStr<ratio.Abs<[de.DtoB<'-9'>, de.DtoB<'3'>]>> = '3'
const abs_test2c: ratio.RatioStr<ratio.Abs<[de.DtoB<'9'>, de.DtoB<'-3'>]>> = '3'
const abs_test2d: ratio.RatioStr<ratio.Abs<[de.DtoB<'-9'>, de.DtoB<'-3'>]>> = '3'
const abs_test3: ratio.RatioStr<ratio.Abs<[de.DtoB<'0'>, de.DtoB<'-3'>]>> = '0'

const not_test0: de.BtoD<ratio.Not<de.DtoB<'9'>>> = '-9'
const not_test1: de.BtoD<ratio.Not<de.DtoB<'-9'>>> = '9'
const not_test1e: de.BtoD<ratio.Not<de.DtoB<'0'>>> = '0'
const  not_test2a: ratio.RatioStr<ratio.Not<[de.DtoB<'9'>, de.DtoB<'3'>]>> = '-3'
const not_test2b: ratio.RatioStr<ratio.Not<[de.DtoB<'-9'>, de.DtoB<'3'>]>> = '3'
const not_test2c: ratio.RatioStr<ratio.Not<[de.DtoB<'9'>, de.DtoB<'-3'>]>> = '3'
const  not_test2d: ratio.RatioStr<ratio.Not<[de.DtoB<'-9'>, de.DtoB<'-3'>]>> = '-3'
const  not_test2ax: ratio.RatioStr<ratio.Not<[de.DtoB<'3'>, de.DtoB<'9'>]>> = '-1/3'
const not_test2bx: ratio.RatioStr<ratio.Not<[de.DtoB<'3'>, de.DtoB<'-9'>]>> = '1/3'
const not_test2cx: ratio.RatioStr<ratio.Not<[de.DtoB<'-3'>, de.DtoB<'9'>]>> = '1/3'
const  not_test2dx: ratio.RatioStr<ratio.Not<[ de.DtoB<'-3'>, de.DtoB<'-9'>]>> = '-1/3'
const  not_test3: ratio.RatioStr<ratio.Not<[de.DtoB<'0'>, de.DtoB<'-3'>]>> = '0'

const fr_test0 : ratio.DecimalRatio<ratio.ForceRatio<[de.DtoB<'10'>, de.DtoB<'2'>]>> = ['10', '2']
const fr_test1 : ratio.DecimalRatio<ratio.ForceRatio<de.DtoB<'3'>>> = ['3', '1']
const fr_test : ratio.DecimalRatio<ratio.ForceRatio<de.DtoB<'0'>>> = ['0', '1']

const fz_test0 : de.BtoD<ratio.ForceNat<[de.DtoB<'10'>, de.DtoB<'2'>]>> = '5'
const fz_test1 : de.BtoD<ratio.ForceNat<de.DtoB<'3'>>> = '3'
const fz_test2 : de.BtoD<ratio.ForceNat<[de.DtoB<'10'>, de.DtoB<'3'>]>> = '3'
const fz_test : de.BtoD<ratio.ForceNat<de.DtoB<'0'>>> = '0'


// -- add +, sub -, mul *, div /.
const add_test0 : ratio.DecimalRatio<ratio.Add<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'10'>, de.DtoB<'2'>]>> = ['20', '2']
const add_test : ratio.DecimalRatio<ratio.Add<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'10'>, de.DtoB<'3'>]>> = ['50', '6']
const sub_test0 : ratio.DecimalRatio<ratio.Sub<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'10'>, de.DtoB<'2'>]>> = ['0', '2']
const sub_test : ratio.DecimalRatio<ratio.Sub<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'10'>, de.DtoB<'3'>]>> = ['10', '6']
const mul_test0 : ratio.DecimalRatio<ratio.Mul<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'10'>, de.DtoB<'2'>]>> = ['100', '4']
const mul_test : ratio.DecimalRatio<ratio.Mul<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'10'>, de.DtoB<'3'>]>> = ['100', '6']
const div_test0 : ratio.DecimalRatio<ratio.Div<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'10'>, de.DtoB<'2'>]>> = ['20', '20']
const div_test1 : ratio.DecimalRatio<ratio.Div<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'10'>, de.DtoB<'3'>]>> = ['30', '20']
// @ts-expect-error:
const div_test : ratio.DecimalRatio<ratio.Div<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'0'>, de.DtoB<'3'>]>> = ['30', '20']

