import * as ratio from '../../src/ratio'
import * as de from '../../src/decimal'
import * as bit from '../../src/bit'
import type { Equal } from '../../src/util'

// Scaling & DecimalRatio
const ratio_scale_test_0 : true = {} as Equal<[['10', '2'], ['8', '2']], ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'8'>, de.DtoB<'2'>]>>>
const ratio_scale_test_1 : true = {} as Equal<[['30', '6'], ['16', '6']], ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'8'>, de.DtoB<'3'>]>>>
const ratio_scale_test_2 : true = {} as Equal<[['-10', '2'], ['8', '2']], ratio.DecimalRatio<ratio.Scaling<[de.DtoB<'-10'>, de.DtoB<'2'>], [de.DtoB<'8'>, de.DtoB<'2'>]>>>

// RatioStr & LCD (Simplification)
const ratio_str_test_0 : true = {} as Equal<'-5', ratio.RatioStr<ratio.LCD<[de.DtoB<'-10'>, de.DtoB<'2'>]>>>
const ratio_str_test_1 : true = {} as Equal<'5',  ratio.RatioStr<ratio.LCD<[de.DtoB<'-10'>, de.DtoB<'-2'>]>>>
const ratio_str_test_2 : true = {} as Equal<'-10/3', ratio.RatioStr<ratio.LCD<[de.DtoB<'-10'>, de.DtoB<'3'>]>>>
const ratio_str_test_3 : true = {} as Equal<'1/5',   ratio.RatioStr<ratio.LCD<[de.DtoB<'-2'>, de.DtoB<'-10'>]>>>

// GCM (Greatest Common Measure)
const gcm_test_0 : true = {} as Equal<'21', de.BtoD<ratio.GCM<de.DtoB<'21'>, de.DtoB<'21'>>>>
const gcm_test_1 : true = {} as Equal<'1',  de.BtoD<ratio.GCM<de.DtoB<'7'>, de.DtoB<'5'>>>>
const gcm_test_2 : true = {} as Equal<'7',  de.BtoD<ratio.GCM<de.DtoB<'7'>, de.DtoB<'0'>>>>
const gcm_test_3 : true = {} as Equal<'3',  de.BtoD<ratio.GCM<de.DtoB<'9'>, de.DtoB<'30'>>>>

// Abs & Not (Unary Operations)
const abs_test_0 : true = {} as Equal<'9', de.BtoD<ratio.Abs<de.DtoB<'-9'>>>>
const abs_test_1 : true = {} as Equal<'3', ratio.RatioStr<ratio.Abs<[de.DtoB<'-9'>, de.DtoB<'3'>]>>>
const not_test_0 : true = {} as Equal<'-9', de.BtoD<ratio.Not<de.DtoB<'9'>>>>
const not_test_1 : true = {} as Equal<'1/3', ratio.RatioStr<ratio.Not<[de.DtoB<'-3'>, de.DtoB<'9'>]>>>

// ForceRatio & ForceNat
const force_test_0 : true = {} as Equal<['10', '2'], ratio.DecimalRatio<ratio.ForceRatio<[de.DtoB<'10'>, de.DtoB<'2'>]>>>
const force_test_1 : true = {} as Equal<['3', '1'],  ratio.DecimalRatio<ratio.ForceRatio<de.DtoB<'3'>>>>
const force_nat_test_0 : true = {} as Equal<'5', de.BtoD<ratio.ForceNat<[de.DtoB<'10'>, de.DtoB<'2'>]>>>
const force_nat_test_1 : true = {} as Equal<'3', de.BtoD<ratio.ForceNat<[de.DtoB<'10'>, de.DtoB<'3'>]>>>

// Arithmetic Operations (+, -, *, /)
const arith_test_add : true = {} as Equal<['50', '6'], ratio.DecimalRatio<ratio.Add<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'10'>, de.DtoB<'3'>]>>>
const arith_test_sub : true = {} as Equal<['10', '6'], ratio.DecimalRatio<ratio.Sub<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'10'>, de.DtoB<'3'>]>>>
const arith_test_mul : true = {} as Equal<['100', '6'], ratio.DecimalRatio<ratio.Mul<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'10'>, de.DtoB<'3'>]>>>
const arith_test_div : true = {} as Equal<['30', '20'], ratio.DecimalRatio<ratio.Div<[de.DtoB<'10'>, de.DtoB<'2'>], [de.DtoB<'10'>, de.DtoB<'3'>]>>>

// Floor & Trunc (Rounding)
const floor_test_0 : true = {} as Equal<'5', de.BtoD<ratio.Floor<[de.DtoB<'11'>, de.DtoB<'2'>]>>>
const floor_test_1 : true = {} as Equal<'-6', de.BtoD<ratio.Floor<[de.DtoB<'11'>, de.DtoB<'-2'>]>>>
const floor_test_2 : true = {} as Equal<'-1', de.BtoD<ratio.Floor<[de.DtoB<'-3'>, de.DtoB<'10'>]>>>

const trunc_test_0 : true = {} as Equal<'5', de.BtoD<ratio.Trunc<[de.DtoB<'11'>, de.DtoB<'2'>]>>>
const trunc_test_1 : true = {} as Equal<'-5', de.BtoD<ratio.Trunc<[de.DtoB<'11'>, de.DtoB<'-2'>]>>>
const trunc_test_2 : true = {} as Equal<'0', de.BtoD<ratio.Trunc<[de.DtoB<'-3'>, de.DtoB<'10'>]>>>

// Simplify & Commonize
const simplify_test_0 : true = {} as Equal<'4',    ratio.SimplifyStr<[de.DtoB<'8'>, de.DtoB<'2'>]>>
const simplify_test_1 : true = {} as Equal<'8/3',  ratio.SimplifyStr<[de.DtoB<'8'>, de.DtoB<'3'>]>>
const simplify_test_2 : true = {} as Equal<'-8/3', ratio.SimplifyStr<[de.DtoB<'-8'>, de.DtoB<'3'>]>>
