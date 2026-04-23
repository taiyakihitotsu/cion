import type { IsInt } from "../../src/ratio/index.js"
import type { Equal } from "../../src/util.js"

// ------------
// -- Bit
// ------------
const Bit_Expected_true: true = {} as Equal<true, IsInt<"00010">>
const Empty_Expected_false: true = {} as Equal<false, IsInt<"">>
const NotBit_Expected_false: true = {} as Equal<false, IsInt<"-1">>

// ------------
// -- Ratio
// ------------
const Ratio_Expected_true: true = {} as Equal<true, IsInt<["00010", "00010"]>>
const Ratio_Both_Empty_Expected_false: true = {} as Equal<false, IsInt<["", ""]>>
const Ratio_Left_Empty_Expected_false: true = {} as Equal<false, IsInt<["", "00010"]>>
const Ratio_Right_Empty_Expected_false: true = {} as Equal<false, IsInt<["00010", ""]>>
const NotRatio_Both_Expected_false: true = {} as Equal<false, IsInt<["-1", "-1"]>>
const NotRatio_Left_Expected_false: true = {} as Equal<false, IsInt<["-1", "00010"]>>
const NotRatio_Right_Expected_false: true = {} as Equal<false, IsInt<["00010", "-1"]>>
