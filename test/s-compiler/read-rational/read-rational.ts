import type {ReadRational} from '../../../src/s-compiler/index.js'
import type {Equal} from '../../../src/util.js'

const Expected_3_2: true = {} as Equal<['3', '2'], ReadRational<`3/2`>>
const Expected_neg3_2: true = {} as Equal<['-3', '2'], ReadRational<`-3/2`>>
const Expected_neg3: true = {} as Equal<['-3'], ReadRational<`-3`>>
const Expected_string_error0: true = {} as Equal<[], ReadRational<`str`>>
const Expected_string_error1: true = {} as Equal<[], ReadRational<`2str`>>
const Expected_lint_error: true = {} as Equal<[], ReadRational<`3  /2`>>
