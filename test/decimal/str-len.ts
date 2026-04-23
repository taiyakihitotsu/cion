import type {Equal} from '../../src/util.js'
import type { StrLen } from '../../src/decimal/index.js'

const strlen_test_0: true = {} as Equal<StrLen<'3999'>, [[[[null]]]]>
const strlen_test_1: true = {} as Equal<StrLen<''>, null>
const strlen_test_2: true = {} as Equal<StrLen<'1'>, [null]>
