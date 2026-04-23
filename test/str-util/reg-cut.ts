import type * as strutil from '../../src/strutil.js'
import type { Equal } from '../../src/util.js'

const test_regcut_0: true = {} as Equal<['{', '1,}))rest'], strutil.RegCut<'{1,}))rest'>>
