import type * as strutil from '../../src/strutil.js'
import type { Equal } from '../../src/util.js'

const test_somelen_same: true = {} as Equal<true, strutil.SomeLen<'sss', 'xxa'>>
const test_somelen_diff: true = {} as Equal<false, strutil.SomeLen<'sss', 'sxxa'>>
const test_somelen_tar_empty: true = {} as Equal<false, strutil.SomeLen<'sss', ''>>
const test_somelen_src_empty: true = {} as Equal<false, strutil.SomeLen<'', 'xxa'>>
const test_somelen_both_empty: true = {} as Equal<true, strutil.SomeLen<'', ''>>
