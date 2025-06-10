import type { VecUtil as v} from '../src/vecutil'

const test_repeat0: v.Repeat<'0000000000000011', ['2']> = [['2'], ['2'], ['2']]
const test_repeat1: v.Repeat<'0000000000000000', ['2']> = []
