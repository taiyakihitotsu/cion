import type Cion from '../src/index'
import type { Filter } from '../src/index'

const testfilter0: Filter<
  [`fn`, [[`sym`, `a`]], [[`sym`, `eq`], [`sym`, `a`], [`prim`, 1]]],
  [`vec`, [`prim`, 0], [`prim`, 1], [`prim`, 1], [`prim`, 2]]
> = [`vec`, [`prim`, 1], [`prim`, 1]];

const maintest0_filter_0: Cion.RawLisp<'(filter (fn [n] (> 3 n)) [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
const maintest0_filter_1: Cion.RawLisp<'(let [f (fn [n] (> 3 n))] (filter f [0 1 2 3 4 5]))'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010']]
const maintest0_filter_2: Cion.RawLisp<'(filter number? [0 1 2 3 4 5])'> = ['vec', ['prim', '0000000000000000'], ['prim', '0000000000000001'], ['prim', '0000000000000010'], ['prim', '0000000000000011'], ['prim', '0000000000000100'], ['prim', '0000000000000101']]
