import type Cion from '../../src/index.js'
import type {Equal} from '../../src/util.js'

const loop_test : true = {} as Equal<['prim', true], Cion.RawLisp<'(let [f (fn [i] (if (> i 0) (f (- i 2)) true))] (f 10))'>>
