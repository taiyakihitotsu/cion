import type Cion from '../src/index'

const lisp_strtest0: Cion.Lisp<`(replace 'ababababa' 'b' 'a')`> = 'aaaaaaaaa'
const lisp_strtest1: Cion.Lisp<`(replace 'ab5468ba' '[\\d]' 'x')`> = 'abxxxxba'
const lisp_strtest2: Cion.Lisp<`(replace 'ababababa' 'x' 'a')`> = 'ababababa'

const lisp_strtest3: Cion.Lisp<`(replace 'ababababa' 'b' (fn [_ y _] 'a'))`> = 'aaaaaaaaa'
const lisp_strtest4: Cion.Lisp<`(replace 'ababababa' 'b' (fn [_ y _] (str y y)))`> = 'abbabbabbabba'

const lisp_strtebbbbst5x: Cion.Lisp<`(replace 'abcdefg' 'b' (fn [x y z] x))`> = 'aacdefg'
const lisp_strtest5y: Cion.Lisp<`(replace 'abcdefg' 'b' (fn [x y z] y))`> = 'abcdefg'
const lisp_strtest5z: Cion.Lisp<`(replace 'abcdefg' 'b' (fn [x y z] z))`> = 'acdefgcdefg'

const lisp_strtest5fx: Cion.Lisp<`(replace 'abcdefg' '[b-g]' (fn [x y z] x))`> = 'aa'
const lisp_strtest5fy: Cion.Lisp<`(replace 'abcdefg' '[b-g]' (fn [x y z] y))`> = 'abcdefg'
const lisp_strtest5fz: Cion.Lisp<`(replace 'abcdefg' '[b-g]' (fn [x y z] z))`> = 'acdefgdefgefgfgg'

const jke : Cion.Lisp<`((fn [x y z] z) '' '' ' ')`> = "' '"
