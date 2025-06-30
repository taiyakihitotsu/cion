import type Cion from '../src/index'

const lisp_strtest0: Cion.Lisp<`(split 'ababababa' 'b')`> = "['a' 'a' 'a' 'a' 'a']"
const lisp_strtest1a: Cion.Lisp<`(split 'ad89df89' '[89]')`> = "['ad' 'df']"
const lisp_strtest1: Cion.Lisp<`(split 'ad89df89' '[89]+')`> = "['ad' 'df']"
const lisp_strtest2: Cion.Lisp<`(split 'ad89df89zz' '[89]+')`> = "['ad' 'df' 'zz']"
