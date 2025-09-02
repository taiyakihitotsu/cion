import type Cion from "../src/index.ts"


const zipmaptest_0 : Cion.Lisp<`(zipmap [:a :b] ['0' '1'])`> = `{:a '0' :b '1'}`
const zipmaptest_1 : Cion.Lisp<`(zipmap [0 1] ['0' '1'])`> = `{0 '0' 1 '1'}`
const zipmaptest_2 : Cion.Lisp<`(zipmap ['0k' '1k'] ['0' '1'])`> = `{'0k' '0' '1k' '1'}`

const zipmaptest_3 : Cion.Lisp<`(assoc (zipmap [:a :b] [0 1]) :a 2)`> = '{:a 2 :b 1}'
