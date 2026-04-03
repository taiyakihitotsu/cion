import type Cion from '../src/index'

/** Simple arithmetic */
const Sum_5: Cion.Lisp<'(+ 2 3)'> = '5'
// @ts-expect-error
const Sum_5_error: Cion.Lisp<'(+ 2 3)'> = '6'

/** Division  */
type Div_2 = Cion.Lisp<'(/ 8 4)'>
//   ^? "2"
type Div_8_3 = Cion.Lisp<'(/ 16 6)'>
//   ^? "8/3"
type Div_nil = Cion.Lisp<'(/ 2 0)'>
//   ^? "nil"

/** Fmap */
type LocalFilter = '(filter (fn [x] (> x 2)))'
type LocalMap = '(map (fn [x] (* x 2)))' 
type Vec_6_8 = Cion.Lisp<`(->> [1 2 3 4] ${LocalFilter} ${LocalMap})`>
//   ^? "[6 8]"

/** Loop with fn + let */
type LocalFn = '(fn [r x] (if (>= 0 x) r (f (+ r 1) (- x 1))))'
type Loop_4 = Cion.Lisp<`(let [f ${LocalFn}] (f 1 3))`>
//   ^? "4"
// @ts-expect-error
const Loop_4_error: Loop_4 = '3'

/** Regex */
type Regex_abc = Cion.Lisp<`(re-find '[a-z]+' 'abc123')`>
//   ^? "'abc'"
// @ts-expect-error
const Regex_abc_error: Regex_abc = ''
