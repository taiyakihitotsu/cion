import type Cion from '../../src/index.js'
import type { Equal } from '../../src/util.js'

// --- zipmap (Key-Value pairing) Tests ---

// Using keywords as keys
const zipmap_test_0 : true = {} as Equal<
  `{:a '0' :b '1'}`, 
  Cion.Lisp<`(zipmap [:a :b] ['0' '1'])`>
>

// Using numbers as keys
const zipmap_test_1 : true = {} as Equal<
  `{0 '0' 1 '1'}`, 
  Cion.Lisp<`(zipmap [0 1] ['0' '1'])`>
>

// Using strings as keys
const zipmap_test_2 : true = {} as Equal<
  `{'0k' '0' '1k' '1'}`, 
  Cion.Lisp<`(zipmap ['0k' '1k'] ['0' '1'])`>
>

// --- Combination with other Map functions ---

// zipmap followed by assoc (updating an existing key)
const zipmap_assoc_test_0 : true = {} as Equal<
  '{:a 2 :b 1}', 
  Cion.Lisp<`(assoc (zipmap [:a :b] [0 1]) :a 2)`>
>
