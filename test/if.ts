import type Cion from '../src/index.ts'

// todo :
// string split works but not correctly, in current.
// Use _ as space until I will have implemented a string parser. 
const maintest4: Cion.RawLisp<"(let [a 'a'] (if (eq a 'a') 'this_is_true', 'this_is_false')"> = [`prim`, "'this_is_false'"]
const maintest5: Cion.RawLisp<"(if true 1 2)"> = ['prim', '0000000000000001']
const maintest6: Cion.RawLisp<"(if true (let [a 'astr' b 'bstr'] (str a b)) 11)"> = ['prim', `'astrbstr'`]
