import type Cion from '../src/index.ts'

const maintest0_eq: Cion.RawLisp<"(eq 'a' 'b')"> = [`prim`, false]
const maintest1_eq: Cion.RawLisp<"(eq 'a' 'a')"> = [`prim`, true]
const maintest2_eq: Cion.RawLisp<"(let [a 'a'] (eq a 'a'))"> = [`prim`, true]
const maintest3_eq: Cion.RawLisp<"(let [a 'b'] (eq a 'a'))"> = [`prim`, false]

const maintest0a_eq: Cion.RawLisp<"(eq 1 1)"> = [`prim`, true]
const maintest1a_eq: Cion.RawLisp<"(eq 1 2)"> = [`prim`, false]
const maintest2a_eq: Cion.RawLisp<"(let [a 1] (eq a 1))"> = [`prim`, true]
const maintest3a_eq: Cion.RawLisp<"(let [a 2] (eq a 1))"> = [`prim`, false]

const maintest0_eq1: Cion.RawLisp<"(= 'a' 'b')"> = [`prim`, false]
const maintest1_eq1: Cion.RawLisp<"(= 'a' 'a')"> = [`prim`, true]
const maintest2_eq1: Cion.RawLisp<"(let [a 'a'] (= a 'a'))"> = [`prim`, true]
const maintest3_eq1: Cion.RawLisp<"(let [a 'b'] (= a 'a'))"> = [`prim`, false]

const maintest0a_eq1: Cion.RawLisp<"(= 1 1)"> = [`prim`, true]
const maintest1a_eq1: Cion.RawLisp<"(= 1 2)"> = [`prim`, false]
const maintest2a_eq1: Cion.RawLisp<"(let [a 1] (= a 1))"> = [`prim`, true]
const maintest3a_eq1: Cion.RawLisp<"(let [a 2] (= a 1))"> = [`prim`, false]
