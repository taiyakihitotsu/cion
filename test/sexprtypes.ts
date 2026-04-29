import type Cion from '../src/index.js'
import type { Atom, TMap, Fn, Sexpr } from "../src/sexprtypes.js"
import type { Equal, DistributeExtends } from '../src/util.js'

/**
[Todo] 2026/04/29

Currently `Sexpr` means an executable S-expression such as `((fn [a] (inc a)) 1)`, not including primitives/atoms.
*/

type Actual_map_0 = Cion.CionCompiler<"{:a 10 :b 20}">
type Expected_map_extends = DistributeExtends<[Actual_map_0], [Atom] | [TMap]>
type Expected_map_not_extends = DistributeExtends<[Actual_map_0], [Sexpr] | [Fn], false>
const expected_map_ok: true = {} as Equal<true, Expected_map_extends>
const expected_map_err: true = {} as Equal<true, Expected_map_not_extends>

type Actual_fn_returns_map_0 = Cion.CionCompiler<"(fn [] {:res (+ 10 20)})">
type Expected_fn_rmap_extends = DistributeExtends<[Actual_fn_returns_map_0], [Atom] | [Fn]>
type Expected_fn_rmap_not_extends = DistributeExtends<[Actual_fn_returns_map_0], [Sexpr] | [TMap], false>
const expected_fn_rmap_ok: true = {} as Equal<true, Expected_fn_rmap_extends>
const expected_fn_rmap_err: true = {} as Equal<true, Expected_fn_rmap_not_extends>

const expected_fn_returns_map_of_atom: true = {} as Equal<true, Actual_fn_returns_map_0 extends Atom ? true : false>
const expected_fn_returns_map_of_fn: true = {} as Equal<true, Actual_fn_returns_map_0 extends Fn ? true : false>
const expected_fn_returns_map_of_sexpr: true = {} as Equal<false, Actual_fn_returns_map_0 extends Sexpr ? true : false>
const expected_fn_returns_map_of_tmap: true = {} as Equal<false, Actual_fn_returns_map_0 extends TMap ? true : false>

type Actual_fn_returns_vec_0 = Cion.CionCompiler<"(fn [x] [x])">
const expected_fn_returns_vec_of_atom: true = {} as Equal<true, Actual_fn_returns_vec_0 extends Atom ? true : false>
const expected_fn_returns_vec_of_fn: true = {} as Equal<true, Actual_fn_returns_vec_0 extends Fn ? true : false>
const expected_fn_returns_vec_of_sexpr: true = {} as Equal<false, Actual_fn_returns_vec_0 extends Sexpr ? true : false>
const expected_fn_returns_vec_of_tmap: true = {} as Equal<false, Actual_fn_returns_vec_0 extends TMap ? true : false>
