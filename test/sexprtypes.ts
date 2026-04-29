/**
[Todo] 2026/04/29

Currently `Sexpr` means an executable S-expression such as `((fn [a] (inc a)) 1)`, not including primitives/atoms.
*/
import type Cion from '../src/index.js'
import type { Atom, TMap, Fn, Sexpr, Vector, LetForm, Prim } from "../src/sexprtypes.js"
import type { Equal, DistributeExtends } from '../src/util.js'

// ----------------------------
// -- Prim
// ----------------------------

type Actual_prim_num_0 = Cion.CionCompiler<"20">
type Expected_prim_num_extends = DistributeExtends<[Actual_prim_num_0], [Atom] | [Prim]>
type Expected_prim_num_not_extends = DistributeExtends<[Actual_prim_num_0], [Sexpr] | [Fn] | [TMap] | [Vector], false>
const expected_prim_num_ok: true = {} as Equal<true, Expected_prim_num_extends>
const expected_prim_num_err: true = {} as Equal<true, Expected_prim_num_not_extends>

type Actual_prim_ratio_0 = Cion.CionCompiler<"20/3">
type Expected_prim_ratio_extends = DistributeExtends<[Actual_prim_ratio_0], [Atom] | [Prim]>
type Expected_prim_ratio_not_extends = DistributeExtends<[Actual_prim_ratio_0], [Sexpr] | [Fn] | [TMap] | [Vector], false>
const expected_prim_ratio_ok: true = {} as Equal<true, Expected_prim_ratio_extends>
const expected_prim_ratio_err: true = {} as Equal<true, Expected_prim_ratio_not_extends>

type Actual_prim_bool_0 = Cion.CionCompiler<"true">
type Expected_prim_bool_extends = DistributeExtends<[Actual_prim_bool_0], [Atom] | [Prim]>
type Expected_prim_bool_not_extends = DistributeExtends<[Actual_prim_bool_0], [Sexpr] | [Fn] | [TMap] | [Vector], false>
const expected_prim_bool_ok: true = {} as Equal<true, Expected_prim_bool_extends>
const expected_prim_bool_err: true = {} as Equal<true, Expected_prim_bool_not_extends>

type Actual_prim_str_0 = Cion.CionCompiler<"'20'">
type Expected_prim_str_extends = DistributeExtends<[Actual_prim_str_0], [Atom] | [Prim]>
type Expected_prim_str_not_extends = DistributeExtends<[Actual_prim_str_0], [Sexpr] | [Fn] | [TMap] | [Vector], false>
const expected_prim_str_ok: true = {} as Equal<true, Expected_prim_str_extends>
const expected_prim_str_err: true = {} as Equal<true, Expected_prim_str_not_extends>

// ----------------------------
// -- Vec
// ----------------------------

type Actual_vec_0 = Cion.CionCompiler<"[10 20]">
type Expected_vec_extends = DistributeExtends<[Actual_vec_0], [Atom] | [Vector]>
type Expected_vec_not_extends = DistributeExtends<[Actual_vec_0], [Sexpr] | [Fn] | [TMap], false>
const expected_vec_ok: true = {} as Equal<true, Expected_vec_extends>
const expected_vec_err: true = {} as Equal<true, Expected_vec_not_extends>

type Actual_empty_vec_0 = Cion.CionCompiler<"[]">
type Expected_empty_vec_extends = DistributeExtends<[Actual_empty_vec_0], [Atom] | [Vector]>
type Expected_empty_vec_not_extends = DistributeExtends<[Actual_empty_vec_0], [Sexpr] | [Fn] | [TMap], false>
const expected_empty_vec_ok: true = {} as Equal<true, Expected_empty_vec_extends>
const expected_empty_vec_err: true = {} as Equal<true, Expected_empty_vec_not_extends>

// ----------------------------
// -- Map
// ----------------------------

type Actual_map_0 = Cion.CionCompiler<"{:a 10 :b 20}">
type Expected_map_extends = DistributeExtends<[Actual_map_0], [Atom] | [TMap]>
type Expected_map_not_extends = DistributeExtends<[Actual_map_0], [Sexpr] | [Fn], false>
const expected_map_ok: true = {} as Equal<true, Expected_map_extends>
const expected_map_err: true = {} as Equal<true, Expected_map_not_extends>

type Actual_empty_map_0 = Cion.CionCompiler<"{}">
type Expected_empty_map_extends = DistributeExtends<[Actual_empty_map_0], [Atom] | [TMap]>
type Expected_empty_map_not_extends = DistributeExtends<[Actual_empty_map_0], [Sexpr] | [Fn], false>
const expected_empty_map_ok: true = {} as Equal<true, Expected_empty_map_extends>
const expected_empty_map_err: true = {} as Equal<true, Expected_empty_map_not_extends>

// ----------------------------
// -- Fn returns Map
// ----------------------------

type Actual_fn_returns_map_0 = Cion.CionCompiler<"(fn [] {:res (+ 10 20)})">
type Expected_fn_rmap_extends = DistributeExtends<[Actual_fn_returns_map_0], [Atom] | [Fn]>
type Expected_fn_rmap_not_extends = DistributeExtends<[Actual_fn_returns_map_0], [Sexpr] | [TMap], false>
const expected_fn_rmap_ok: true = {} as Equal<true, Expected_fn_rmap_extends>
const expected_fn_rmap_err: true = {} as Equal<true, Expected_fn_rmap_not_extends>

// ----------------------------
// -- Fn returns Vec
// ----------------------------

type Actual_fn_returns_vec_0 = Cion.CionCompiler<"(fn [x] [x])">
type Expected_fn_returns_vec_extends = DistributeExtends<[Actual_fn_returns_vec_0], [Atom] | [Fn]>
type Expected_fn_returns_vec_not_extends = DistributeExtends<[Actual_fn_returns_vec_0], [Sexpr] | [TMap], false>
const expected_fn_returns_vec_ok: true = {} as Equal<true, Expected_fn_returns_vec_extends>
const expected_fn_returns_vec_err: true = {} as Equal<true, Expected_fn_returns_vec_not_extends>

// ----------------------------
// -- Let returns Map
// ----------------------------

type Actual_let_returns_map_0 = Cion.CionCompiler<"(let [a 0] {:res (+ 10 20)})">
type Expected_let_rmap_extends = DistributeExtends<[Actual_let_returns_map_0], [LetForm]>
type Expected_let_rmap_not_extends = DistributeExtends<[Actual_let_returns_map_0], [Sexpr] | [TMap] | [Vector] | [Atom] | [Prim] | [Fn], false>
const expected_let_rmap_ok: true = {} as Equal<true, Expected_let_rmap_extends>
const expected_let_rmap_err: true = {} as Equal<true, Expected_let_rmap_not_extends>

// ----------------------------
// -- Let returns Vec
// ----------------------------

type Actual_let_returns_vec_0 = Cion.CionCompiler<"(let [a 0] [a])">
type Expected_let_returns_vec_extends = DistributeExtends<[Actual_let_returns_vec_0], [LetForm]>
type Expected_let_returns_vec_not_extends = DistributeExtends<[Actual_let_returns_vec_0], [Sexpr] | [TMap] | [Vector] | [Atom] | [Prim] | [Fn], false>
const expected_let_returns_vec_ok: true = {} as Equal<true, Expected_let_returns_vec_extends>
const expected_let_returns_vec_err: true = {} as Equal<true, Expected_let_returns_vec_not_extends>
