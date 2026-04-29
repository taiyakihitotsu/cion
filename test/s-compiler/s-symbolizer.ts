import type { SSymbolizer } from "../../src/s-compiler/s-symbolizer.js"
import type { Equal } from "../../src/util.js"

type Expected_true = SSymbolizer<'true'>
const expected_true: true = {} as Equal<Expected_true, ['prim', true]>

type Expected_false = SSymbolizer<'false'>
const expected_false: true = {} as Equal<Expected_false, ['prim', false]>

type Expected_if = SSymbolizer<'if'>
const expected_if: true = {} as Equal<Expected_if, 'if'>

type Expected_let = SSymbolizer<'let'>
const expected_let: true = {} as Equal<Expected_let, 'let'>

type Expected_fn = SSymbolizer<'fn'>
const expected_fn: true = {} as Equal<Expected_fn, 'fn'>

type Expected_nil = SSymbolizer<'nil'>
const expected_nil: true = {} as Equal<Expected_nil, ['prim', 'nil']>

type Expected_empty_str = SSymbolizer<"''">
const expected_empty_str: true = {} as Equal<Expected_empty_str, ['prim', "''"]>

type Expected_some_str = SSymbolizer<"'some'">
const expected_some_str: true = {} as Equal<Expected_some_str, ['prim', "'some'"]>

type Expected_rational = SSymbolizer<"3/2">
const expected_rational: true = {} as Equal<Expected_rational, ['prim', ["0000000000000011", "0000000000000010"]]>

type Expected_rational_neg = SSymbolizer<"-3/2">
const expected_rational_neg: true = {} as Equal<Expected_rational_neg, ['prim', ["1111111111111101", "0000000000000010"]]>

type Expected_int = SSymbolizer<"3">
const expected_int: true = {} as Equal<Expected_int, ['prim', "0000000000000011"]>

type Expected_int_neg = SSymbolizer<"-3">
const expected_int_neg: true = {} as Equal<Expected_int_neg, ['prim', "1111111111111101"]>

type Expected_int_zero = SSymbolizer<"0">
const expected_int_zero: true = {} as Equal<Expected_int_zero, ['prim', "0000000000000000"]>
