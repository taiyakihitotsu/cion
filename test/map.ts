import type Cion from '../src/index.ts'

const maintest12_get_0: Cion.RawLisp<"(:a {:a 1})"> = ['prim', '0000000000000001']
const maintest12_get_1: Cion.RawLisp<"(:a {:a 1 :b 2})"> = ['prim', '0000000000000001']
const maintest12_get_2: Cion.RawLisp<"({:a 1 :b 2} :a)"> = ['prim', '0000000000000001']
const maintest12_get_3: Cion.RawLisp<"(:c {:a 1 :b 2})"> = []
const maintest12_get_4: Cion.RawLisp<"({:a 1 :b 2} :c)"> = []
const maintest12_get_5: Cion.RawLisp<"(get {:a 1 :b 2} :a)"> = ['prim', '0000000000000001']

const maintest0_mapst_0: Cion.RawLisp<"{:a 1}"> = ['map', [['key', ':a'], ['prim', '0000000000000001']]]
