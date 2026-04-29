import type {Equal} from '../../src/util.js'
import type {BitUniform} from '../../src/bit/index.js'

const bituniform0: true = {} as Equal<BitUniform<"1111", "00000">, ["01111", "00000"]>
const bituniform1: true = {} as Equal<BitUniform<"001111", "00000">, ["001111", "000000"]>
const bituniform2: true = {} as Equal<BitUniform<"001111", "111100">, ["001111", "111100"]>

const bituniform3: true = {} as Equal<BitUniform<'', '111100'>, ['000000', '111100']>
const bituniform4: true = {} as Equal<BitUniform<'001111', ''>, [never, '']>
