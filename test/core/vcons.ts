import type { VCons } from '../../src/index.js'
        
const testvcons0: VCons<['vec', 1, ['vec', 2, ['vec', 3]]]> = ['vec', 1, 2, 3]        
const testvcons1: VCons<['vec', 1, ['vec', 2, ['vec', 3, ['vec']]]]> = ['vec', 1, 2, 3]
const testvcons2: VCons<['vec', 1, ['vec', 2, ['vec', 3, 3]]]> = ['vec', 1, 2, 3, 3]
