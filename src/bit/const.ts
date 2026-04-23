import type * as Peano from "../peano.js";

// CONSTANTS.
export type MAX = Peano.T16;

export type BitZero = `${'0000'}${'0000'}${'0000'}${'0000'}`
export type BitOne =  `${'0000'}${'0000'}${'0000'}${'0001'}`

export const Pad8: '00000000'  = '00000000'
export const Pad16: '0000000000000000' = '0000000000000000'
export type CurPadType = typeof Pad8
export const CurPad: CurPadType = Pad8

export type Neg1 = `${'1111'}${'1111'}${'1111'}${'1111'}`
export type NegMin = `${'1000'}${'0000'}${'0000'}${'0000'}`
export type PosMax = `${'0111'}${'1111'}${'1111'}${'1111'}`
