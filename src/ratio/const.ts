// -----------
// -- util
// -----------
export type IntBitString = string // [todo]
export type Nat = IntBitString // [todo]
export type Ratio = [IntBitString, IntBitString]
export type RatioNumber = Nat | Ratio
export type DivByZero = 'nil'
export type IntZero = '0000000000000000'
export type IntOne = '0000000000000001'
export type RatioZero = [IntZero, IntOne]
export type RatioOne =  [IntOne, IntOne]
