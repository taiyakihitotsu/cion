export type PeanoToDecimal<
  N> =
  N extends null
    ? 0
  : N extends [null]
    ? 1
  : N extends [[null]]
    ? 2
  : N extends [[[null]]]
    ? 3
  : N extends [[[[null]]]]
    ? 4
  : N extends [[[[[null]]]]]
    ? 5
  : N extends [[[[[[null]]]]]]
    ? 6
  : N extends [[[[[[[null]]]]]]]
    ? 7
  : N extends [[[[[[[[null]]]]]]]]
    ? 8
  : N extends [[[[[[[[[null]]]]]]]]]
    ? 9
  : never
