// This processes every two heads chars of both arguments at one call.
// note that one char doesn't extends including 3 infers template literal type.
// 
// Return `false` if one is `''`
// Return `true` if X longer than Y.
// -------------------------------------------------
// BitLenGthan<"0000", "0000"> // false
// BitLenGthan<"000", "0001"> // false
// BitLenGthan<"0001", "000"> // true
// -------------------------------------------------
export type BitLenGthan<X extends string, Y extends string> = '' extends X | Y ? false : _BitLenGthan<X, Y>

export type _BitLenGthan<
  X extends string
, Y extends string> =
  X extends `${infer _}${infer _}${infer char2}`
    ? Y extends `${infer _}${infer _}${infer char2y}`
      ? char2y extends ''
        ? char2 extends ''
          ? false
        : true
      : char2 extends ''
        ? false
      : _BitLenGthan<char2, char2y>
    : true
  : Y extends ''
    ? true
  : false
