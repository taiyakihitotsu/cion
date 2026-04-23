export type BitNot<
  B extends string> =
  B extends `0`
    ? `1`
  : B extends `1`
    ? `0`
  : B extends `${infer H}${infer T}`
    ? `${BitNot<H>}${BitNot<T>}`
  : never
