export type BitShiftLeftOne<
  B extends string> =
  B extends `${infer H}`
    ? `${H}0` extends `${infer _}${infer D}`
      ? D
    : never
  : never
