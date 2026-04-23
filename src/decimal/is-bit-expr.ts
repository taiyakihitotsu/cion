/**
Validates whether a string is a valid bit-string (binary expression).
Returns `true` if the string consists only of '0' and '1', otherwise `false`.

```typescript
type Case1 = IsBitExpr<'1010'> //=> true
type Case2 = IsBitExpr<'1020'> //=> false
type Case3 = IsBitExpr<'abc'>  //=> false
type Case4 = IsBitExpr<''>     //=> false
```
*/
export type IsBitExpr<
  S extends string> =
  S extends `${infer H}${infer T}`
    ? H extends '0' | '1'
      ? T extends ''
        ? true
      : IsBitExpr<T>
    : false
  : false
