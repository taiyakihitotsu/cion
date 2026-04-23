import type { NumUnion, ReadRationalFail } from './const.js'

/**
Returns `true` if the string represents a valid integer (including negative).
Returns `false` otherwise.

```typescript
SIsInt<'0'> //=> true
SIsInt<'s'> //=> false
SIsInt<'001'> //=> true
SIsInt<'38'> //=> true
SIsInt<'-1'> //=> true
SIsInt<'3/2'> //=> false
SIsInt<'1.4'> //=> false
```
*/
export type SIsInt<S> = _SIsInt<S>
type _SIsInt<
  S
, Top extends boolean = true> =
  S extends `${infer H}${infer R}`
    ? H extends '-'
      ? Top extends true
        ? _SIsInt<R, false> extends true
          ? true
        : false
      : false
    : H extends NumUnion
      ? R extends ''
        ? true
      : _SIsInt<R, false>
    : false
  : false

/**
Parses a string into a rational representation.
Returns:
- An empty tuple `[]` (ReadRationalFail) if the input is not a valid number.
- A single-member tuple `[integer]` if the input is an integer.
- A two-member tuple `[numerator, denominator]` if the input is a fraction.

All numeric parts are validated using `SIsInt`.

```typescript
ReadRational<`3/2`> //=> ['3', '2']
ReadRational<`-3/2`>> //=> ['-3', '2']
ReadRational<`-3`>> //=> ['-3']
ReadRational<`str`>> //=> []
ReadRational<`2str`>> //=> []
ReadRational<`3  /2`>> //=> []
```
*/
export type ReadRational<
  S extends string
, Ret extends string[] = []
, SS extends string = ''> =
  S extends ''
    ? SS extends ''
      ? ReadRationalFail
    : SIsInt<SS> extends true
      ? [...Ret, SS]
    : ReadRationalFail
  : S extends `${infer H}${infer R}`
    ? H extends NumUnion | '-'
      ? ReadRational<R, Ret, `${SS}${H}`>
    : H extends '/'
      ? SIsInt<SS> extends true
        ? ReadRational<R, [...Ret, SS], ''>
      : ReadRationalFail
    : ReadRationalFail
  : never
