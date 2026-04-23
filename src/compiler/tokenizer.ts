import type {Rec} from '../util.js'

export type SPad<S extends string> = S extends ` ${infer SS}` ? SS  : ` ${S}`

/**
Extracts a quoted string literal and returns a tuple of [captured_string, remaining_text].
*/
export type ReadString<
  S extends string
, ident extends `"` | `'`> =
  S extends `${ident}${infer P}${ident}${infer R}`
    ? [`${ident}${P}${ident}`, ` ${R}`]
  : never

/**
Tokenizes an S-expression string into an array of string.

NOTE: All input strings must have a single space prefix to ensure consistent pattern matching in `recp.`
Use `SPad` to normanize the input before passing it to this type.

```typescript
Compiler.SParser<` (let [a "test is this"] (str "a b" a))`>
//=> ['(', 'let', '[', 'a', '"test is this"', ']', '(', 'str', '"a b"', 'a', ')', ')']
```
*/
export type recp<
  Sexpr
, R extends string[] = []> =
  Sexpr extends `  ${infer U}` | ` \n${infer U}`
    ? { r: recp<` ${U}`, R> }
  : Sexpr extends ` (${infer U}`
    ? { r: recp<` ${U}`, [...R, '(']> }
  : Sexpr extends ` {${infer U}`
    ? { r: recp<` ${U}`, [...R, '{']> }
  : Sexpr extends ` [${infer U}`
    ? { r: recp<` ${U}`, [...R, '[']> }
  : Sexpr extends ` "${infer U}`
    ? { r: recp<ReadString<`"${U}`, `"`>[1], [...R, ReadString<`"${U}`, `"`>[0]]> }
  : Sexpr extends ` '${infer U}`
    ? { r: recp<ReadString<`'${U}`, `'`>[1], [...R, ReadString<`'${U}`, `'`>[0]]> }
  : Sexpr extends ` ${infer fU} ${infer Next}`
    ? fU extends `${infer ffU}}`
      ? ffU extends ''
        ? { r: recp<` ${Next}`, [...R, '}']> }
      : { r: recp<` ${ffU} } ${Next}`, R> }
    : fU extends `${infer ffU}]`
      ? ffU extends ''
        ? { r: recp<` ${Next}`, [...R, ']']> }
      : { r: recp<` ${ffU} ] ${Next}`, R> }
    : fU extends `${infer ffU})`
      ? ffU extends ''
        ? { r: recp<` ${Next}`, [...R, ')']> }
      : { r: recp<` ${ffU} ) ${Next}`, R> }
    : fU extends `${infer ffU}"`
      ? { r: recp<` ${Next}`, [...R, ffU, '"']> }
    : { r: recp<` ${Next}`, [...R, fU]> }
  : Sexpr extends ` ${infer U})`
    ? { r: recp<` ${U} ) `, R> }
  : Sexpr extends ` ${infer U}}`
    ? { r: recp<` ${U} } `, R> }
  : Sexpr extends ` ${infer U}]`
    ? { r: recp<` ${U} ] `, R> }
  : Sexpr extends ` ${infer U}`
    ? { r: [...R, ...(U extends '' ? [] : [U])] }
  : { r: R }

export type Tokenizer<Sexpr extends string> = Rec<recp<SPad<Sexpr>>>
