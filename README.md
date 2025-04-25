# RoadMap
## Goal
```typescript
(let [a 'a'
      f (fn [c] (str '+' c '+'))]
  (if (eq '+a+' (f a)) 'this_is_true' 'this_is_false'))
```

```typescript
type Sexpr =
`(let [c  {:status  "in"
           :message "message"}
       cc {:status  "out"
           :message "message"}
       cv [c cc cc]
       f (fn [a b] (= "in" (b a)))]
   (->> cv
        (filter (fn [x] (f :status x)))
        first
        :message))`

const message: Return<Eval<Compile<Sexpr>>> = "message"
```

## Done & Todo
- [x] let
- [x] fn
- [x] vector
- [x] hashmap
- [x] if
- [x] map, filter, remove, reduce.
- [x] getter: first, with keyword, etc.
- [x] logical operation: and, or, not, >, <, =, >=, <=.
- [x] number (bit operators)
- [x] threading macro: ->, ->>.
- [x] string parser: currently, only supporting texts which has no two or more connected-spaces such like "a  b".
- [x] Compiler

# Builtin
## Definition
They are listed in implemented of usable in Sexpr level.
## Implemented Already
if, fn, let, str, eq, and, or, not, >, <, =, >=, <=, +, -, *, /, mod, map, filter, remove, reduce, conj, concat, interleave, reverse, first, last, rest, butlast, assoc, assoc-in, update, update-in, get, vec, nil.
## Not Implemented Currently
inc, dec,
## Thinking about whether to implement or not
- about vector:
keep, second, partition, range, repeat.
- about hashmap:
dissoc, zipmap, keys, vals, zipmap, into.
## No plan to implement
- transducers such like (map f)
- lazy-seq
- R\Q
- list (because of the same role of vec in typelevel)
- loop & recur syntax (I think it's enough of map, reduce, recursive call or something)
- defmacro
- things I've forgot to list


# Env
- ts-node: 9.6.5
- tsc: 5.4.5

# Author
taiyakihitotsu

# License
3-clause BSD license
