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
- [x] let >1 args
- [x] fn
- [x] fn >1 args
- [x] vector (= list / array here)
- [ ] hashmap
- [x] if
- [x] eq
- [x] map
- [x] filter
- [x] get
- [x] get with a key
- [x] first
- [x] logical operation: and, or, not, >, <, =, >=, <=.
- [x] number (bit operators)
only for 2bit and lack of div in current.
- [ ] arrow macro
- [ ] string parser
- [x] Compiler

# Builtin
## Definition
They are listed in implemented of usable in Sexpr level.
Some of them are usable only in AST though, not should it be called as implemented.
## Implemented Already
if, fn, let, eq, str.
## Not Implemented Currently
and, or, not, >, <, =, >=, <=, inc, dec, +, -, *, /, mod, map, filter, remove, vec, nil.
## Thinking about whether to implement or not
- about vector:
keep, set, conj, concat, first, second, get, rest, butlast, partition, range, repeat, reduce.
- about hashmap:
assoc, dissoc, update, hash-map, zipmap, keys, vals, zipmap, into.
## No plan to implement
- transducers such like (map f)
- lazy-seq
- R\Q
- list (because of the same role of vec in typelevel)
- loop, recur (I think it's enough of map or something)
- macro
- things I've forgot to list


# Env
- ts-node: 9.6.5
- tsc: 5.4.5

# Author
taiyakihitotsu

# License
3-clause BSD license
