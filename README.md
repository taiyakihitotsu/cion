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
- [ ] logical operation: and, or, not, >, <, =, >=, <=.
- [ ] number (bit operators)
only for 2bit and lack of div in current.
- [ ] arrow macro
- [ ] string parser
- [x] Compiler

# Implemented
if, fn, let, eq, str.

# Env
- ts-node: 9.6.5
- tsc: 5.4.5

# Author
taiyakihitotsu

# License
3-clause BSD license
