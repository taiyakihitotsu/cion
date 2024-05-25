# RoadMap

## Goal
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
- [ ] fn >1 args
- [x] vector (= list / array here)
- [x] hashmap
- [x] if
- [x] eq
- [x] map
- [x] filter
- [x] get
- [x] get with a key
- [x] first
- [ ] number
- [ ] arrow macro
- [ ] Compiler


# Run
```bash
$ npm run build:watch
```

# Env
- ts-node: 9.6.5
- tsc: 5.4.5

# Author
https://github.com/taiyakihitotsu/

# License
3-clause BSD license






