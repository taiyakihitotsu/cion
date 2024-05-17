# RoadMap

## Goal
```typescript
type Sexpr =
`(let [c  {:status  "in"
           :message "message"}
       cc {:status  "out"
           :message "message"}
       cv [c cc cc]
       f (fn [a b] (if (= "in" (b a)) true false))]
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
- [ ] vector (= list / array here)
- [ ] hashmap
- [x] if
- [ ] eq
- [ ] filter
- [ ] get
- [ ] get with a key
- [ ] first
- [ ] arrow macro


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






