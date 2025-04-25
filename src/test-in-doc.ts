import type Lisp from './index'

const test_in_doc0: Lisp<`(let [a 'a' f (fn [c] (str '+' c '+'))] (if (eq '+a+' (f a)) 'this_is_true' 'this_is_false'))`> = ['prim', "'this_is_true'"]

const test_in_doc1: Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= "in" (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x))))))`> = ['vec', ['map', [['key', ':status'], ['prim', "'in'"], ['key', ':message'], ['prim', "'message1'"]]]]

const test_in_doc2: Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= "in" (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x)))) first))`> = ['map', [['key', ':status'], ['prim', "'in'"], ['key', ':message'], ['prim', "'message1'"]]]

const test_in_doc3: Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= "in" (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x)))) first :message))`> = ['prim', "'message1'"]
