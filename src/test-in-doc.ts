import type Lisp from './index'

const test_in_doc0: Lisp<`(let [a 'a' f (fn [c] (str '+' c '+'))] (if (eq '+a+' (f a)) 'this_is_true' 'this_is_false'))`> = ['prim', "'this_is_true'"]
 
// const test_in_doc1: Lisp<`(let [c {:status "in" :message "message"} cc {:status "out" :message "message"} cv [c cc cc] f (fn [a b] (= "in" (b a)))] (->> cv (filter (fn [x] (f :status x))) first :message))`> = ''

const test_in_doc1: Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= "in" (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x))))))`> = ''
