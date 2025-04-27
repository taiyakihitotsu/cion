import type Cion from './index'

const test_in_doc0: Cion.Lisp<`(let [a 'a' f (fn [c] (str '+' c '+'))] (if (eq '+a+' (f a)) 'this_is_true' 'this_is_false'))`> = "'this_is_true'"

const test_in_doc1: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= "in" (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x))))))`> = `[{:status 'in' :message 'message1'}]`

const test_in_doc2: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= "in" (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x)))) first))`> = `{:status 'in' :message 'message1'}`

const test_in_doc3: Cion.Lisp<`(let [c {:status 'in' :message 'message1'} cc {:status 'out' :message 'message2'} cv [c cc cc] f (fn [a b] (= "in" (b a)))] (->> cv (filter (fn [x] (= 'in' (:status x)))) first :message))`> = "'message1'"
