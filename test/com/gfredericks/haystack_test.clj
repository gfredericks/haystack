(ns com.gfredericks.haystack-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.haystack :refer [needle]])
  (:import [clojure.lang Keyword]))

(deftest equals-test
  (are [a b] (nil? (needle a b {}))
       12 12
       :foo :foo
       [21 "okay"] '(21 "okay")
       {:x12 [1 2 3]} {:x12 [1 2 3]}
       #{9 99 999} #{999 99 9}))


(deftest different-scalars-test
  (are [a b out] (= out (needle a b {}))
       41 42 {:reason :different-values
              :values [41 42]}

       41 "42" {:reason :different-types
                :types [Long String]}

       :foo "foo" {:reason :different-types
                   :types [Keyword String]}))

(deftest different-collections-test
  (are [a b out] (= out (needle a b {}))
       [7 8 9] [10 11] {:reason :different-lengths
                        :lengths [3 2]}

       [1 2 3 4] [1 2 42 4] {:reason :different-element
                             :key 2
                             :diff {:reason :different-values
                                    :values [3 42]}}

       {:a 7 :b 15} {:b 15} {:reason :missing-key
                             :key :a
                             :arg-position :second}

       #{9 8 2} #{2 9} {:reason :missing-value
                        :identifier 8
                        :arg-position :second}

       #{9 8 2} #{2 9 11} {:reason :missing-value
                           :identifier 8
                           :arg-position :second}))
