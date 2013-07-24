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

(deftest ordered?-test
  (are [a b out] (= out (needle a b {:ordered? false}))

       [7 8 9] [9 8 7] nil

       {:a ["foo" "twang" "wazzles"]}
       {:a '("twang" "wazzles" "foo")}
       nil

       {:a ["foo" "twang" "wazzles"]}
       {:a '("twang" "wazzles" "foo" "hurrah")}
       {:reason :different-element
        :key :a
        :diff {:reason :missing-value
               :identifier "hurrah"
               :arg-position :first}})
  (are [a b out] (= out (needle a b {:ordered? false
                                     :identifier count}))
       ;; here "zaz" and "wow" will be identified as supposed to be the
       ;; same, and their difference will be reported
       ["halright" "zaz" "wack"]
       ["halright" "wack" "wow"]
       {:reason :different-element
        :identifier 3
        :diff {:reason :different-values
               :values ["zaz" "wow"]}}))

(deftest match-missing-key-with-nil?-test
  (are [a b out] (= out (needle a b {:match-missing-key-with-nil? true}))
       {:foo 99, :bar nil} {:foo 99} nil

       {:baz {:bar nil}} {:baz {}} nil))

(deftest metadata-opts-test
  (let [opts {:haystack {:match-missing-key-with-nil? true}}]
    (are [a b out] (= out (needle a b {}))
         {:baz {:bar nil}} {:baz {}} {:reason :different-element
                                      :key :baz
                                      :diff {:reason :missing-key
                                             :key :bar
                                             :arg-position :second}}

         {:baz (with-meta {:bar nil} opts)}
         {:baz {}}
         nil

         {:baz {:bar nil}}
         {:baz (with-meta {} opts)}
         nil

         (with-meta {:baz {:bar nil}} opts)
         {:baz {}}
         nil)))
