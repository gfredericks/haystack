(ns com.gfredericks.haystack
  (:require [clojure.set :as sets])
  (:import [clojure.lang Sequential]))

(defn ^:private index-by
  [key-fn coll]
  (into {} (for [x coll] [(key-fn x) x])))


(defn needle
  "Returns nil if a and b are equivalent, else returns a description
   of an example difference between the two."
  [a b {:keys [ordered? identifier]
        :or {ordered? true, identifier identity}
        :as opts}]
  (cond

   (and (instance? Sequential a)
        (instance? Sequential b)
        ordered?)
   (if (not= (count a) (count b))
     {:reason :different-lengths, :lengths [(count a) (count b)]}

     (if-let [[i diff] (->> (map #(needle %1 %2 opts) a b)
                            (map list (range))
                            (filter second)
                            (first))]
       {:reason :different-element, :key i, :diff diff}
       ;; nil cuz they be equal
       nil))

   (and (map? a) (map? b))
   (let [keys-a (set (keys a))
         keys-b (set (keys b))
         extra-as (sets/difference keys-a keys-b)
         extra-bs (sets/difference keys-b keys-a)]
     (cond (first extra-as)
           {:reason :missing-key
            :key (first extra-as)
            :arg-position :second}

           (first extra-bs)
           {:reason :missing-key
            :key (first extra-bs)
            :arg-position :first}

           :else
           (if-let [[k diff] (->> keys-a
                                  (map (juxt identity #(needle (get a %) (get b %) opts)))
                                  (filter second)
                                  (first))]
             {:reason :different-element, :key k, :diff diff}
             ;; nil cuz they be equal
             nil)))

   ;; The unordered collections case
   (and (coll? a) (coll? b))
   (let [a' (index-by identifier a)
         b' (index-by identifier b)
         {:keys [reason key arg-position diff]} (needle a' b' opts)]
     (case reason
       nil nil

       :missing-key
       {:reason :missing-value
        :identifier key
        :arg-position arg-position}

       :different-element
       {:reason :different-element
        :identifier key
        :diff diff}))


   (not= a b)
   (if (= (type a) (type b))
     {:reason :different-values, :values [a b]}
     {:reason :different-types, :types [(type a) (type b)]})))
