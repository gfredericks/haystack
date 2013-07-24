(ns com.gfredericks.haystack
  (:require [clojure.set :as sets])
  (:import [clojure.lang Sequential]))

(defn ^:private index-by
  [key-fn coll]
  (into {} (for [x coll] [(key-fn x) x])))


(defn needle
  "Returns nil if a and b are equivalent, else returns a description
   of an example difference between the two. Takes an options map
   that can have any of the following keys:

     :ordered?
       (default: true) determines whether sequential collections will
       be compared as ordered, in which case elements at the same index
       are compared. When :ordered? is false, it may be helpful to supply
       a smart :identifier function to use for matching the elements --
       see the next option for more details.
     :identifier
       (default: identity) used to match elements in unordered collections.
       If for example you have records with an :id key, you could supply
       :id as the :identifier function, and more specific differences
       would be reported.
     :match-missing-key-with-nil?
       (default: false) when true, allows {:foo nil} to match {}.

   Additionally options can be supplied in the :haystack key on an
   object's metadata at any level, in which case it applies at that
   level and recursively below it. The metadata can be present on
   either argument."
  [a b opts]
  (let [{:keys [ordered? identifier match-missing-key-with-nil?]
        :or {ordered? true,
             identifier identity,
             match-missing-key-with-nil? false}
        :as opts}
        ;; Currently metadata options apply recursively as well, though
        ;; that might not always be wanted.
        (merge opts (-> a meta :haystack) (-> b meta :haystack))]
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
           extra-as (cond->> (sets/difference keys-a keys-b)
                             match-missing-key-with-nil?
                             (remove #(nil? (get a %))))
           extra-bs (cond->> (sets/difference keys-b keys-a)
                             match-missing-key-with-nil?
                             (remove #(nil? (get b %))))]
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

     ;; one's a map but the other isn't
     (or (map? a) (map? b))
     {:reason :different-collection-types
      :types [(type a) (type b)]}

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
       {:reason :different-types, :types [(type a) (type b)]}))))
