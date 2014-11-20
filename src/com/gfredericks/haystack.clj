(ns com.gfredericks.haystack
  (:require [clojure.set :as sets])
  (:import [clojure.lang Sequential]))

(defn ^:private index-by
  [key-fn coll]
  (into {} (for [x coll] [(key-fn x) x])))


(defn needles
  "Returns nil if a and b are equivalent, else returns a lazy seq of
   descriptions of the differences between the two. Takes an options map
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
    (seq
     (cond

      (and (instance? Sequential a)
           (instance? Sequential b)
           ordered?)
      (let [el-diffs (for [[xa xb i] (map vector a b (range))
                           :let [diffs (needles xa xb opts)]
                           diff diffs]
                       {:reason :different-element
                        :key i
                        :diff diff})]
        (cond->> el-diffs
                 (not= (count a) (count b))
                 (cons {:reason  :different-lengths
                        :lengths [(count a) (count b)]})))

      (and (map? a) (map? b))
      (let [keys-a (set (keys a))
            keys-b (set (keys b))
            ;; TODO:  keep these as sets
            extra-as (cond->> (sets/difference keys-a keys-b)
                              match-missing-key-with-nil?
                              (remove #(nil? (get a %))))
            extra-bs (cond->> (sets/difference keys-b keys-a)
                              match-missing-key-with-nil?
                              (remove #(nil? (get b %))))
            key-diffs (for [k (sets/intersection keys-a keys-b)
                            :let [diffs (needles (get a k) (get b k) opts)]
                            diff diffs]
                        {:reason :different-element
                         :key    k
                         :diff   diff})]
        (cond-> key-diffs

                (seq extra-as)
                (conj {:reason       :missing-keys
                       :keys         extra-as
                       :arg-position :second})

                (seq extra-bs)
                (conj {:reason       :missing-keys
                       :keys         extra-bs
                       :arg-position :first})))

      ;; one's a map but the other isn't
      (or (map? a) (map? b))
      [{:reason :different-collection-types
        :types [(type a) (type b)]}]

      ;; The unordered collections case
      (and (coll? a) (coll? b))
      (let [a' (index-by identifier a)
            b' (index-by identifier b)]
        (for [{:keys [reason key keys arg-position diff]} (needles a' b' opts)
              :when reason
              :let [xs (case reason
                         :missing-keys
                         (for [k keys]
                           {:reason       :missing-value
                            :identifier   k
                            :arg-position arg-position})

                         :different-element
                         [{:reason     :different-element
                           :identifier key
                           :diff       diff}])]
              x xs]
          x))

      (not= a b)
      [(if (= (type a) (type b))
         {:reason :different-values, :values [a b]}
         {:reason :different-types, :types [(type a) (type b)]})]))))

(defn needle
  "Returns nil if a and b are equivalent, else an example difference
  as per the docstring for needles."
  [a b opts]
  (first (needles a b opts)))
