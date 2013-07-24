# haystack

A Clojure library for debugging differences between large datasets
that are supposed to be equal.

## Obtainage

Via Leiningen: `[com.gfredericks/haystack "0.2.0"]`

## Usage

``` clojure
(require '[com.gfredericks.haystack :refer [needle]])

(def v (vec (range 100000)))
(def v' (assoc v 3878 :foo))

(needle v v' {})
=> {:reason :different-element,
    :key 3878,
    :diff {:reason :different-types,
           :types [java.lang.Long clojure.lang.Keyword]}}
```

## TODO

- Document options better
- Make return values less confusing
- Sometimes reporting type differences is misleading?

## License

Copyright © 2013 Gary Fredericks

Distributed under the Eclipse Public License, the same as Clojure.
