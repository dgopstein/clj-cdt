(ns clj-cdt.collection-util)

(def any-pred? (comp boolean some))
(defn exists?
  ([lst] (any-pred? true? lst))
  ([pred lst] (any-pred? pred lst)))

(defn map-kv [f m]
  (->> m (map (fn [[k v]] (f k v))) (into {})))

(defn map-values-kv [f m]
  (map-kv (fn [k v] {k (f k v)}) m))

(defn map-values [f m] (map-values-kv #(f %2) m))

(defn map-keys [f m] (map-kv (fn [k v] [(f k) v]) m))

(defn find-after
  "Take the element after the specified one"
  [coll elem]
  (->> (map vector coll (rest coll))
       (filter #(= elem (first %)))
       first
       last))

; https://github.com/mikera/clojure-utils/blob/master/src/main/clojure/mikera/cljutils/loops.clj
(defmacro doseq-indexed
    "loops over a set of values, binding index-sym to the 0-based index of each value"
  ([[val-sym values index-sym] & code]
   `(loop [vals# (seq ~values)
           ~index-sym (long 0)]
      (if vals#
        (let [~val-sym (first vals#)]
          ~@code
          (recur (next vals#) (inc ~index-sym)))
               nil))))
