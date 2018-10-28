(ns clj-cdt.clj-util)

(defn tap [f x] (f x) x)
(defn pap
  "print the value of the argument and return it; optionally modified by a function"
  ([x] (tap prn x))
  ([f x] (tap (comp prn f) x)))

(defn =by
  "Test if arguments are equal after applying f to all of them"
  [f & args]
  (apply = (map f args)))

; print the name and value of an expression
(defmacro pprn [x]
  `(let [y# ~x]
    (do
      (print (str ~(str x ": ") (prn-str y#)))
      y#)))

(defn arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))

;; http://stackoverflow.com/questions/23178750/iteratively-apply-function-to-its-result-without-generating-a-seq
(defn fn-pow
  [f x n]
    (nth (iterate f x) n))

; https://stackoverflow.com/questions/11676120/why-dont-when-let-and-if-let-support-multiple-bindings-by-default/36160972#36160972
(defmacro if-let*
  ([bindings then]
   `(if-let* ~bindings ~then nil))
  ([bindings then else]
   (if (seq bindings)
     `(if-let [~(first bindings) ~(second bindings)]
        (if-let* ~(drop 2 bindings) ~then ~else)
        ~else)
          then)))

; https://github.com/clojure/clojure/blob/clojure-1.9.0-alpha14/src/clj/clojure/core.clj#L3836
(defmacro time-mins
  "Evaluates expr and prints the time it took.  Returns the value of expr."
  [expr]
  `(let [start# (. System (nanoTime))
         ret#   ~expr
         end#   (. System (nanoTime))
         diff#  (- end# start#)
         mins-raw#   (/ (double diff#) (* 60 1000 1000000.0))
         mins#  (int mins-raw#)
         secs#  (* 60 (- mins-raw# mins#))]
     (prn (format "Elapse time: %d:%05.2f mins" mins# secs#))
          ret#))
