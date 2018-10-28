(ns clj-cdt.modify-ast-test
  (:require [clojure.test :refer :all]
            [clj-cdt.clj-cdt :refer :all]
            [clj-cdt.writer-util :refer :all]
            [clj-cdt.modify-ast :refer :all]
            ))

(deftest replace-node-test
  (testing "Duplicate the arguments of an arithmetic expression"
    (let [expr (parse-expr "1 + 2 * 3")]
      (is (= "2 * 3 + 2 * 3"
             (write-tree
              (replace-expr expr (first (children expr))
                                 (second (children expr))))))

      (is (= "1 + 1"
             (write-tree
              (replace-expr expr (second (children expr))
                                 (first (children expr))))))

      )

    (let [expr (parse-expr "f(1, 2 * 3)")]
      (is (= "f(2 * 3, 2 * 3)"
             (write-tree
              (replace-expr expr (nth (children expr) 1)
                                 (nth (children expr) 2)))))

      (is (= "f(1, 1)"
             (write-tree
              (replace-expr expr (nth (children expr) 2)
                                 (nth (children expr) 1)))))

      )


  (let [expr (parse-expr "1, 2 * 3")]
      (is (= "2 * 3, 2 * 3"
             (write-tree
              (replace-expr expr (nth (children expr) 0)
                                 (nth (children expr) 1)))))

      (is (= "1, 1"
             (write-tree
              (replace-expr expr (nth (children expr) 1)
                                 (nth (children expr) 0)))))

      )

  ;; Make sure we can accept statements as parents
  (let [stmt (parse-stmt "(2 * 3);")]

    (is (= "(2 * 3);\n"
           (write-tree
            (replace-expr stmt (nth (children stmt) 0)
                               (nth (children stmt) 0)))))

    )

  ;; make sure we can set function name expressions
  (let [node (->> "len = (size_t)(temp - txt + 1)" parse-expr (get-in-tree [1 0]))
        mom (parent node)
        kid (child node)]

    (is (= "size_t(temp - txt + 1)" (write-ast (replace-expr mom node kid))))
    )
  ))
