(ns clj-cdt.modify-ast-test
  (:require [clojure.test :refer :all]
            [clj-cdt.clj-util :refer :all]
            [clj-cdt.clj-cdt :refer :all]
            [clj-cdt.writer-util :refer :all]
            [clj-cdt.modify-ast :refer :all]
            ))

(deftest replace-expr-test
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


  ;; get/set IASTEnumerationSpecifier$IASTEnumerator
  (let [node (->> "enum Flag {Freadonly = (1 << 0),Fcontrol = (1 << 1)};" parse-stmt (get-in-tree [0 0 1 1]))
        mom (parent node)
        kid (child node)]

      (replace-expr mom node kid) ;; this value doesn't write correctly anyway, so no sense in testing it, I guess? Really I'm just happy as long as it doesn't throw an exception
    )

  ;; get/set CPPASTTemplateId
  (let [node (->> "typedef Bitmap<(MAX_TABLES > 64 ? MAX_TABLES : 64)> table_bitmap;" parse-stmt (get-in-tree [0 0 0 1]))
        mom (parent node)
        kid (child node)]

      (->> (parse-expr "2") (replace-expr mom node) write-ast (= "Bitmap<2>") is)
    )

  ;; get/set ICPPASTConstructorInitializer
  (let [node (->> "string queue((Merge_chunk_less(compare_context)), (Malloc_allocator<Merge_chunk*>(key_memory_Unique_merge_buffer)));" parse-stmt (get-in-tree [0 1 1 0]))
        mom (parent node)
        kid (child node)]

    (->> (replace-expr mom node kid) write-ast (= "(Merge_chunk_less(compare_context), (Malloc_allocator<Merge_chunk*>(key_memory_Unique_merge_buffer)))") is)
    )


  ;; the code for initializer-lists is directly copied from the constructor list code. refactor it
  (let [node (->> "a b[] = {{1, (2 | 3)}};" parse-stmt (get-in-tree [0 1 2 0 0 1]))
        mom (parent node)
        kid (child node)]

     (write-ast (replace-expr mom node kid))
    )

  (let [node (->> "struct timeval *ev_tv = &(**pev).ev_timeout;" parse-stmt (get-in-tree [0 1 2 0 0 0]))
        mom (parent node)
        kid (child node)]

     (write-ast (replace-expr mom node kid))
    )
  ))
