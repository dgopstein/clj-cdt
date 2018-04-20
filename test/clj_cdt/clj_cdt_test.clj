(ns clj-cdt.clj-cdt-test
  (:require [clojure.test :refer :all]
            [clj-cdt.writer-util :refer :all]
            [clj-cdt.clj-cdt :refer :all]
            )
  (:import
           [org.eclipse.cdt.core.dom.ast IASTExpression IASTLiteralExpression IASTDoStatement]))

(deftest cdt-util-test
    (testing "count-nodes"
      (is (= 3 (count-nodes (parse-expr "1 + 2"))))
      (is (= 6 (count-nodes (parse-expr "f(1 + 2)"))))
      )

    (testing "node-name"
      (is (= "argc"
             (->>
              "int main(int argc, char **argv) {argc = 2;}"
              parse-source
              (get-in-tree [0 2 0 0 0 0])
              node-name)
             )))

  ;(deftest parse-includes
  ;  (testing "translation-unit"
  ;    (->> "include_parser.c" parse-resource (get-in-tree [1 2 0 0 2]) .getExpressionType str (= "int") is)
  ;    ))

  (deftest parse-expr-stmt-test
    (testing "parse-expr/parse-stmt"
      (let [sanitize #(clojure.string/replace % #"\s+" " ")]
        (is (= "1 + 3" (sanitize (write-ast (parse-expr "1 + 3")))))
        (is (= "1 + 3; " (sanitize (write-ast (parse-stmt "1 + 3;")))))
        (is (= "{ 1 + 3; f(); } " (sanitize (write-ast (parse-stmt "{1 + 3; f();}")))))
        ))

    (testing "stmt-str?"
      (is (false? (stmt-str? "1 + 3")))
      (is (true? (stmt-str? "1 + 3;")))
      (is (true? (stmt-str? "{ 1 + 3; f(); } ")))
      )
    )

  (deftest parse-frag-test
    (testing "parse-expr"
      (let [sanitize #(clojure.string/replace % #"\s+" " ")
            cmp-frag (fn [parser code] (is (= (write-ast (parser code)) (write-ast (parse-frag code)))))]
        (cmp-frag parse-expr "1 + 3")
        (cmp-frag parse-stmt "1 + 3;")
        (cmp-frag parse-stmt "{ 1 + 3; f(); } ")


        (is (instance? IASTExpression (parse-frag "f(x)")))
        (is (= (class (get-in-tree [0] (parse-stmt "int x = 1;"))) (class (parse-frag "int x = 1"))))

        ;; perhaps this example should parse as do-while, but alternatively,
        ;; without the semicolon, it's NOT a statement, so it shouldn't be
        ;; parsed as one
        (->> "do {} while(1)" parse-frag (instance? IASTDoStatement) is)
      )))

  (testing "height"
    (is (= 3 (->> "1 + 2;" parse-frag height)))
    (is (= 2 (->> "1 + 2" parse-frag height))))

  (testing "depth"
    (let [root (parse-source "int main() { 1 + 2; }")]
      (is (= 1 (->> root depth)))
      (is (= 1 (->> root safe-parent depth))) ; there is no parent
      (is (= 2 (->> root children first depth)))
      (is (= 3 (->> root children first children first depth)))
      (is (= 6 (->> root (get-in-tree [0 2 0 0 0]) depth)))
      ))
  )

(deftest filter-tree-test
  (testing "filter-tree"
    (is
     (= ["1" "2" "3"]
        (->> "1 + 2 - 3" parse-expr
             (filter-tree (partial instance? IASTLiteralExpression))
             (map write-ast))))
    ))

(deftest writer-util-test
  (testing "write-node"
    (let [cases [
                 ["+" "a + b"]
                 [";" "a + b;"]
                 ["<IdExpression>" "a"]
                 ["b" "b" [0]]
                 ["int" "int a;" [0 0]]
                 ["()" "f(1)"]
                 ["f" "f(1)" [0 0]]
                 ["3" "3"]
                 ["*" "*c"]
                 ["++" "d++"]
                 ["?:" "1 ? 2 : 3"]
                 ["<Problem>" "~a+"]
                 ["=" "x = 1"]
                 ["=" "x = {}"] ; gcc/libstdc++-v3/testsuite/21_strings/basic_string_view/operations/find/wchar_t/2.cc:149
                 ]]

      (doseq [[expected frag idx] cases]
        (is (= expected (->> frag parse-frag ((if idx (partial get-in-tree idx) identity)) write-node)) (prn-str [expected frag])))
      )

      (is (= "x=y+z" (->> "#define x y+z" parse-source all-preprocessor first write-ast)))
    )
  )
