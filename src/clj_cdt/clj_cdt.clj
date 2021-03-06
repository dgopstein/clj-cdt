(ns clj-cdt.clj-cdt
  (:require [clojure.string :as str]
            [schema.core :as s]
            [swiss.arrows :refer :all]
            [clj-cdt.clj-util :refer :all]
            [clj-cdt.collection-util :refer :all]
            )
  (:import [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage gnu.c.GCCLanguage]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent
            IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.core.dom.ast IASTNode IASTTranslationUnit
            IASTExpression IASTBinaryExpression IASTLiteralExpression
            IASTDoStatement IASTCompositeTypeSpecifier IASTProblemStatement
            IASTName IASTIdExpression IASTDeclarator IASTFunctionDefinition]
           [org.eclipse.cdt.core.dom.ast.cpp ICPPASTNamespaceDefinition]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTProblemStatement] ;TODO remove
           [org.eclipse.cdt.internal.core.parser.scanner ASTFileLocation]
           [org.eclipse.cdt.core.model ILanguage])) ;TODO remove

(s/defn translation-unit
  [file-content :- FileContent &
   [{:keys [language include-dirs resolve-includes]
     :or {language :c++ include-dirs [] resolve-includes false}}]]
  (let [definedSymbols {}
        includePaths (into-array String include-dirs)
        info (new ScannerInfo definedSymbols includePaths)
        log (new DefaultLogService)
        include-resolver (if (and (not resolve-includes)
                                  (empty? include-dirs))
                           (IncludeFileContentProvider/getEmptyFilesProvider)
                           (clj_cdt.FileCodeReaderFactory/getInstance))
        ilanguage (case language
                 ;; :assembly (org.eclipse.cdt.core.model.AssemblyLanguage/getDefault)
                    :c   (GCCLanguage/getDefault)
                    :c++ (GPPLanguage/getDefault))
        flags (bit-or 0 ILanguage/OPTION_PARSE_INACTIVE_CODE)]
    (.getASTTranslationUnit ilanguage file-content info include-resolver
     org.eclipse.cdt.internal.core.index.EmptyCIndex/INSTANCE flags log)))

(defn parse-source
  "Create an AST from in-memory source (filename is for documentation only)"
  [source & [{:as opts :keys [filename directory]
              :or {filename "anonymously-parsed-code.c"
                   directory nil}}]]
  (let [filepath (if directory
                   (.getPath (clojure.java.io/file directory filename))
                   filename)]
    (translation-unit (FileContent/create filepath (.toCharArray source)) opts)))

(defmulti parse-file (fn [arg & [opts]] (class arg)))
(defmethod parse-file java.io.File [file & [opts]]
  (parse-file (.getPath file) opts))
(defmethod parse-file String [filename & [opts]]
  (when-let [file-content (FileContent/createForExternalFileLocation filename)]
    (translation-unit file-content opts)))

(defmulti children class)
(defmethod children IASTNode [node] (.getChildren node))

(defn parent [node] (.getParent node))
(defn safe-parent [node] (or (.getParent node) node))
(defn root-node? [node] (-> node parent nil?))
(defn root-ancestor [node]
  (let [p (parent node)]
    (if (nil? p)
      node
      (recur p))))

(defn pre-tree
  ([f node] (pre-tree f node 1))
  ([f node index]

   (let [kids (children node)
         ret (case (arg-count f)
                   1 (f node)
                   2 (f node index))]

     (conj
           (doseq [iast-node kids]
             (pre-tree f iast-node (inc index)))
           ret)))
  ([f node index tree-path]

   (let [kids (children node)
         kids-last-index (count kids)
         ret (f node index tree-path)]

     (conj
           (doseq [[iast-node child-index] (map list kids (range 0 kids-last-index))]
             (pre-tree f iast-node (inc index) (conj tree-path child-index)))
           ret))))
(defn leaf? [node] (empty? (children node)))

(defn leaves [node]
  (if (leaf? node)
    node
    (flatten (map leaves (children node)))))

(defn all-parents
  "Get the all grandparents of the node"
  [node]
  (take-while some? (iterate parent node)))

(defn height
  "What is the furthest chain of children under this node"
  [node]
  (inc
   (apply max 0
          (map height
               (children node)))))

(defn depth "How many nodes lie between this one and the root"
  [node] (->> node all-parents count))

(defn ancestor
  "Get the nth grandparent of the node"
  [n node]
    (fn-pow parent node n))

(defn ancestral-instance?
  "Check whether any of this nodes ancestry are of the type listed"
  [type node]
  (if (nil? node)
    false
    (if (instance? type node)
      true
      (ancestral-instance? type (parent node)))))

(defn typename [node]
  (let [name (-> node .getClass .getSimpleName)]
    (nth (re-find #"AST(.*)" name) 1)))

(defn filter-height
  "Return every sub-tree of size n"
  [n node]
  ;; start from the leaves of the tree and walk upwards n generations
  (let [candidates (distinct (map (partial ancestor n) (leaves node)))]
    ;; candidates may still have deeper branches than the one we came up from
    (filter #(= n (height %)) candidates)))

(defn flatten-tree [node]
  (conj (mapcat flatten-tree (children node)) node))

(defn flatten-tree-infixy [node]
  "Approximate an in-order flattening"
  (if (instance? IASTBinaryExpression node)
    (concat (flatten-tree-infixy (.getOperand1 node)) [node]
            (flatten-tree-infixy (.getOperand2 node)))
    (cons node (mapcat flatten-tree-infixy (children node)))))

(defn filter-tree
  "Find every AST node that matches pred"
  [pred node]
  (->> node flatten-tree (filter pred)))

(defn filter-type
  "Return every example of type"
  [type node]
  (filter-tree #(= (typename %) type) node))

(defn filter-type-parent
  "Return the parent of each type"
  [type node]
  (->> node
       (filter-type type)
       (map parent)
       distinct))

(defn filter-instance
  [type node]
  ancestral-instance?
  (let [kids        (children node)
        kid-matches (mapcat (partial filter-instance type) kids)
        matches     (filter (partial instance? type) kids)]
    (concat matches kid-matches)))

(defn filter-instance-parent
  "Return the parent of each type"
  [type node]
  (->> node
       (filter-instance type)
       (map parent)
       distinct))

(defn get-in-tree
  "Find a value in the AST by indexes"
  [indices node]
  (cond
    (nil? node) nil
    (empty? indices) node
    :else (recur (rest indices) (nth (children node) (first indices) nil))))

(defn child
  "Return the only child of a unary node"
  [node]
  (let [[x1 x2] (children node)]
    ;(assert (nil? x2) (str "node '" (safe-write-ast node) "' at " (tree-path node) " should only have one child '" (safe-write-ast x1) "', but instead also had a second '" (safe-write-ast x2) "'"))
    (assert (nil? x2) (str "node should only have one child, but instead also had a second"))
    x1))

(defn parse-stmt
  "Turn a single C statement into an AST"
  [code]
  (->> (str "int main() {" code "\n} ")
      parse-source
      (get-in-tree [0 2 0])))

(defn parse-expr
  "Turn a single C expression into an AST"
  [code]
  (->> (str " (" code "\n);")
      parse-stmt
      (get-in-tree [0 0])))

; core/org.eclipse.cdt.core/parser/org/eclipse/cdt/internal/core/dom/rewrite/changegenerator/ChangeGenerator.java:getNextSiblingNode(IASTNode node)
(s/defn next-sibling :- (s/maybe IASTNode)
  [node :- IASTNode]
  (let [parent-node (parent node)
        siblings
          (condp instance? parent-node
                ICPPASTNamespaceDefinition (.getDeclarations (cast ICPPASTNamespaceDefinition) true)
                IASTCompositeTypeSpecifier (.getDeclarations (cast IASTCompositeTypeSpecifier) true)
                (children parent-node))]

        (find-after siblings node)))

(s/defn stmt-str? :- s/Bool
  [code :- String]
  (let [stmt-parse (parse-stmt code)]
    ; if the code isn't a statement the next node will be a problem statement
    (and (not (nil? stmt-parse))
      (not (instance? CPPASTProblemStatement (next-sibling stmt-parse)))) ;TODO replace with IProblemStatement
  ))

(defn valid-parse?
  [node]
  (and (not (instance? IASTProblemStatement node)) node))

(defn parse-frag
  "Turn a single C fragment (statement or expression) into an AST"
  [code]
  (->> [#(when (stmt-str? %1) (parse-stmt %1))
        parse-expr
        (fn [src] (let [node (-> src (str "\n;") parse-stmt)]
                    ;; some nodes change dramatically without their semicolon
                    ;; e.g. do { ... } while -> compound statement
                    ;; this is common in macro definitions
                    (if (instance? org.eclipse.cdt.core.dom.ast.IASTDoStatement node)
                      node
                      (get-in-tree [0] node))))
        parse-source]
       (map (fn [parser] (parser code)))
       (filter valid-parse?)
       first))

(defmulti loc "Get location information about an AST node" class)
(defmethod loc ASTFileLocation [l]
  (let [offset (.getNodeOffset l)
        length (.getNodeLength l)
        start-line (.getStartingLineNumber l)
        end-line  (.getEndingLineNumber l)]
    {:line (long start-line)
     :offset (long offset) :end-offset (long (+ length offset)) :length (long length)
     :start-line (long start-line) :end-line (long end-line)}))

(defmethod loc Object
  [node]
  (loc (.getFileLocation node)))

(defmethod loc :default [node] nil) ; catch nulls

(def offset (comp :offset loc))
(def end-offset (comp :end-offset loc))
(def start-line (comp :start-line loc))
(def end-line (comp :end-line loc))
(defn lines [node]
  '(if-let* [s (start-line node) ;TODO fix
            e (end-line node)]
    (range s (inc e))
    []))

(defn filename [node] (some-> node .getFileLocation .getFileName))

(defn all-preprocessor [node] (->> node root-ancestor .getAllPreprocessorStatements))
(defn all-macro-defs [node] (->> node root-ancestor .getMacroDefinitions))
(defn all-macro-exps [node] (->> node root-ancestor .getMacroExpansions))
(defn all-comments [node] (->> node root-ancestor .getComments (into [])))

(defn all-nodes [root]
  "Find all AST nodes as well as meta-nodes like Macro/Comment/etc"
  (apply concat ((juxt all-preprocessor all-comments flatten-tree) root)))

(defn parse-macro-def
  [macro-def]
  (let [[all name args body] (->> macro-def str (re-find #"^([^=(]*)(?:\(([^)]*)\))?=(.*)"))]
    {:name name
     :args (some-<>> args (str/split <> #",") (remove empty?))
     :body body}))

(s/defn parse-macro-def-body
  "Try to turn the body of a macro definition into
   an AST node with the correct file location/length"
  [macro-def :- org.eclipse.cdt.internal.core.parser.scanner.ASTMacroDefinition]
  (let [body     (:body (parse-macro-def macro-def))
        line-aligned-body (str (str/join (repeat (dec (start-line macro-def)) "\n")) body)
        new-node (parse-frag line-aligned-body)]

    (doto new-node
      ;(.setOffsetAndLength macro-def)
      )))

(import '(org.eclipse.cdt.core.dom.ast IASTMacroExpansionLocation))
(defn intersects-macro-exp?
  "Is any part of this node generated by a macro-expansion"
  [node]
  (->> node .getNodeLocations (exists? (partial instance? IASTMacroExpansionLocation))))

(defn contained-by-macro-exp?
  "Is this node entirely generated by a macro-expansion"
  [node]
  (let [[loc_first & loc_rest] (.getNodeLocations node)]
    (and (empty? loc_rest) (instance? IASTMacroExpansionLocation loc_first))))

(defn count-nodes
  "Count the size of the ast"
  [node]
    (inc (reduce + (map count-nodes (children node)))))

(defn contains-location?
  "Does this node contain the given offset/length"
  [root offset length]
  (let [root-loc (.getFileLocation root)
        root-offset (.getNodeOffset root-loc)
        root-length (.getNodeLength root-loc)]

    ;; The location/offset is fully contained in this node
    (and (<=    root-offset                 offset)
         (>= (+ root-offset root-length) (+ offset length)))))

(defn contains-offset?
  "Does this node contain the given offset"
  [node offset]
    (when-let [{node-offset :offset node-length :length} (some->> node loc)]
       (<= node-offset offset (+ node-offset node-length))))

(defn offset-parent?
  "True if this is deepest AST node that contains an offset"
  [node offset]
  (and
   (contains-offset? node offset)
   (not (exists? #(contains-offset? % offset) (children node)))))

'(s/defn search-tree-by :- (s/maybe IASTNode)
  "binary search the tree for val, after applying (f node)"
  [f :- (s/=> s/Any IASTNode) val :- s/Any root :- IASTNode]
  )

; https://stackoverflow.com/a/8950240
(s/defn binary-search-children-offset :- (s/maybe IASTNode)
  [target :- s/Int kids] ; :- [IASTNode]]
  (loop [l 0 h (unchecked-dec (count kids))]
    (if (<= h (inc l))
      (cond
        (== h -1) nil
        (contains-offset? (nth kids l) target) (nth kids l)
        (contains-offset? (nth kids h) target) (nth kids h)
        :else nil)
      (let [m (unchecked-add l (bit-shift-right (unchecked-subtract h l) 1))]
        (if (<= target (-> kids (nth m) end-offset))
          (recur l m)
          (recur (unchecked-inc m) h))))))

(s/defn binary-search-location-parent :- (s/maybe IASTNode)
  "binary search the tree for an offset"
  [target-off :- s/Int target-len :- s/Int root :- IASTNode]
  (let [{start :offset len :length} (loc root)]
    (when (<= start target-off (+ target-off target-len) (+ start len))
      (or (some->> (binary-search-children-offset target-off (children root))
                   (binary-search-location-parent target-off target-len)) ; make tail-recursive?
          root))))

(s/defn offset-parent
  "Find the AST node that contains the whole location offset
   Assumes that no children of a single parent overlap in terms of offset"
  ([root :- IASTNode offset :- s/Int] (binary-search-location-parent offset 0 root))
  ([node :- IASTNode] (offset-parent (root-ancestor node) (:offset (loc node)))))

(s/defn location-parent
  [node :- IASTNode]
  (let [{start :offset len :length} (loc node)]
    (binary-search-location-parent start len (root-ancestor node))))

;(s/defn map-dir-trees
;  "Apply a function to the root of the AST of every c file in a directory"
;  [f :- (s/=> s/Any [IASTTranslationUnit]) dirname :- s/Str]
;  (map-dir-files (comp f parse-file) dirname))
;
;(s/defn map-dir-nodes
;  "Apply a function to the every node of every c file in a directory"
;  [f :- (s/=> s/Any [IASTTranslationUnit]) dirname :- s/Str]
;  (flatten (map-dir-trees #(->> % flatten-tree (map f)) dirname)))

(s/defn tree-path
  "the path from the root to this node"
  [node]
  (let [dad (parent node)]
    (if (nil? dad)
      []
      (conj (tree-path dad) (.indexOf (seq (children dad)) node)))))

; The name of the variable/function/argument/etc
(defmulti node-name class)
(defmethod node-name IASTName [node] (-> node .getSimpleID String.))
(defmethod node-name IASTIdExpression [node] (-> node .getName node-name))
(defmethod node-name IASTDeclarator [node] (-> node .getName node-name))
(defmethod node-name IASTFunctionDefinition [node] (-> node .getDeclarator node-name))
(defmethod node-name :default [node] nil)

(s/defn from-include?
  "Does this AST node originate in an included file"
  ([node :- IASTNode] (from-include? (root-ancestor node) node))
  ([root :- IASTTranslationUnit node :- IASTNode]
   (not (=by filename root node))))
