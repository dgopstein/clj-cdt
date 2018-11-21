;; Update AST's in memory

(ns clj-cdt.modify-ast
  (:require
   [schema.core :as s]
   [clj-cdt.clj-util :refer :all]
   [clj-cdt.clj-cdt :refer :all]
   [clj-cdt.writer-util :refer :all]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode IASTArrayModifier
      IASTArraySubscriptExpression IASTBinaryExpression IASTCaseStatement
      IASTCastExpression IASTConditionalExpression IASTDoStatement
      IASTEnumerationSpecifier IASTEnumerationSpecifier$IASTEnumerator
      IASTEqualsInitializer IASTExpression IASTExpressionList
      IASTExpressionStatement IASTFieldDeclarator IASTFieldReference
      IASTForStatement IASTFunctionCallExpression IASTIfStatement
      IASTInitializerExpression IASTIdExpression IASTReturnStatement
      IASTSimpleDeclSpecifier IASTSwitchStatement IASTUnaryExpression
      IASTWhileStatement]
   [org.eclipse.cdt.core.dom.ast.c ICASTArrayDesignator]
   [org.eclipse.cdt.core.dom.ast.cpp ICPPASTArrayDesignator
      ICPPASTConstructorChainInitializer ICPPASTConstructorInitializer
      ICPPASTDeleteExpression ICPPASTFunctionDeclarator ICPPASTInitializerList
      ICPPASTNewExpression ICPPASTPackExpansionExpression
      ICPPASTSimpleTypeConstructorExpression ICPPASTTemplateId
      ICPPASTTemplatedTypeTemplateParameter ICPPASTTypenameExpression]
   [org.eclipse.cdt.core.dom.ast.gnu cpp.IGPPASTSimpleDeclSpecifier
           c.IGCCASTArrayRangeDesignator c.IGCCASTSimpleDeclSpecifier
           IGNUASTGotoStatement]))

(def -expr-getter-setter-suffixes
  [
   [IASTArrayModifier "ConstantExpression"]
   [IASTArraySubscriptExpression "Argument" "ArrayExpression"]
   [IASTBinaryExpression "Operand1" "Operand2"]
   [IASTCaseStatement "Expression"]
   [IASTCastExpression "Operand"]
   [IASTConditionalExpression "LogicalConditionExpression" "PositiveResultExpression" "NegativeResultExpression"]
   [IASTDoStatement "Condition"]
   [IASTEnumerationSpecifier "Name"]
   [IASTEnumerationSpecifier$IASTEnumerator "Name" "Value"]
   [IASTEqualsInitializer "InitializerClause"]
   [IASTExpressionStatement "Expression"]
   [IASTFieldDeclarator "BitFieldSize"]
   [IASTFieldReference "FieldOwner" "FieldName"]
   [IASTForStatement "ConditionExpression" "IterationExpression"]
   [IASTFunctionCallExpression "FunctionNameExpression"]
   [IASTIfStatement "ConditionExpression"]
   [IASTInitializerExpression "Expression"]
   [IASTIdExpression "Name"]
   [IASTReturnStatement "ReturnValue"]
   [IASTSimpleDeclSpecifier "DeclTypeExpression"]
   [IASTSwitchStatement "ControllerExpression"]
   [IASTUnaryExpression "Operand"]
   [IASTWhileStatement "Condition"]
   [ICASTArrayDesignator "SubscriptExpression"]
   [ICPPASTArrayDesignator "SubscriptExpression"]
   [ICPPASTConstructorChainInitializer "InitializerValue"]
   ;[ICPPASTConstructorInitializer "Expression"]
   [ICPPASTDeleteExpression "Operand"]
   [ICPPASTFunctionDeclarator "NoexceptExpression"]
   [ICPPASTNewExpression "NewPlacement" "NewInitializer" "PlacementArguments"]
   [ICPPASTPackExpansionExpression  "Pattern"]
   [ICPPASTSimpleTypeConstructorExpression "InitialValue"]
   [ICPPASTTemplatedTypeTemplateParameter "DefaultValue"]
   [ICPPASTTypenameExpression "InitialValue"]
   [IGCCASTArrayRangeDesignator "RangeFloor" "RangeCeiling"]
   [IGCCASTSimpleDeclSpecifier "TypeofExpression"]
   [IGNUASTGotoStatement "LabelNameExpression"]
   [IGPPASTSimpleDeclSpecifier "TypeofExpression"]
   ])

(defn -call-method "Call a method by name from a string" [obj m & args]
  (clojure.lang.Reflector/invokeInstanceMethod obj m (to-array args)))

(def -expr-getters-setters-by-type
  "A mapping between each (eclipse) type of Expression node
   and all the methods that get and set its children"
  (->> -expr-getter-setter-suffixes
       (map (fn [[type & suffixes]]
              [type
               {:getters (map (fn [sfx] #(-call-method % (str 'get sfx))) suffixes)
                :setters (map (fn [sfx] #(-call-method %1 (str 'set sfx) %2)) suffixes)}]))
       (into {})
       ))

(s/defn -function-call-getter-setter
  [fn-expr :- IASTFunctionCallExpression]
  (let [args (.getArguments fn-expr)
        thawed-fn-expr (.copy fn-expr)
        thawed-args (.getArguments thawed-fn-expr)]
    {:getters (cons #(.getFunctionNameExpression fn-expr) (map constantly args))
     :setters
     (cons #(do (.setFunctionNameExpression thawed-fn-expr (.copy %))
                thawed-fn-expr)
           (->> thawed-args
                (map-indexed
                 (fn [idx _]
                   (fn [new-child]
                     (aset thawed-args idx (.copy new-child))
                     thawed-fn-expr)))))}
    )
  )

(s/defn -template-id-getter-setter
  [template-id :- ICPPASTTemplateId]
  (let [args (.getTemplateArguments template-id)
        thawed-template-id (.copy template-id)
        thawed-args (.getTemplateArguments thawed-template-id)]
    {:getters (cons #(.getTemplateName template-id) (map constantly args))
     :setters
     (cons #(do (.setTemplateName thawed-template-id (.copy %))
                thawed-template-id)
           (->> thawed-args
                (map (fn [old-arg]
                       (fn [new-arg]
                         (.replace thawed-template-id old-arg (.copy new-arg))
                         thawed-template-id)))))}
    )
  )

(s/defn -getters-setters-using-replace
  "Some GCC specific AST nodes use .replace to swap out children.
   This function takes the boilerplate out of configuring those
   getter/setter generation functions"
  [get-args-fn parent-node]
    (let [args (get-args-fn parent-node)
          thawed-parent-node (.copy parent-node)
          thawed-args (get-args-fn thawed-parent-node)]
      {:getters (map constantly args)
       :setters (->> thawed-args
                     (map (fn [old-arg]
                            (fn [new-arg]
                              (.replace thawed-parent-node old-arg (.copy new-arg))
                              thawed-parent-node))))}
    ))

(s/defn expr-getters-setters :- {:getters [(s/=> IASTExpression)]
                                 :setters [(s/=> IASTNode IASTExpression)]}
  "For a given expression AST node, return functions that get/set it's children.
   The getters operate on the original node, so that children may be matched by
   equality,and the setters operate on a copy, so that it may be mutated."
  [node :- IASTNode]
  (condp instance? node
    IASTFunctionCallExpression    (-function-call-getter-setter node)
    ICPPASTTemplateId             (-template-id-getter-setter node)
    IASTExpressionList            (-getters-setters-using-replace (memfn getExpressions) node)
    ICPPASTConstructorInitializer (-getters-setters-using-replace (memfn getArguments) node)
    ICPPASTInitializerList        (-getters-setters-using-replace (memfn getClauses) node)
    (let [thawed-node (.copy node)
          getter-setter-map
            (->> -expr-getters-setters-by-type
            (filter #(instance? (first %) node))
            first last)]
          (->
           getter-setter-map
           (update :getters (partial map #(partial % node)))
           (update :setters (partial map (fn [setter]
                                           (fn [new-child]
                                             (setter thawed-node (.copy new-child))
                                             thawed-node))))
           ))))

(s/defn replace-expr-by-idx :- IASTNode
  "Update a child inside a parent node by its position.
   Returns an updated copy of the parent."
  [parent :- IASTNode
   old-idx :- s/Int
   new-child :- IASTExpression
   ]
  (let [{getters :getters setters :setters} (expr-getters-setters parent)
        setter (nth setters old-idx)]

     (setter new-child)
    ))

(s/defn replace-exprs :- IASTNode
  "Update multiple children inside a parent node.
   Returns an updated copy of the parent."
  [parent :- IASTNode
   old-kids ;;:- [IASTExpression] - These don't play nicely with java arrays
   new-kids ;;:- [IASTExpression]
   ]

  (assert (=by count old-kids new-kids) (str "Old expressions must be replaced by the same number of new expressions. You tried to replace " (count old-kids) " with " (count new-kids)))

  (if (empty? old-kids)
    parent
    (let [{getters :getters setters :setters} (expr-getters-setters parent)
          old-idxs (map (fn [old-child] (.indexOf (map #(%) getters) old-child)) old-kids)]

      (reduce (fn [mom [old-idx new-kid]]
                (let [setter (nth setters old-idx)]
                  (setter new-kid))) parent (map vector old-idxs new-kids)))))

(s/defn replace-all-exprs :- IASTNode
  "Update all children inside a parent node.
   Returns an updated copy of the parent."
  [parent new-kids]
  (replace-exprs parent (children parent) new-kids))

(s/defn replace-expr :- IASTNode
  "Update a child inside a parent node.
   Returns an updated copy of the parent."
  [parent old-child new-child]

  (replace-exprs parent [old-child] [new-child]))
