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
      IASTInitializerExpression IASTReturnStatement IASTSimpleDeclSpecifier
      IASTSwitchStatement IASTUnaryExpression IASTWhileStatement]
   [org.eclipse.cdt.core.dom.ast.c ICASTArrayDesignator]
   [org.eclipse.cdt.core.dom.ast.cpp ICPPASTArrayDesignator
      ICPPASTConstructorChainInitializer ICPPASTConstructorInitializer
      ICPPASTDeleteExpression ICPPASTFunctionDeclarator ICPPASTNewExpression
      ICPPASTPackExpansionExpression ICPPASTSimpleTypeConstructorExpression
      ICPPASTTemplateId ICPPASTTemplatedTypeTemplateParameter
      ICPPASTTypenameExpression]
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

(s/defn -constructor-initializer-getter-setter
  [cons-init :- ICPPASTConstructorInitializer]
  (let [args (.getArguments cons-init)
        thawed-cons-init (.copy cons-init)
        thawed-args (.getArguments thawed-cons-init)]
    {:getters (map constantly args)
     :setters (->> thawed-args
                (map (fn [old-arg]
                       (fn [new-arg]
                         (.replace thawed-cons-init old-arg (.copy new-arg))
                         thawed-cons-init))))}
    )
  )

(s/defn -expression-list-getter-setter
  [expr-list :- IASTExpressionList]
  (let [exprs (.getExpressions expr-list)
        thawed-expr-list (.copy expr-list)
        thawed-exprs (.getExpressions thawed-expr-list)]
    {:getters (map constantly exprs)
     :setters
     (->> thawed-exprs
          (map (fn [thawed-expr]
                 (fn [new-child]
                   (.replace thawed-expr-list thawed-expr (.copy new-child))
                   thawed-expr-list))))}
    )
  )

(s/defn expr-getters-setters :- {:getters [(s/=> IASTExpression)]
                                 :setters [(s/=> IASTNode IASTExpression)]}
  "For a given expression AST node, return functions that get/set it's children.
   The getters operate on the original node, so that children may be matched by
   equality,and the setters operate on a copy, so that it may be mutated."
  [node :- IASTNode]
  (condp instance? node
    IASTFunctionCallExpression    (-function-call-getter-setter node)
    ICPPASTTemplateId             (-template-id-getter-setter node)
    IASTExpressionList            (-expression-list-getter-setter node)
    ICPPASTConstructorInitializer (-constructor-initializer-getter-setter node)
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

(s/defn replace-expr :- IASTNode
  "Update a child inside a parent node.
   Returns an updated copy of the parent."
  [parent :- IASTNode
   old-child :- IASTExpression
   new-child :- IASTExpression]
  (let [{getters :getters setters :setters} (expr-getters-setters parent)
        old-idx (.indexOf (map #(%) getters) old-child)
        setter (nth setters old-idx)]

     (setter new-child)
    ))
