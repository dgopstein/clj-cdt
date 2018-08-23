(ns clj-cdt.expr-operator
  (:require [clojure.string :as str]
            [schema.core :as s])
  (:import [org.eclipse.cdt.core.dom.ast IASTUnaryExpression IASTBinaryExpression]
           [org.eclipse.cdt.internal.core.dom.parser.cpp
            CPPASTQualifiedName CPPASTExpressionList CPPASTConditionalExpression
            CPPASTNewExpression CPPASTDeleteExpression CPPASTCastExpression
            CPPASTArraySubscriptExpression CPPASTFieldReference CPPASTFunctionCallExpression]))

(def ExprOperator (s/maybe {:enum (s/maybe s/Int) :arity (s/maybe s/Int) :name s/Keyword :precedence s/Int :syntax s/Str}))

(defmulti expr-operator "If the node is an operator expression, return information about its operator" class)

(s/defmethod expr-operator :default :- ExprOperator [node]
  (-> node type
      {CPPASTQualifiedName            {:enum nil :arity 2   :name :qualified-name  :precedence 1  :syntax "::"       }
       CPPASTArraySubscriptExpression   {:enum nil :arity 2   :name :array-subscript :precedence 2  :syntax "[]"       }
       CPPASTFieldReference             {:enum nil :arity 2   :name :field-reference :precedence 2  :syntax ". or ->"  }
       CPPASTCastExpression             {:enum nil :arity 2   :name :cast            :precedence 2  :syntax "(<TYPE>)" }
       CPPASTFunctionCallExpression     {:enum nil :arity 2   :name :function-call   :precedence 2  :syntax "F()"      }
       CPPASTNewExpression            {:enum nil :arity 1   :name :new             :precedence 3  :syntax "new"      }
       CPPASTDeleteExpression         {:enum nil :arity 1   :name :delete          :precedence 3  :syntax "delete"   }
       CPPASTConditionalExpression    {:enum nil :arity 3   :name :conditional     :precedence 15 :syntax "?:"       }
       CPPASTExpressionList           {:enum nil :arity nil :name :expression-list :precedence 16 :syntax ","        }}))

(s/defmethod expr-operator IASTUnaryExpression :- ExprOperator [node]
  (-> node .getOperator
      {
       IASTUnaryExpression/op_prefixIncr       {:enum 0  :arity 1 :name :prefixIncr       :precedence 3  :syntax "++x"    }
       IASTUnaryExpression/op_prefixDecr       {:enum 1  :arity 1 :name :prefixDecr       :precedence 3  :syntax "--x"    }
       IASTUnaryExpression/op_plus             {:enum 2  :arity 1 :name :plus             :precedence 3  :syntax "+x"     }
       IASTUnaryExpression/op_minus            {:enum 3  :arity 1 :name :minus            :precedence 3  :syntax "-x"     }
       IASTUnaryExpression/op_star             {:enum 4  :arity 1 :name :star             :precedence 3  :syntax "*x"     }
       IASTUnaryExpression/op_amper            {:enum 5  :arity 1 :name :amper            :precedence 3  :syntax "&x"     }
       IASTUnaryExpression/op_tilde            {:enum 6  :arity 1 :name :tilde            :precedence 3  :syntax "~x"     }
       IASTUnaryExpression/op_not              {:enum 7  :arity 1 :name :not              :precedence 3  :syntax "!x"     }
       IASTUnaryExpression/op_sizeof           {:enum 8  :arity 1 :name :sizeof           :precedence 3  :syntax "sizeof" }
       IASTUnaryExpression/op_postFixIncr      {:enum 9  :arity 1 :name :postFixIncr      :precedence 2  :syntax "x++"    }
       IASTUnaryExpression/op_postFixDecr      {:enum 10 :arity 1 :name :postFixDecr      :precedence 2  :syntax "x--"    }
       IASTUnaryExpression/op_bracketedPrimary {:enum 11 :arity 1 :name :bracketedPrimary :precedence 0 :syntax "(x)"     }
       IASTUnaryExpression/op_throw            {:enum 12 :arity 1 :name :throw            :precedence 15 :syntax "throw"  }}))
       ;IASTUnaryExpression/op_typeid ; 13
       ;IASTUnaryExpression/op_typeof ; 14 - deprecated
       ;IASTUnaryExpression/op_sizeofParameterPack ; 16
       ;IASTUnaryExpression/op_noexcept ; 17
       ;IASTUnaryExpression/op_labelReference ; 18

(s/defmethod expr-operator IASTBinaryExpression :- ExprOperator [node]
  (-> node .getOperator
      {
       IASTBinaryExpression/op_multiply          {:enum 1  :arity 2 :name :multiply         :precedence 5  :syntax "*"    }
       IASTBinaryExpression/op_divide            {:enum 2  :arity 2 :name :divide           :precedence 5  :syntax "/"    }
       IASTBinaryExpression/op_modulo            {:enum 3  :arity 2 :name :modulo           :precedence 5  :syntax "%"    }
       IASTBinaryExpression/op_plus              {:enum 4  :arity 2 :name :plus             :precedence 6  :syntax "+"    }
       IASTBinaryExpression/op_minus             {:enum 5  :arity 2 :name :minus            :precedence 6  :syntax "-"    }
       IASTBinaryExpression/op_shiftLeft         {:enum 6  :arity 2 :name :shiftLeft        :precedence 7  :syntax "<<"   }
       IASTBinaryExpression/op_shiftRight        {:enum 7  :arity 2 :name :shiftRight       :precedence 7  :syntax ">>"   }
       IASTBinaryExpression/op_lessThan          {:enum 8  :arity 2 :name :lessThan         :precedence 8  :syntax "<"    }
       IASTBinaryExpression/op_greaterThan       {:enum 9  :arity 2 :name :greaterThan      :precedence 8  :syntax ">"    }
       IASTBinaryExpression/op_lessEqual         {:enum 10 :arity 2 :name :lessEqual        :precedence 8  :syntax "<="   }
       IASTBinaryExpression/op_greaterEqual      {:enum 11 :arity 2 :name :greaterEqual     :precedence 8  :syntax ">="   }
       IASTBinaryExpression/op_binaryAnd         {:enum 12 :arity 2 :name :binaryAnd        :precedence 10 :syntax "&"    }
       IASTBinaryExpression/op_binaryXor         {:enum 13 :arity 2 :name :binaryXor        :precedence 11 :syntax "^"    }
       IASTBinaryExpression/op_binaryOr          {:enum 14 :arity 2 :name :binaryOr         :precedence 12 :syntax "|"    }
       IASTBinaryExpression/op_logicalAnd        {:enum 15 :arity 2 :name :logicalAnd       :precedence 13 :syntax "&&"   }
       IASTBinaryExpression/op_logicalOr         {:enum 16 :arity 2 :name :logicalOr        :precedence 14 :syntax "||"   }
       IASTBinaryExpression/op_assign            {:enum 17 :arity 2 :name :assign           :precedence 15 :syntax "="    }
       IASTBinaryExpression/op_multiplyAssign    {:enum 18 :arity 2 :name :multiplyAssign   :precedence 15 :syntax "*="   }
       IASTBinaryExpression/op_divideAssign      {:enum 19 :arity 2 :name :divideAssign     :precedence 15 :syntax "/="   }
       IASTBinaryExpression/op_moduloAssign      {:enum 20 :arity 2 :name :moduloAssign     :precedence 15 :syntax "%="   }
       IASTBinaryExpression/op_plusAssign        {:enum 21 :arity 2 :name :plusAssign       :precedence 15 :syntax "+="   }
       IASTBinaryExpression/op_minusAssign       {:enum 22 :arity 2 :name :minusAssign      :precedence 15 :syntax "-="   }
       IASTBinaryExpression/op_shiftLeftAssign   {:enum 23 :arity 2 :name :shiftLeftAssign  :precedence 15 :syntax "<<="  }
       IASTBinaryExpression/op_shiftRightAssign  {:enum 24 :arity 2 :name :shiftRightAssign :precedence 15 :syntax ">>="  }
       IASTBinaryExpression/op_binaryAndAssign   {:enum 25 :arity 2 :name :binaryAndAssign  :precedence 15 :syntax "&="   }
       IASTBinaryExpression/op_binaryXorAssign   {:enum 26 :arity 2 :name :binaryXorAssign  :precedence 15 :syntax "^="   }
       IASTBinaryExpression/op_binaryOrAssign    {:enum 27 :arity 2 :name :binaryOrAssign   :precedence 15 :syntax "|="   }
       IASTBinaryExpression/op_equals            {:enum 28 :arity 2 :name :equals           :precedence 9  :syntax "=="   }
       IASTBinaryExpression/op_notequals         {:enum 29 :arity 2 :name :notequals        :precedence 9  :syntax "!="   }
       IASTBinaryExpression/op_pmdot             {:enum 30 :arity 2 :name :pmdot            :precedence 4  :syntax "."    }
       IASTBinaryExpression/op_pmarrow           {:enum 31 :arity 2 :name :pmarrow          :precedence 4  :syntax "->"   }
       ; IASTBinaryExpression/op_max ; 32
       ; IASTBinaryExpression/op_min ; 33
       ; IASTBinaryExpression/op_ellipses ; 34
       }))
