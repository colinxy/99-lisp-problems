
;; 46. Truth tables for logical expressions.

;; Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed. Note that A and B can be Prolog goals (not only the constants true and fail).
;; A logical expression in two variables can then be written in prefix notation, as in the following example: and(or(A,B),nand(A,B)).

;; Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

;; Example:
;; * table(A,B,and(A,or(A,B))).
;; true true true
;; true fail true
;; fail true fail
;; fail fail fail
;; * (table 'A 'B '(and A (or A B)))
;; ((T T T)
;;  (T NIL T)
;;  (NIL T NIL)
;;  (NIL NIL NIL))

;; only `and', `or' are supported as logical operator,
;; assumed to be binary operator
(defun logical-prefix-eval (expr mapping)
  (let ((operator      (first expr))
        (left-operand  (second expr))
        (right-operand (third expr)))
    (if (symbolp left-operand)
        (setf left-operand (funcall mapping left-operand))
        (setf left-operand (logical-prefix-eval left-operand mapping)))
    (if (symbolp right-operand)
        (setf right-operand (funcall mapping right-operand))
        (setf right-operand (logical-prefix-eval right-operand mapping)))
    (cond
      ((eq operator 'and) (and left-operand right-operand))
      ((eq operator 'or)  (or  left-operand right-operand))
      (t nil))))

(defun table (a b expr)
  (flet ((key-val-mapping (pair1 pair2) (lambda (symb)
                                          (cond
                                            ((eq symb (car pair1)) (cdr pair1))
                                            ((eq symb (car pair2)) (cdr pair2))
                                            (t nil)))))
    (list
     (list t t
           (logical-prefix-eval expr (key-val-mapping (cons a t) (cons b t))))
     (list t nil
           (logical-prefix-eval expr (key-val-mapping (cons a t) (cons b nil))))
     (list nil t
           (logical-prefix-eval expr (key-val-mapping (cons a nil) (cons b t))))
     (list nil nil
           (logical-prefix-eval expr (key-val-mapping (cons a nil) (cons b nil)))))))


;; 47. Truth tables for logical expressions (2).
;; Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.

;; Example:
;; * table(A,B, A and (A or not B))
;; true true true
;; true fail true
;; fail true fail
;; fail fail fail
;; * table('A, 'B, '(A and (A or not B)))

;; trivial modification prefix to infix, omitted
;; (defun logical-infix-eval (expr mapping)
;;   )


;; 48. Truth tables for logical expressions (3).
;; Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.

;; Example:
;; * table([A,B,C], A and (B or C) equ A and B or A and C).
;; true true true true
;; true true fail true
;; true fail true true
;; true fail fail true
;; fail true true true
;; fail true fail true
;; fail fail true true
;; fail fail fail true
;; * (table-extended '(a b c) '(a and (b or c) eq a and b or a and c))

(defun table-extended (symbols expr)
  )


;; 49. Gray code.

;; An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
;; n = 1: C(1) = ['0','1'].
;; n = 2: C(2) = ['00','01','11','10'].

;; Find out the construction rules and write a predicate with the following specification:

;; % gray(N,C) :- C is the N-bit Gray code

;; Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?

(defparameter gray-cache (make-array 1
                                     :fill-pointer 1
                                     :element-type 'list
                                     :adjustable t
                                     :initial-contents '((""))))

(defun gray-code (n)
  (if (array-in-bounds-p gray-cache n)
      (aref gray-cache n)
      (let* ((gray-n-1         (gray-code (1- n)))
             (gray-n-1-reverse (reverse gray-n-1))
             (gray-n (append
                      (mapcar #'(lambda (str) (concatenate 'string "0" str))
                              gray-n-1)
                      (mapcar #'(lambda (str) (concatenate 'string "1" str))
                              gray-n-1-reverse))))
        (vector-push-extend gray-n gray-cache)
        gray-n)))
