
;; A binary tree is either empty or it is composed of a root element and two successors, which are binary trees themselves.

;; 54A. Check whether a given term represents a binary tree
;; Write a predicate istree which returns true if and only if its argument is a list representing a binary tree.

;; Example:
;; * (istree '(a (b nil nil) nil))
;; T
;; * (istree '(a (b nil nil)))
;; NIL

(defun istree (tree)
  (if (null tree)
      t
      (and (= 3 (length tree))
           (atom   (first tree))
           (istree (second tree))
           (istree (third tree)))))


;; 55. Construct completely balanced binary trees
;; In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

;; Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
;; Example:
;; * cbal-tree(4,T).
;; T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
;; T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;

;; * (cbal-tree 4)
;; ((X (X NIL NIL) (X NIL (X NIL NIL)))
;;  (X (X NIL NIL) (X (X NIL NIL) NIL))
;;  (X (X NIL (X NIL NIL)) (X NIL NIL))
;;  (X (X (X NIL NIL) NIL) (X NIL NIL)))

(defun cartesian-product (lst-lst1 lst-lst2)
  (mapcan #'(lambda (lst-left)
              (mapcar #'(lambda (lst-right) (list lst-left lst-right))
                      lst-lst2))
          lst-lst1))

(defun cbal-tree (n)
  (if (< n 1)
      '(nil)
      (mapcar #'(lambda (children) (cons 'X children))
              (if (evenp (1- n))
                  (let ((subtree (cbal-tree (/ (1- n) 2))))
                    (cartesian-product subtree subtree))
                  (append
                   (cartesian-product (cbal-tree (floor (1- n) 2))
                                      (cbal-tree (ceiling (1- n) 2)))
                   (cartesian-product (cbal-tree (ceiling (1- n) 2))
                                      (cbal-tree (floor (1- n) 2))))))))


;; 56. Symmetric binary trees
;; Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.

(defun tree-symmetric-p ()
  )
