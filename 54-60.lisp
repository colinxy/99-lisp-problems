
;; A binary tree is either empty or it is composed of a root element and two successors, which are binary trees themselves.

;; 54A. Check whether a given term represents a binary tree
;; Write a predicate istree which returns true if and only if its argument is a list representing a binary tree.

;; Example:
;; * (istree '(a (b nil nil) nil))
;; T
;; * (istree '(a (b nil nil)))
;; NIL

(defun istree (tree)
  (or (null tree)
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

(defun cartesian-product (lst1 lst2)
  (mapcan #'(lambda (left)
              (mapcar #'(lambda (right) (list left right))
                      lst2))
          lst1))

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

(defun tree-mirror-p (left-tree right-tree)
  (if (and left-tree right-tree)
      (and (tree-mirror-p (second left-tree)  (third right-tree))
           (tree-mirror-p (second right-tree) (third left-tree)))
      (and (null left-tree)
           (null right-tree))))

(defun tree-symmetric-p (tree)
  (tree-mirror-p (second tree) (third tree)))


;; 57. Binary search trees (dictionaries)
;; Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.
;; Example:
;; * construct([3,2,5,7,1],T).
;; T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))

;; * (construct '(3 2 5 7 1))
;; (3 (2 (1 NIL NIL) NIL) (5 NIL (7 NIL NIL)))

;; Then use this predicate to test the solution of the problem P56.
;; Example:
;; * test-symmetric([5,3,18,1,4,12,21]).
;; Yes
;; * test-symmetric([3,2,5,7,1]).
;; Yes

;; * (tree-symmetric-p (construct '(5 3 18 1 4 12 21)))
;; T
;; * (tree-symmetric-p (construct '(3 2 5 7 1)))
;; T

;; TREE HAS TO BE NOT EMPTY
(defun insert-into-tree (elem tree)
  (loop
     for prev = tree then curr
     for curr = (if (< elem (car prev))
                    (second prev)
                    (third  prev))

     while curr
     finally (if (< elem (car prev))
                 (setf (second prev) (list elem nil nil))
                 (setf (third  prev) (list elem nil nil)))))

(defun construct (lst)
  (let ((tree (list (car lst) nil nil)))
    (loop for elem in (cdr lst)
       do (insert-into-tree elem tree))
    tree))


;; 58. Generate-and-test paradigm

;; Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes. Example:
;; * sym-cbal-trees(5,Ts).
;; Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]

;; * (sym-cbal-trees 5)
;; ((X (X NIL (X NIL NIL)) (X (X NIL NIL) NIL))
;;  (X (X (X NIL NIL) NIL) (X NIL (X NIL NIL))))

;; How many such trees are there with 57 nodes? Investigate about how many solutions there are for a given number of nodes? What if the number is even? Write an appropriate predicate.

;; certainly better algorithm exists, but generate-and-test is okay
(defun sym-cbal-trees (n)
  (remove-if-not #'tree-symmetric-p (cbal-tree n)))


;; 59. Construct height-balanced binary trees
;; In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.

;; Write a predicate hbal-tree/2 to construct height-balanced binary trees for a given height. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
;; Example:
;; * hbal-tree(3,T).
;; T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
;; T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;

(defun hbal-tree ()
  )
