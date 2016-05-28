
(load "11-20.lisp")

;; 21. Insert an element at a given position into a list.

;; Example:
;; * (insert-at 'alfa '(a b c d) 2)
;; (A ALFA B C D)

;; cannot insert into end of the list (not specified)
(defun insert-at (elem lst index)
  (if (and lst
           (>= index 1))
      (if (= 1 index)
          (cons elem lst)
          (cons (car lst)
                (insert-at elem (cdr lst) (1- index))))
      lst))


;; 22. Create a list containing all integers within a given range.
;; If first argument is smaller than second, produce a list in decreasing order.
;; Example:
;; * (range 4 9)
;; (4 5 6 7 8 9)

;; number-sequence is available in emacs lisp
(defun range (from to)
  (if (<= from to)
      (cons from
            (range (1+ from) to))
      nil))


;; 23. Extract a given number of randomly selected elements from a list.
;; The selected items shall be returned in a list.

;; Example:
;; * (rnd-select '(a b c d e f g h) 3)
;; (E D A)

(defun rnd-select (lst count)
  (if (> count 0)
      (let ((index (random (length lst)))) ;INDEX is 0 based
        (cons (nth index lst)
              (rnd-select (remove-at lst (1+ index)) ;REMOVE-AT is 1-based
                          (1- count))))
      nil))


;; 24. Lotto: Draw N different random numbers from the set 1..M.
;; The selected numbers shall be returned in a list.

;; Example:
;; * (lotto-select 6 49)
;; (23 1 17 33 21 37)

(defun lotto-select (n m)
  (rnd-select (range 1 m) n))


;; 25. Generate a random permutation of the elements of a list.

;; Example:
;; * (rnd-permu '(a b c d e f))
;; (B A D C E F)

(defun rnd-permu (lst)
  (rnd-select lst (length lst)))


;; 26. Generate the combinations of K distinct objects chosen from the N elements of a list

;; In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

;; Example:
;; * (combination 3 '(a b c d e f))
;; ((A B C) (A B D) (A B E) ... )

(defun combination (k lst)
  (if (> k (length lst))
      nil
      (if (> k 0)
          (append (mapcar #'(lambda (rest) (cons (car lst) rest))
                          (combination (1- k) (cdr lst)))
                  (combination k (cdr lst)))
          '(()))))                      ;has to use list of list


;; 27. Group the elements of a set into disjoint subsets.
;; a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.

;; Example:
;; * (group3 '(aldo beat carla david evi flip gary hugo ida))
;; ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
;; ... )

;; b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

;; Example:
;; * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
;; ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
;; ... )

;; Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

;; assume heterogeneous list
(defun remove-all (items lst)
  (dolist (i items lst)
    (setf lst (remove i lst :count 1)))) ;oh my complexity

;; assume heterogeneous list
(defun group3 (lst)
  "Group into (2 3 4)."
  (if (= 9 (length lst))
      (loop for g2 in (combination 2 lst)
         for rest3-4 = (remove-all g2 lst)
         append
           (mapcar #'(lambda (group3-4) (cons g2 group3-4))
                   (loop for g3 in (combination 3 rest3-4)
                      for rest4 = (remove-all g3 rest3-4)
                      collect (list g3 rest4))))
      nil))

;; assume heterogeneous list
(defun group (lst groups)
  (if (= (apply #'+ groups)
         (length lst))
      (if groups
          (loop for g in (combination (car groups) lst)
             for rest = (remove-all g lst)
             append
               (mapcar #'(lambda (rest-groups) (cons g rest-groups))
                       (group rest (cdr groups))))
          '(()))
      nil))


;; 28. Sorting a list of lists according to length of sublists

;; a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

;; Example:
;; * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;; ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))

;; b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

;; Example:
;; * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;; ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))

;; didn't bother to optimize, calls length multiple times
(defun lsort (lst-lst)
  (sort (copy-seq lst-lst) #'< :key #'length))

(defun make-histogram (sequence key)
  (let ((histogram (make-hash-table)))
    (map nil #'(lambda (elem)
                 (incf (gethash (funcall key elem) histogram 0)))
         sequence)                      ;use map in place of loop
    histogram))

(defun lfsort (lst-lst)
  (let ((length-table (make-histogram lst-lst #'length)))
    (sort (copy-seq lst-lst) #'<
          :key #'(lambda (elem)
                   (gethash (length elem) length-table)))))
