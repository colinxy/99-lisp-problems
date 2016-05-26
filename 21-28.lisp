
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

;; lst heterogeneous
(defun group3 (lst)
  "Group into (2 3 4)."
  (if (= 9 (length lst))
      (mapcar )
      nil))

(defun group (lst groups)
  (if (= (apply #'+ groups)
         (length lst))
      ()
      nil))
