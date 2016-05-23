
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
  )
