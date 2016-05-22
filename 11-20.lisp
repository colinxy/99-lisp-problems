
(load "1-10.lisp")

;; 11. Modified run-length encoding.
;; Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

;; Example:
;; * (encode-modified '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))

(defun encode-modified-helper (packed-lst)
  (if packed-lst
      (cons (let* ((first-lst (car packed-lst))
                   (first-length (length first-lst)))
              (if (= 1 first-length)
                  (car first-lst)
                  (list first-length (car first-lst))))
            (encode-modified-helper (cdr packed-lst)))
      nil))

(defun encode-modified (lst)
  (encode-modified-helper (pack lst)))


;; 12. Decode a run-length encoded list.
;; Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.

(defun decode (encoded-lst)
  (if encoded-lst
      (append (let ((first-item (car encoded-lst)))
                (if (listp first-item)
                    (make-list (first first-item)
                               :initial-element (second first-item))
                    (list first-item)))
              (decode (cdr encoded-lst)))
      nil))


;; 13. Run-length encoding of a list (direct solution).
;; Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

;; Example:
;; * (encode-direct '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))

(defun encode-direct-helper (encoded rest)
  (let ((number (first encoded))
        (element (second encoded)))
    (if (and rest
             (equal element (car rest)))
        (encode-direct-helper (list (1+ number) element)
                              (cdr rest))
        (if (= 1 number)
            (list element rest)
            (list encoded rest)))))

(defun encode-direct (lst)
  (if lst
      (let ((splitted (encode-direct-helper (list 1 (car lst))
                                            (cdr lst))))
        (cons (first splitted)
              (encode-direct (second splitted))))
      nil))


;; 14. Duplicate the elements of a list.

;; Example:
;; * (dupli '(a b c c d))
;; (A A B B C C C C D D)

(defun dupli (lst)
  (if lst
      (let ((first-elem (car lst)))
        (cons first-elem
              (cons first-elem
                    (dupli (cdr lst)))))
      nil))


;; 15. Replicate the elements of a list a given number of times.

;; Example:
;; * (repli '(a b c) 3)
;; (A A A B B B C C C)

(defun repli (lst number)
  (if lst
      (let ((first-elem (car lst)))
        (append (make-list number :initial-element first-elem)
                (repli (cdr lst) number)))
      nil))


;; 16. Drop every N'th element from a list.

;; Example:
;; * (drop '(a b c d e f g h i k) 3)
;; (A B D E G H K)

(defun drop-helper (lst curr nth)
  (if lst
      (if (= 1 curr)
          (drop-helper (cdr lst) nth nth)
          (cons (car lst)
                (drop-helper (cdr lst) (1- curr) nth)))
      nil))

(defun drop (lst nth)
  (drop-helper lst nth nth))


;; 17. Split a list into two parts; the length of the first part is given.
;; Do not use any predefined predicates.

;; Example:
;; * (split '(a b c d e f g h i k) 3)
;; ( (A B C) (D E F G H I K))

(defun split-helper (first-half-reversed second-half nth)
  (if second-half
      (if (= 0 nth)
          (list first-half-reversed second-half)
          (split-helper (cons (car second-half)
                              first-half-reversed)
                        (cdr second-half)
                        (1- nth)))
      (list first-half-reversed second-half)))

(defun split (lst nth)
  (let* ((splitted (split-helper nil lst nth))
         (first-half-reversed (first splitted))
         (second-half        (second splitted)))
    (list (reverse first-half-reversed) second-half)))


;; 18. Extract a slice from a list.
;; Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.

;; Example:
;; * (slice '(a b c d e f g h i k) 3 7)
;; (C D E F G)

(defun from-nth (lst index)
  (if (and lst
           (> index 1))
      (from-nth (cdr lst) (1- index))
      lst))

(defun slice-nth (lst nth)
  (if (and lst
           (>= nth 0))
      (cons (car lst)
            (slice-nth (cdr lst) (1- nth)))
      nil))

(defun slice (lst begin end)
  "Indexes are 1 based."
  (slice-nth (from-nth lst begin) (- end
                                     (if (< begin 1) 1 begin))))


;; 19. Rotate a list N places to the left.
;; Examples:
;; * (rotate '(a b c d e f g h) 3)
;; (D E F G H A B C)

;; * (rotate '(a b c d e f g h) -2)
;; (G H A B C D E F)

(defun rotate (lst n-places)
  (let* ((n (if (< n-places 0)
                (+ (length lst) n-places)
                n-places))
         (splitted (split lst n))
         (front-half (first splitted))
         (back-half (second splitted)))
    (append back-half front-half)))


;; 20. Remove the K'th element from a list.
;; Example:
;; * (remove-at '(a b c d) 2)
;; (A C D)

(defun remove-at (lst nth)
  (if (and lst
           (>= nth 1))
      (if (= 1 nth)
          (cdr lst)
          (cons (car lst)
                (remove-at (cdr lst)
                           (1- nth))))
      lst))
