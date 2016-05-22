
;; 99 Lisp Problem
;; http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html

;; 1. Find the last box of a list.
;; Example:
;; * (my-last '(a b c d))
;; (D)

(defun my-last (lst)
  (car (reverse lst)))


;; 2. Find the last but one box of a list.
;; Example:
;; * (my-but-last '(a b c d))
;; (C D)

(defun my-but-last (lst)
  (if (cddr lst)
      (my-but-last (cdr lst))
      lst))


;; 3. Find the K'th element of a list.
;; The first element in the list is number 1.
;; Example:
;; * (element-at '(a b c d e) 3)
;; C

(defun element-at (lst index)
  (if (= index 1)
      (car lst)
      (element-at (cdr lst) (1- index))))


;; 4. Find the number of elements of a list.

(defun my-length (lst)
  (if lst
      (1+ (my-length (cdr lst)))
      0))


;; 5. Reverse a list.

(defun my-reverse (lst)
  (if lst
      (append (my-reverse (cdr lst)) (list (car lst)))
      nil))


;; 6. Find out whether a list is a palindrome.
;; A palindrome can be read forward or backward; e.g. (x a m a x)

(defun palindrome-p (lst)
  (equal lst (my-reverse lst)))


;; 7. Flatten a nested list structure.
;; Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
;; Example:
;; * (my-flatten '(a (b (c d) e)))
;; (A B C D E)
;; Hint: Use the predefined functions list and append.

(defun my-flatten (nested-list)
  (if nested-list
      (append (let ((element (car nested-list)))
                (if (atom element)
                    (list element)
                    (my-flatten element)))
              (my-flatten (cdr nested-list)))
      nil))


;; 8. Eliminate consecutive duplicates of list elements.
;; If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
;; Example:
;; * (compress '(a a a a b c c a a d e e e e))
;; (A B C A D E)

(defun compress (lst)
  (if (cdr lst)                         ;more than 1 elements
      (if (equal (car lst) (cadr lst))
          (compress (cdr lst))
          (cons (car lst) (compress (cdr lst))))
      lst))


;; 9. Pack consecutive duplicates of list elements into sublists.
;; If a list contains repeated elements they should be placed in separate sublists.
;; Example:
;; * (pack '(a a a a b c c a a d e e e e))
;; ((A A A A) (B) (C C) (A A) (D) (E E E E))

(defun pack-split-helper (front-half-reversed back-half)
  (if back-half                ;prevent infinite loop when last item is nil
      (if (equal (car front-half-reversed) (car back-half))
          (pack-split-helper (cons (car back-half) front-half-reversed)
                             (cdr back-half))
          (list front-half-reversed back-half))
      (list front-half-reversed back-half)))

(defun pack-split (lst)
  (let ((splitted (pack-split-helper (list (car lst))
                                     (cdr lst))))
    (list (reverse (first splitted)) (second splitted))))
;; do a reverse here because objects that are equal might not be the same

(defun pack (lst)
  (if lst
      (let ((splitted (pack-split lst)))
        (cons (first splitted)
              (pack (second splitted))))
      nil))


;; 10. Run-length encoding of a list.
;; Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

;; Example:
;; * (encode '(a a a a b c c a a d e e e e))
;; ((4 A) (1 B) (2 C) (2 A) (1 D) (4 E))

(defun encode-helper (packed-lst)
  (if packed-lst
      (cons (list (length (car packed-lst)) (caar packed-lst))
            (encode-helper (cdr packed-lst)))
      nil))

(defun encode (lst)
  (encode-helper (pack lst)))
