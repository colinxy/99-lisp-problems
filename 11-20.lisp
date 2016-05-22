
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

(defun encode-direct (lst)
  )
