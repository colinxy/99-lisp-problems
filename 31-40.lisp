
(load "1-10.lisp")

;; 31. Determine whether a given integer number is prime.

;; Example:
;; * (is-prime 7)
;; T

;; borrowed from rosettacode
(defun is-prime (n)
  (and (> n 1)
       (or (= 2 n) (oddp n))
       (loop for i from 3 to (isqrt n) by 2
            never (zerop (rem n i)))))


;; 32. Determine the greatest common divisor of two positive integer numbers.
;; Use Euclid's algorithm.
;; Example:
;; * (gcd 36 63)
;; 9

(defun gcd* (m n)
  (if (zerop n)
      m
      (gcd* n (rem m n))))


;; 33. Determine whether two positive integer numbers are coprime.
;; Two numbers are coprime if their greatest common divisor equals 1.

;; Example:
;; * (coprime 35 64)
;; T

(defun coprime (m n)
  (= 1 (gcd m n)))


;; 34. Calculate Euler's totient function phi(m).
;; Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
;; Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

;; * (totient-phi 10)
;; 4

;; Find out what the value of phi(m) is if m is a prime number. Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA). In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later).

(defun totient-phi (n)
  (loop for i from 1 to (1- n)
     count (coprime n i)))


;; 35. Determine the prime factors of a given positive integer.
;; Construct a flat list containing the prime factors in ascending order.

;; Example:
;; * (prime-factors 315)
;; (3 3 5 7)

(defun prime-factors (n)
  (loop with i = 2
     while (>= n (* i i))

     if (zerop (rem n i))
     collect i into factors
     and do (setf n (floor n i))

     else do (incf i)
     finally
       (return (if (= 1 n)
                   factors
                   (append factors (list n))))))


;; 36. Determine the prime factors of a given positive integer (2).
;; Construct a list containing the prime factors and their multiplicity.

;; Example:
;; * (prime-factors-mult 315)
;; ((3 2) (5 1) (7 1))

(defun prime-factors-mult (n)
  (mapcar #'reverse (encode (prime-factors n)))) ;encode from 1-10.lisp


;; 37. Calculate Euler's totient function phi(m) (improved).
;; See problem P34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem P36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
;; phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) + (p3 - 1) * p3 ** (m3 - 1) + ...

;; Note that a ** b stands for the b'th power of a.
