
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

(defun euler-phi (n)
  (reduce #'(lambda (phi pair) (let ((p (car pair)))
                                 (* (/ phi p) (1- p))))
          (prime-factors-mult n) :initial-value n))


;; 38. Compare the two methods of calculating Euler's totient function.
;; Use the solutions of problems P34 and P37 to compare the algorithms. Take the number of logical inferences as a measure for efficiency. Try to calculate phi(10090) as an example.


;; 39. A list of prime numbers.
;; Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

;; borrowed from rosetta code
(defun prime-sieve (upper-limit)
  (cons 2                               ;build from odd only numbers
        (loop
           with upper = (ash (1- upper-limit) -1)
           with is-prime-arr = (make-array (1+ upper)
                                           :element-type 'bit
                                           :initial-element 0)
           ;; index maps to number (1+ (* 2 index))
           ;; 0 means is-prime
           with stop = (ash (isqrt upper-limit) -1)

           for i from 1 to upper
           when (zerop (sbit is-prime-arr i))
           collect (1+ (ash i 1))
           and when (<= i stop) do
             (loop for j from (ash (* i (1+ i)) 1) to upper by (1+ (ash i 1))
                do (setf (sbit is-prime-arr j) 1)))))


;; 40.Goldbach's conjecture.
;; Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.

;; Example:
;; * (goldbach 28)
;; (5 23)

(defun goldbach (n)
  (if (and (evenp n) (> n 2))
      (loop for i from 2 to (ash n -1)
         when (and (is-prime i)
                   (is-prime (- n i)))
         return (list i (- n i)))
      nil))


;; 41. A list of Goldbach compositions.
;; Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
;; Example:
;; * (goldbach-list 9 20)
;; 10 = 3 + 7
;; 12 = 5 + 7
;; 14 = 3 + 11
;; 16 = 3 + 13
;; 18 = 5 + 13
;; 20 = 3 + 17

;; In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50.

(defun goldbach-list (lower upper)
  (loop for i from lower to upper
     when (evenp i)
     collect (goldbach i)))
