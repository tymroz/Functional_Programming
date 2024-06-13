(defun binomial (n k)
  (cond ((or (= k 0) (= k n)) 1)
        (t (+ (binomial (- n 1) (- k 1)) (binomial (- n 1) k)))))

(defun binomial2 (n k)
    (if (or (= k 0) (= k n)) 
        1
    (if (> k n)     
        0
    (let ((pascal (generatePascal (+ n 1))))
        (nth k (nth n pascal))))))

(defun generatePascal (rows)
    (labels ((nextRow (row)
        (append (list 1)
            (mapcar #'+ (butlast row) (rest row))
            (list 1)))
        (iter (n currentRow result)
            (if (= n 0)
            (reverse result)
                (iter (- n 1) (nextRow currentRow) (cons currentRow result)))))
        (iter rows (list 1) nil)))

(defun _merge (xs ys)
    (cond ((null xs) ys)
        ((null ys) xs)
        ((<= (car xs) (car ys)) 
            (cons (car xs) (_merge (cdr xs) ys)))
        (t (cons (car ys) (_merge xs (cdr ys))))))

(defun mergeSort (xs)
    (if (or (null xs) (null (cdr xs)))
        xs
        (let* ((len (length xs))
            (half (floor (/ len 2)))
            (left (subseq xs 0 half))
            (right (subseq xs half)))
            (_merge (mergeSort left) (mergeSort right)))))

(defun extendedGcd (a b)
    (if (zerop b)
        (values a 1 0)
    (multiple-value-bind (g x1 y1) (extendedGcd b (mod a b))
    (values g y1 (- x1 (* (floor a b) y1))))))

(defun de (a b)
    (multiple-value-bind (g x y) (extendedGcd a b)
    (list g x y)))

(defun factorize (d n)
    (if (> (* d d) n)
        (list n)
    (if (zerop (mod n d))
        (cons d (factorize d (/ n d)))
        (factorize (+ d 1) n))))

(defun primeFactors (n)
    (if (<= n 1)
        nil
    (factorize 2 n)))

(defun totient (n)
    (labels ((countCoprimes (k result)
        (cond ((= k n) result)
              ((= (gcd n k) 1) (countCoprimes (+ k 1) (+ result 1)))
              (t (countCoprimes (+ k 1) result)))))
    (countCoprimes 1 0)))

(defun removeDuplicates (factors)
    (remove-duplicates factors :test #'=))

(defun totient2 (n)
    (if (<= n 1)
        0
    (let ((factors (removeDuplicates(primeFactors n))))
        (round (* n (reduce #'* (mapcar (lambda (p) (- 1 (/ 1.0 p))) factors)))))))

(defun isPrime (n)
    (cond ((< n 2) nil) 
        ((= n 2) t)   
        ((evenp n) nil)         
        (t
        (labels ((primeP (d)
                    (or (> d (isqrt n))
                        (and (not (zerop (mod n d)))
                             (primeP (+ d 2))))))
           (primeP 3)))))

(defun range (start end)
    (if (< start end)
        nil
    (cons start (range (1- start) end))))

(defun sieve (n)
    (remove-if-not #'isPrime (range n 1)))

(print (binomial 10 3))
(print (binomial2 10 3))
(print (mergeSort '(5 3 8 6 2 7 4 1)))
(print (de 56 15))
(print (primeFactors 50))
(print (sieve 50))
(print (totient 100))
(print (totient2 100))