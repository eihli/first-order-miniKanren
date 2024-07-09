#lang racket
(require "tools.rkt")
(require "math.rkt")

(define build-num
  (lambda (n)
    (cond
      ((odd? n)
       (cons 1
         (build-num (quotient (- n 1) 2))))
      ((and (not (zero? n)) (even? n))
       (cons 0
         (build-num (quotient n 2))))
      ((zero? n) '()))))

(define (one? n)
  (= n 1))

(define (unbuild-num n)
  (let loop ((n n) (r 0) (i 0))
    (cond
     ((null? n) r)
     ((one? (car n)) (loop (cdr n) (+ r (expt 2 i)) (add1 i)))
     (else (loop (cdr n) r (add1 i))))))

(define parse-nums
  (lambda (layers)
    (cond
     ((null? layers) '())
     ((symbol? (car layers))
      (cons (car layers) (map unbuild-num (cdr layers))))
     ((pair? layers)
      (cons (parse-nums (car layers)) (parse-nums (cdr layers))))
     (else layers))))

(define linearo
  (lambda (q in out)
    (== q `(Linear ,in ,out))))

(define percento
  (lambda (x)
    (fresh (y)
      (pluso x y (build-num 99)))))

(define dropouto
  (lambda (q in out)
    (fresh (p)
      (percento p)
      (== in out)
      (== q `(Dropout ,in ,out ,p)))))

(define reluo
  (lambda (q in out)
    (conde
     ((== in out)
      (== q `(Relu ,in ,out))))))

(define layero
  (lambda (q in out)
    (conde
     ((linearo q in out))
     ((dropouto q in out))
     ((reluo q in out)))))

(define-relation (countero q i)
  (fresh (k)
    (conde
     ((== q i))
     ((pluso i '(1) k)
      (countero q k)))))

(define-relation (archo arch first-in last-out)
  (conde
   ((fresh (layer)
           (layero layer first-in last-out)
           (== arch `(,layer))))
   ((fresh (head tail hidden)
           (countero hidden (build-num 8))
           (layero head first-in hidden)
           (archo tail hidden last-out)
           (== arch `(,head . ,tail))))))

(define conso
  (lambda (head tail result)
    (== result `(,head . ,tail))))

(define-relation (appendo ab c abc)
  (conde
    ((== '() ab) (== c abc))
    ((fresh (a b bc)
       (== `(,a . ,b) ab)
       (== `(,a . ,bc) abc)
       (appendo b c bc)))))

(define-relation (full-archo arch first-in last-out)
  (fresh (layers)
         (fresh (tmp)
                (appendo `((IN ,first-in)) layers tmp)
                (appendo tmp `((OUT ,last-out)) arch))
         (archo layers first-in last-out)))

;; (parse-nums
;;  (run 20 (q)
;;    (full-archo q (build-num 768) (build-num 10))))
(run 20 (q)
     (full-archo q (build-num 10) (build-num 2)))
