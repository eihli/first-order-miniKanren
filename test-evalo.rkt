#lang racket
(require "tools.rkt")
(require "math.rkt")

(map
 (compose unbuild-num car)
 (run 1 (q)
      (*o (build-num 2) (build-num 2) q)))

;; Find a function that squares its argument.
(run 5 (p)
     (fresh (body)
            (== p `(lambda ,body))
            (eval-expo `(app ,p ,(make-num 3)) '() (make-num 9))
            (eval-expo `(app ,p ,(make-num 4)) '() (make-num 16))))

;; Find functions that double their argument.
(run 5 (p)
     (fresh (body)
            (== p `(lambda ,body))
            (eval-expo `(app ,p ,(make-num 3)) '() (make-num 6))
            (eval-expo `(app ,p ,(make-num 4)) '() (make-num 8))))


;; Find a function that doubles the first element in a list.
(run 1 (p)
     (fresh (l1 l2 l3 l4 body)
            (== l1 `(list ,(make-num 1) ,(make-num 5)))
            (== l2 `(list ,(make-num 2)))
            (== l3 `(list ,(make-num 2)))
            (== l4 `(list ,(make-num 4)))
            (== p `(lambda ,body))
            (eval-expo `(app ,p ,l1) '() l2)
            (eval-expo `(app ,p ,l3) '() l4)))

;; Run a function that doubles the first element in a list.
(run 5 (q)
     (fresh ()
            (eval-expo
             `(app
               (lambda (list (car (cdr (lambda list))) (*o (number 0 1) (car (var ())))))
               (list ,(make-num 7)))
             '()
             q)))

;; Find a function that doubles each element in a list.
(run 1 (p)
     (fresh (l1 l2 l3 l4 body)
            (== l1 `(list ,(make-num 1) ,(make-num 2) ,(make-num 3)))
            (== l2 `(list ,(make-num 2) ,(make-num 4) ,(make-num 6)))
            (== l3 `(list ,(make-num 1) ,(make-num 2)))
            (== l4 `(list ,(make-num 2) ,(make-num 4)))
            (== p `(lambda ,body))
            (eval-expo `(app ,p ,l1) '() l2)
            (eval-expo `(app ,p ,l3) '() l4)))

;; (run 1 (p)
;;      (fresh (n m v pv)
;;             (== p `(*o ,n ,m ,pv))
;;             (== n `(var ()))
;;             (eval-expo `(*o ,n ,m ,v) `(,(make-num 4)) (make-num 16)))
;;      (fresh (n m v pv)
;;             (== p `(*o ,n ,m ,pv))
;;             (== n `(var ()))
;;             (eval-expo `(*o ,n ,m ,v) `(,(make-num 6)) (make-num 36)))
;;      )

;; (run 1 (p)
;;      (eval-expo p
;;                 `((,(make-num 2) ,(make-num 3) ,(make-num 4)))
;;                 `((,(make-num 4) ,(make-num 6) ,(make-num 8))))
;;      (eval-expo p
;;                 `((,(make-num 1) ,(make-num 2) ,(make-num 3)))
;;                 `((,(make-num 2) ,(make-num 4) ,(make-num 6))))
;;      )
