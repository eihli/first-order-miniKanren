#lang racket
(require "tools.rkt")

(require "math.rkt")
(require "microk-fo.rkt")

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

(define-relation (layero layer in out)
  (conde
   ((== layer `(Linear ,in ,out)))
   ((== layer `(Relu ,in ,out))
    (== in out))
   ((fresh (percent)
      (== layer `(Dropout ,in ,out ,percent))
      (== in out)
      (countero percent)
      (<o '(1) percent)
      (<o percent '(0 0 1 0 0 1 1))))))

(define-relation (tailo lst tail)
  (fresh (a d rst)
    (conde
     ((== lst `(,a . ,d))
      (disj*
       (== a 1)
       (== a 0))
      (== d `(,tail . ())))
     ((== lst `(,a . ,rst))
      (disj*
       (== a 1)
       (== a 0))
      (tailo rst tail)))))

(define-relation (countero q)
  (conde
   ((== q `(1)))
   ((tailo q 1))))

(define-relation (layerso layers in out)
  (conde
   ((fresh (layer)
           (layero layer in out)
           (== layers `(,layer (OUT ,out)))))
   ((fresh (layer-1 layer-2 hidden)
           (countero hidden)
           (<lo '(1) hidden)
           (layero layer-1 in hidden)
           (layerso layer-2 hidden out)
           (== layers `(,layer-1 . ,layer-2))))))

;; (parse-nums
;;  (run 20 (q)
;;    (full-archo q (build-num 768) (build-num 10))))

;; (explore parallel-step (prune/stream (dnf/stream (query (q) (layerso q '(0 1) '(0 1))))))

;; (stream->choices (prune/stream (dnf/stream (query (q)
;;                                                   (fresh (x y)
;;                                                          (conj*
;;                                                           (disj*
;;                                                            (== x 1)
;;                                                            (== x 5))
;;                                                           (== y 2)
;;                                                           (== q `(,x . ,y))))))))

;; (drop (run 5000 (q)
;;       (layerso q (build-num 2) (build-num 2))) 4980)

;; (run 20 (q) (layerso q '(0 1) '(0 1)))

;; (drop
;;  (run 100 (q)
;;       (archo q (build-num 2) (build-num 2)))
;;  80)

(define (parallel-expand g)
  (let loop ((g g))
    (match g
      ((conj g1 g2)     (conj (loop g1) (loop g2)))
      ((relate thunk _) (thunk))
      (_                g))))

(define (parallel-start st g)
  (match g
    ((disj g1 g2)     (parallel-step (mplus (pause st g1) (pause st g2))))
    ((conj g1 g2)     (parallel-step (bind (pause st g1) g2)))
    ((relate thunk _) (pause st (thunk)))
    ((== t1 t2)       (unify t1 t2 st))))

;; Assumes stream is pruned and dnf.
(define (parallel-step s)
  (match s
    ((mplus s1 s2)
     (let ((s1 (if (mature? s1) s1 (parallel-step s1))))
       (cond ((not s1) s2)
             ((pair? s1) (cons (car s1) (mplus s2 (cdr s1))))
             (else (mplus s2 s1)))))
    ((bind s g)
     (let ((s (if (mature? s) s (parallel-step s))))
       (cond ((not s) #f)
             ((pair? s) (parallel-step (mplus (pause (car s) g)
                                              (bind (cdr s) g))))
             (else (bind s (parallel-expand g))))))
    ((pause st g) (parallel-start st g))
    (_            s)))


;; Assuming you can present choices and something can select a choice... how do you proceed?
;;
;; You `step`. `(step (list-ref choices index))`
(define (ow-explore/stream step stream)
  stream)

;; (define responder-thread
;;   (thread
;;    (lambda ()
;;      (define responder (zmq-socket 'rep))
;;      (zmq-bind responder "tcp://*:5555")
;;      (let loop ((choices (stream->choices (prune/stream (dnf/stream (pause empty-state (fresh (x) (conde ((== x 0)) ((== x 1))))))))))
;;        (define msg (zmq-recv-string responder))
;;        (printf "Server received: ~s\n" msg)
;;        (zmq-send responder "World")
;;        (loop)))))



(let ((stream (pause empty-state (fresh (x)
                                   (== `(,x) initial-var)
                                   (conde ((== x 0)) ((== x 1)))))))
  (prune/stream (dnf/stream stream)))
