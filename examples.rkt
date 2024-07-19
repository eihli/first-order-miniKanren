#lang racket
(provide (all-defined-out) (all-from-out "tools.rkt" racket/pretty))
(require "tools.rkt" racket/pretty)
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)
(define-syntax example
  (syntax-rules ()
    ((_ e) (begin (newline)
                  (pretty-print 'e)
                  (displayln "==>")
                  (pretty-print e)))))
(define-syntax examples
  (syntax-rules ()
    ((_ e ...) (begin (example e) ...))))

(require "common.rkt")
(require "mk-fo.rkt")
(require "microk-fo.rkt")
(require "tools.rkt")

(include "mk-syntax.rkt")

(define-relation (bit-xoro x y z)
  (conde
   ((== x 0) (== y 0) (== z 0))
   ((== x 0) (== y 1) (== z 1))
   ((== x 1) (== y 0) (== z 1))
   ((== x 1) (== y 1) (== z 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples
(define-relation (appendo ab c abc)
  (conde
   ((== '() ab) (== c abc))
   ((fresh (a b bc)
           (== `(,a . ,b) ab)
           (== `(,a . ,bc) abc)
           (appendo b c bc)))))

;;;; Works fine
;; (explore step (query (q) (appendo q '(2) '(1 2))))

(explore parallel-step (query  (a b) (appendo a b '(1 2 3 4))))
;;;; Errors with the following:
;;
;; Using step procedure: parallel-step
;; Exploring query:
;; (query (a b) (appendo a b (quote (1 2 3 4))))

;; ================================================================================
;; Current Depth: 0
;; Number of Choices: 1

;; | Choice 1:
;; |  a = #s(var a 3)
;; |  b = #s(var b 4)
;; |  Constraints:
;; |  * (appendo #s(var a 3) #s(var b 4) (1 2 3 4))

;; [h]elp, [u]ndo, or choice number>
;; 1

;; ================================================================================
;; Previous Choice:
;; |  a = #s(var a 3)
;; |  b = #s(var b 4)
;; |  Constraints:
;; |  * (appendo #s(var a 3) #s(var b 4) (1 2 3 4))

;; Current Depth: 1
;; Number of Choices: 2

;; | Choice 1:
;; match: no matching clause for #s(bind #s(state ((#s(var #f 0) #s(var a 3) #s(var b 4))) () () ()) #s(conj #s(== () #s(var a 3)) #s(== #s(var b 4) (1 2 3 4))))


(examples
 (run* (q) (appendo '(1 2 3) '(4 5) q))
 (run* (p q) (appendo p q '(1 2 3 4 5)))
 ;; We can change our search strategy by choosing a different way to step.
 ;; `step` gives us a typical interleaving search.
 (run*/step step (p q) (appendo p q '(1 2 3 4 5))))


(define-relation (reverseo ys sy)
  (conde
    ((== '() ys) (== '() sy))
    ((fresh (first rest prefix)
       (== `(,first . ,rest) ys)
       ;; With a typical search strategy, there is no refutationally complete
       ;; ordering of the following two goals.  This ordering works well when
       ;; running in the forward direction, but not in the backward direction.
       (reverseo rest prefix)
       (appendo prefix `(,first) sy)))))

(examples
  (run*/step step (q) (reverseo '(1 2 3) q))
  ;; Asking for more than one answer will fail to terminate.
  (run/step step 1 (q) (reverseo q '(3 2 1)))
  ;; Using a refutationally complete search strategy solves the problem.
  (run*/step parallel-step (q) (reverseo q '(3 2 1))))

(displayln
"We can explore the backwards reverseo query to observe its bad behavior.")
(displayln "Stop exploring by triggering EOF (probably Ctrl-D).")
(explore step (query (q) (reverseo q '(2 1))))

;; A miniscule relational interpreter.
(define-relation (evalo expr env value)
  (conde
    ((== `(quote ,value) expr))

    ((fresh (index)
       (== `(var ,index) expr)
       (lookupo index env value)))

    ((fresh (a d va vd)
       (== `(cons ,a ,d) expr)
       (== `(,va . ,vd) value)
       (evalo a env va)
       (evalo d env vd)))))

;; Variables are represented namelessly using relative De Bruijn indices.
;; These indices are encoded as peano numerals: (), (s), (s s), etc.
(define-relation (lookupo index env value)
  (fresh (arg e*)
    (== `(,arg . ,e*) env)
    (conde
      ((== '() index) (== arg value))
      ((fresh (i* a d)
         (== `(s . ,i*) index)
         (== `(,a . ,d) e*)
         (lookupo i* e* value))))))

(displayln
"We can solve simple programming-by-example (PBE) problems by running our
interpreter backwards on multiple examples.  Here, we'll synthesize a program
that repeats the input three times using two examples.

But with a typical search strategy, the constraints in the second example
remain dormant until the first example is fully satisfied.  This delayed
feedback means we may deeply explore a hopeless program even when there is
shallow evidence that it will fail the second example.  For example, we may
uselessly consider several programs of the form (cons (quote 0) _) when no such
program could ever succeed on the second example.")
(displayln "Stop exploring by triggering EOF (probably Ctrl-D).")
(explore step
  (query (program)
    (evalo program '(0) '(0 0 0))
    (evalo program '(1) '(1 1 1))))

(displayln
"Using parallel-step allows us to expand constraints from both examples
simultaneously, pruning impossibilities much sooner.")
(displayln "Stop exploring by triggering EOF (probably Ctrl-D).")
(explore parallel-step
  (query (program)
    (evalo program '(0) '(0 0 0))
    (evalo program '(1) '(1 1 1))))
