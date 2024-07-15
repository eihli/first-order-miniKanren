#lang racket/base

(provide run)

(require (prefix-in dispatch: web-server/dispatch)
         (prefix-in dispatch-log: web-server/dispatchers/dispatch-log)
         (prefix-in jsexpr: web-server/http/json)
         (prefix-in xexpr: web-server/http/xexpr)
         (prefix-in rqstruct: web-server/http/request-structs)
         (prefix-in servlet: web-server/servlet-env)
         (only-in racket/match match)
         (only-in racket/list takef dropf)
         (only-in racket/port with-output-to-string)
         json
         "nas.rkt"
         "tools.rkt"
         "microk-fo.rkt"
         (rename-in "mk-syntax.rkt" (run mk/run))
         racket/serialize)

(define (not-found-route request)
  (xexpr:response/xexpr
   `(html (body (h2 "Uh-oh! Page not found.")))))

(define (home-route request)
  (xexpr:response/xexpr
   `(html (body (h2 "Look ma, no state!!!!!!!!!")))))

(define-relation (appendo a b ab)
  (conde
   ((== a '()) (== b ab))
   ((fresh (a1 a2 res)
      (== a `(,a1 . ,a2))
      (appendo a2 b res)
      (== ab `(,a1 . ,res))))))

(define (serializable-choices choices)
  (cond
    ((null? choices) '())
    ((pair? choices) `(,(serializable-choices (car choices)) . ,(serializable-choices (cdr choices))))
    (else (match choices
            ((relate th de) (relate 'procedure (cdr de)))
            ((pause st gl) (pause st (serializable-choices gl)))
            ((conj g1 g2) (conj (serializable-choices g1) (serializable-choices g2)))
            ((disj g1 g2) (disj (serializable-choices g1) (serializable-choices g2)))
            (choices choices)))))

(define (initialize request)
  (let ((choices
         (stream->choices
          (prune/stream
           (dnf/stream
            (pause
             empty-state
             (fresh (a b)
                    (== `(,a ,b) initial-var)
                    (appendo a b `(1 2 3 4)))))))))
    (jsexpr:response/jsexpr (hash 'choices (with-output-to-string (lambda ()
                                                                    (write (serialize (serializable-choices choices)))))))))

(define (step request)
  (let ((choices
         (stream->choices
          (prune/stream
           (dnf/stream
            (pause
             empty-state
             (fresh (a b)
                    (== `(,a ,b) initial-var)
                    (appendo a b `(1 2 3 4)))))))))
    (jsexpr:response/jsexpr (hash 'choices (with-output-to-string (lambda ()
                                                                    (write (serialize (serializable-choices choices)))))))))


(let ((choices
       (stream->choices
        (prune/stream
         (dnf/stream
          (pause
           empty-state
           (fresh (a b)
                  (== `(,a ,b) initial-var)
                  (appendo a b `(1 2 3 4)))))))))
  (list
   (serialize (serializable-choices choices))
   (jsexpr? (hash 'choices (with-output-to-string (lambda ()
                                                    (write (serialize (serializable-choices choices)))))))))

(define-values (route-dispatch route-url)
  (dispatch:dispatch-rules
   [("")                           home-route]
   [("initialize") #:method "post" initialize]
   [("step")       #:method "post" step]
   [else                           not-found-route]))

(define (route-dispatch/log-middleware req)
  (display (dispatch-log:apache-default-format req))
  (flush-output)
  (route-dispatch req))

(define (run)
  (servlet:serve/servlet
   route-dispatch/log-middleware
   #:servlet-path "/"
   #:servlet-regexp #rx""
   #:stateless? #t))

(run)
