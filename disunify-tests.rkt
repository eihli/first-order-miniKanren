(display "\nRunning =/= tests")
(newline)

(test 'diseq-0
  (run* (q) (=/= 101 5))
  '((_.0)))

(test 'diseq-1
  (run* (q) (=/= 5 q))
  '(#s(Ans (_.0) ((=/= ((_.0 5)))))))

(test 'diseq-2
  (run* (q) (=/= q 5))
  '(#s(Ans (_.0) ((=/= ((_.0 5)))))))

(test 'diseq-3
  (run* (p q) (=/= p q))
  '(#s(Ans (_.0 _.1) ((=/= ((_.0 _.1)))))))

(test 'diseq-4
  (run* (q) (=/= q 12) (=/= q 18)) 
  '(#s(Ans (_.0) ((=/= ((_.0 12)) ((_.0 18)))))))

(test 'diseq-5
  (run* (p r s) (=/= (list p p) (list r s)))
  '(#s(Ans (_.0 _.1 _.2) ((=/= ((_.0 _.2) (_.1 _.2)))))))

(test 'diseq-6
  (run* (q) (=/= q 'hello))
  '(#s(Ans (_.0) ((=/= ((_.0 hello)))))))

(test 'diseq-7
  (run* (q) (=/= q "world"))
  '(#s(Ans (_.0) ((=/= ((_.0 "world")))))))

(test 'equality-diseq-0
  (run* (q) (== q 29) (=/= q 17))
  '((29)))

(test 'equality-diseq-1
  (run* (q) (=/= q 17) (== q 29))
  '((29)))

(test 'equality-diseq-2
  (run* (x) (== x 5) (=/= 1 20))
  '((5)))

(test 'equality-diseq-3
  (run* (p q) (== p 3) (== q 4) (=/= p q))
  '((3 4)))

(test 'equality-diseq-4
  (run* (p q) (=/= p q) (== p 3) (== q 4))
  '((3 4)))

(test 'equality-diseq-5
  (run* (x) (== x 13) (=/= (list x 11) (list 13 12)))
  '((13)))

(test 'equality-diseq-6
  (run* (x) (=/= (list x 11) (list 13 12)) (== x 13))
  '((13)))

(test 'diseq-with-conde-0
  (run* (q) (conde ((=/= q 77)) ((=/= q 54))))
  '(#s(Ans (_.0) ((=/= ((_.0 77))))) #s(Ans (_.0) ((=/= ((_.0 54)))))))

(test 'diseq-with-conde-1
  (run* (x) (== x 5) (conde ((=/= x 5)) ((=/= 1 20))))
  '((5)))

(test 'diseq-fail-0
  (run* (q) (=/= 1 1))
  '())

(test 'diseq-fail-1
  (run* (q) (=/= 3 q) (== q 3))
  '())

(test 'diseq-fail-2
  (run* (p q) (== p 1) (== q 2) (=/= (cons p q) (cons 1 2)))
  '())

(test 'diseq-fail-3
  (run* (p q) (=/= (cons p q) (cons 1 2)) (== p 1) (== q 2))
  '())

(test 'diseq-fail-4
  (run* (p q) (=/= p 1) (=/= q 2) (== (cons p q) (cons 1 2)))
  '())

(test 'diseq-fail-5
  (run* (p q) (== (cons p q) (cons 1 2)) (=/= p 1) (=/= q 2))
  '())

(test 'diseq-fail-6
  (run* (p q r) (=/= p r) (== p q) (== q r))
  '())

(test 'diseq-fresh-test-0
  (run* (a b) (fresh (c) (== a b) (=/= c (cons a b))))
  '((_.0 _.0)))

(test 'diseq-fresh-test-1
  (run* (a b) (fresh (c) (== a b) (=/= a (cons c 5))))
  '((_.0 _.0)))

(test 'diseq-fresh-test-2
  (run* (a b) (fresh (c) (=/= (cons c 5) (cons a b))))
  '((_.0 _.1)))

(test 'diseq-fresh-test-3
  (run* (a b) (fresh (c) (=/= b c) (== c a)))
  '(#s(Ans (_.0 _.1) ((=/= ((_.0 _.1)))))))

(test 'diseq-pair-test-0
  (run* (a b c) (== a b) (=/= a (cons c 5)))
  '(#s(Ans (_.0 _.0 _.1) ((=/= ((_.0 (_.1 . 5))))))))

;; Faster-miniKanren tests

(test "=/=-0"
  (run* (q) (=/= 5 q))
  '(#s(Ans (_.0) ((=/= ((_.0 5)))))))

(test "=/=-1"
  (run* (q)
    (=/= 3 q)
    (== q 3))
  '())

(test "=/=-2"
  (run* (q)
    (== q 3)
    (=/= 3 q))
  '())

(test "=/=-3"
  (run* (q)
    (fresh (x y)
      (=/= x y)
      (== x y)))
  '())

(test "=/=-4"
  (run* (q)
    (fresh (x y)
      (== x y)
      (=/= x y)))
  '())

(test "=/=-5"
  (run* (q)
    (fresh (x y)
      (=/= x y)
      (== 3 x)
      (== 3 y)))
  '())

(test "=/=-6"
  (run* (q)
    (fresh (x y)
      (== 3 x)
      (=/= x y)
      (== 3 y)))
  '())

(test "=/=-7"
  (run* (q)
    (fresh (x y)
      (== 3 x)
      (== 3 y)
      (=/= x y)))
  '())

(test "=/=-8"
  (run* (q)
    (fresh (x y)
      (== 3 x)
      (== 3 y)
      (=/= y x)))
  '())

(test "=/=-9"
  (run* (q)
    (fresh (x y z)
      (== x y)
      (== y z)
      (=/= x 4)
      (== z (+ 2 2))))
  '())

(test "=/=-10"
  (run* (q)
    (fresh (x y z)
      (== x y)
      (== y z)
      (== z (+ 2 2))
      (=/= x 4)))
  '())

(test "=/=-11"
  (run* (q)
    (fresh (x y z)
      (=/= x 4)
      (== y z)
      (== x y)
      (== z (+ 2 2))))
  '())

(test "=/=-12"
  (run* (q)
    (fresh (x y z)
      (=/= x y)
      (== x `(0 ,z 1))
      (== y `(0 1 1))))
  '((_.0)))

(test "=/=-13"
  (run* (q)
    (fresh (x y z)
      (=/= x y)
      (== x `(0 ,z 1))
      (== y `(0 1 1))
      (== z 1)
      (== `(,x ,y) q)))
  '())

(test "=/=-14"
  (run* (q)
    (fresh (x y z)
      (=/= x y)
      (== x `(0 ,z 1))
      (== y `(0 1 1))
      (== z 0)))
  '((_.0)))

(test "=/=-15"
  (run* (q)
    (fresh (x y z)
      (== z 0)
      (=/= x y)
      (== x `(0 ,z 1))
      (== y `(0 1 1))))
  '((_.0)))

(test "=/=-16"
  (run* (q)
    (fresh (x y z)
      (== x `(0 ,z 1))
      (== y `(0 1 1))
      (=/= x y)))
  '((_.0)))

(test "=/=-17"
  (run* (q)
    (fresh (x y z)
      (== z 1)
      (=/= x y)
      (== x `(0 ,z 1))
      (== y `(0 1 1))))
  '())

(test "=/=-18"
  (run* (q)
    (fresh (x y z)
      (== z 1)
      (== x `(0 ,z 1))
      (== y `(0 1 1))
      (=/= x y)))
  '())

(test "=/=-19"
  (run* (q)
    (fresh (x y)
      (=/= `(,x 1) `(2 ,y))
      (== x 2)))
  '((_.0)))

(test "=/=-20"
  (run* (q)
    (fresh (x y)
      (=/= `(,x 1) `(2 ,y))
      (== y 1)))
  '((_.0)))

(test "=/=-21"
  (run* (q)
    (fresh (x y)
      (=/= `(,x 1) `(2 ,y))
      (== x 2)
      (== y 1)))
  '())

(test "=/=-22"
  (run* (q)
    (fresh (x y)
      (=/= `(,x 1) `(2 ,y))
      (== `(,x ,y) q)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 2) (_.1 1)))))))

(test "=/=-23"
  (run* (q)
    (fresh (x y)
      (=/= `(,x 1) `(2 ,y))
      (== x 2)
      (== `(,x ,y) q)))
  '(#s(Ans ((2 _.0)) ((=/= ((_.0 1)))))))

(test "=/=-24"
  (run* (q)
    (fresh (x y)
      (=/= `(,x 1) `(2 ,y))
      (== x 2)
      (== y 9)
      (== `(,x ,y) q)))
  '(((2 9))))

(test "=/=-24b"
  (run* (q)
  (fresh (a d)
    (== `(,a . ,d) q)
    (=/= q `(5 . 6))
    (== a 5)
    (== d 6)))
  '())

(test "=/=-25"
  (run* (q)
    (fresh (x y)
      (=/= `(,x 1) `(2 ,y))
      (== x 2)
      (== y 1)
      (== `(,x ,y) q)))
  '())

(test "=/=-26"
  (run* (q)
    (fresh (a x z)
      (=/= a `(,x 1))
      (== a `(,z 1))
      (== x z)))
  '())

(test "=/=-27"
  (run* (q)
    (fresh (a x z)
      (=/= a `(,x 1))
      (== a `(,z 1))
      (== x 5)
      (== `(,x ,z) q)))
  '(#s(Ans ((5 _.0)) ((=/= ((_.0 5)))))))

(test "=/=-28"
  (run* (q)
    (=/= 3 4))
  '((_.0)))

(test "=/=-29"
  (run* (q)
    (=/= 3 3))
  '())

(test "=/=-30"
  (run* (q) (=/= 5 q)
	    (=/= 6 q)
	    (== q 5))
  '())

(test "=/=-31"
  (run* (q)
  (fresh (a d)
    (== `(,a . ,d) q)
    (=/= q `(5 . 6))
    (== a 5)))
  '(#s(Ans ((5 . _.0)) ((=/= ((_.0 6)))))))

(test "=/=-32"
  (run* (q)
    (fresh (a)
      (== 3 a)
      (=/= a 4)))
  '((_.0)))

(test "=/=-33"
  (run* (q)
    (=/= 4 q)
    (=/= 3 q))
  '(#s(Ans (_.0) ((=/= ((_.0 4)) ((_.0 3)))))))

(test "=/=-34"
  (run* (q) (=/= q 5) (=/= q 5))
  '(#s(Ans (_.0) ((=/= ((_.0 5)))))))

(test "=/=-35"
  (let ((foo (lambda (x)
               (fresh (a)
                 (=/= x a)))))
    (run* (q) (fresh (a) (foo a))))
  '((_.0)))

(test "=/=-36"
  (let ((foo (lambda (x)
               (fresh (a)
                 (=/= x a)))))
    (run* (q) (fresh (b) (foo b))))
  '((_.0)))

(test "=/=-37"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= x y)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 _.1)))))))

(test "=/=-37b"
  (run* (q)
  (fresh (a d)
    (== `(,a . ,d) q)
    (=/= q `(5 . 6))))
  '(#s(Ans ((_.0 . _.1)) ((=/= ((_.0 5) (_.1 6)))))))

(test "=/=-37c"
  (run* (q)
  (fresh (a d)
    (== `(,a . ,d) q)
    (=/= q `(5 . 6))
    (== a 3)))
  '(((3 . _.0))))

(test "=/=-38"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= y x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 _.1)))))))

(test "=/=-39"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= x y)
      (=/= y x)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 _.1)))))))

(test "=/=-40"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= x y)
      (=/= x y)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 _.1)))))))

(test "=/=-41"
  (run* (q) (=/= q 5) (=/= 5 q))
  '(#s(Ans (_.0) ((=/= ((_.0 5)))))))

(test "=/=-42"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= `(,x ,y) `(5 6))
      (=/= x 5)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 5)))))))

(test "=/=-43"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= x 5)
      (=/= `(,x ,y) `(5 6))))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 5)))))))

(test "=/=-44"
  (run* (q)
    (fresh (x y)
      (=/= x 5)
      (=/= `(,x ,y) `(5 6))
      (== `(,x ,y) q)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 5)))))))

(test "=/=-45"
  (run* (q)
    (fresh (x y)
      (=/= 5 x)
      (=/= `(,x ,y) `(5 6))
      (== `(,x ,y) q)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 5)))))))

(test "=/=-46"
  (run* (q)
    (fresh (x y)
      (=/= 5 x)
      (=/= `( ,y ,x) `(6 5))
      (== `(,x ,y) q)))
  '(#s(Ans ((_.0 _.1)) ((=/= ((_.0 5)))))))

(test "=/=-47"
  (run* (x)
    (fresh (y z)
      (=/= x `(,y 2))
      (== x `(,z 2))))
  '(((_.0 2))))

(test "=/=-48"
  (run* (x)
    (fresh (y z)
      (=/= x `(,y 2))
      (== x `((,z) 2))))
  '((((_.0) 2))))

(test "=/=-49"
  (run* (x)
    (fresh (y z)
      (=/= x `((,y) 2))
      (== x `(,z 2))))
  '(((_.0 2))))

(test "=/=-50"
  (run 1 (q) (=/= q #f))
  '(#s(Ans (_.0) ((=/= ((#f _.0)))))))

(test "non watch-var pair implies satisfied"
  (run 1 (a b c d)
       (=/= (cons a c)
            (cons b d))
       (== c '(1 . 2))
       (== d '(1 . 3)))
  '((_.0 _.1 (1 . 2) (1 . 3))))

(test "null, pair, and atomic types order correctly in =/= reification"
  (run 1 (q)
    (=/= q '())
    (=/= q '(foo))
    (=/= q 5))
  '(#s(Ans (_.0) ((=/= ((() _.0)) ((_.0 (foo))) ((_.0 5)))))))