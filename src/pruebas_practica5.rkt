#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")
(require "interp.rkt")

(print-only-errors)
(plai-ignore-exn-strings #t)
(test-inexact-epsilon 2)

#| Módulo de pruebas unitarias para la Práctica 5 |#

;; Expresiones de prueba.

(define expr01 'foo)
(define expr02 '1729)
(define expr03 '12.46)
(define expr04 
    '{+ {- 1 2 3} 
        {* 4 5 6} 
        {/ 20 2 2} 
        {% 20 2 3} 
        {min 1 7 2 9} 
        {max 1 8 3 5} 
        {pow 2 3 4} 
        {sqrt 81}})
(define expr05
    '{with {{a 2} {b 3} {c 4}}
        {+ a b c}})
(define expr06
    '{with {{a {+ 2 3}} {b {* 7 4}} {c {max 1 7 2}}}
        {sqrt {% a b c}}})
(define expr07
    '{with {{a 2} {b 3} {c {+ 2 3}} {d {with {{e 1}} e}}}
        {max a b c d}})
(define expr08
    '{with* {{a 2} {b {+ a a}}}
        b})
(define expr09
    '{with* {{a 2} {b {+ a a}} {c {* a b}}}
        {with* {{d c} {e d}}
            {+ a b c d e}}})
(define expr10
    '{with* {{a x} {b {+ a a}}}
        {with {{x b}}
            x}})
(define expr11
    'false)
(define expr12
    'true)
(define expr13
    '{not {and {or {< 1 2 3} {<= 4 5 6} {= 7 7 7}} {or {/= 8 9 10} {> 11 12 13} {>=  14 15 16}}}})
(define expr14
    '{fun {x} x})
(define expr15
    '{fun {b h} {/ {* b h} 2}})
(define expr16
    '{fun {a b c} {/ {+ {* b -1} {sqrt {- {pow b 2} {* 4 a c}}}} {* 2 a}}})
(define expr17
    '{{fun {x} x} 1729})
(define expr18
    '{{fun {b h} {/ {* b h} 2}} 4 2})
(define expr19
    '{{fun {a b c} {/ {+ {* b -1} {sqrt {- {pow b 2} {* 4 a c}}}} {* 2 a}}} 1 7 10})
(define expr20
    '{with* {{a 2} {b 3} {c 4}
            {foo {fun {x y z} {+ a b c x y z}}}}
        {with {{a 5} {b 6} {c 7}}
            {foo 10 20 30}}})
(define expr21
    '{if {> 10 2}
         true
         false})
(define expr22
    '{with {{a 2} {b 4} {c 3}}
        {if {> a 2}
            {if {> a c}
                a
                c}
            {if {> b c}
                b
                c}}})
(define expr23
    '{cond
        {{> 3 5} true}
        {{< 5 3} true}
        {else false}})
(define expr24
    '{with* {{disc {fun {a b c} {- {pow b 2} {* 4 a c}}}}
            {valida {fun {x y z}
              {with* {{d {disc x y z}}}
                {cond
                  {{> d 0} 1}
                  {{= d 0} 2}
                  {else 3}}}}}}
        {valida 5 8 7}})
(define expr25
    '{with {{a 3} {b {/ 10 0}}}
        a})

;; Pruebas auxiliares.

(test (mmodulo 0) 0)
(test (mmodulo 10 2) 0)
(test (mmodulo 10 3) 1)
(test (mmodulo 1729 10 3) 0)
(test (mmodulo 1729 10 4) 1)

(test (mexpt 0) 0)
(test (mexpt 2 3 4) 4096)
(test (mexpt 2 3 4 5) 1152921504606846976)
(test (mexpt 0 2 3 4 5) 0)
(test (mexpt 1 2 3 4 5) 1)

(test (mequal? 'a 'a 'a) #t)
(test (mequal? 1 2 3) #f)
(test (mequal? #t) #t)
(test (mequal? "uno") #t)
(test (mequal? "a" "a" "a") #t)

(test (not-equal? 'a 'a 'a) #f)
(test (not-equal? 1 2 3) #t)
(test (not-equal? #f) #f)
(test (not-equal? "uno") #f)
(test (not-equal? "a" "a" "a") #f)

(test (mand) #t)
(test (mand #t) #t)
(test (mand #f) #f)
(test (mand #t #t) #t)
(test (mand #t 23) #t)

(test (mor) #f)
(test (mor #t) #t)
(test (mor #f) #f)
(test (mor #t #t) #t)
(test (mor #t 23) #t)

;; Pruebas para el ejercicio 5.1

(test (parse expr01) (idS 'foo))
(test (parse expr02) (numS 1729))
(test (parse expr03) (numS 12.46))
(test (parse expr04) 
    (opS + 
        (list (opS - (list (numS 1) (numS 2) (numS 3))) 
              (opS * (list (numS 4) (numS 5) (numS 6))) 
              (opS / (list (numS 20) (numS 2) (numS 2)))
              (opS mmodulo (list (numS 20) (numS 2) (numS 3)))
              (opS min (list (numS 1) (numS 7) (numS 2) (numS 9)))
              (opS max (list (numS 1) (numS 8) (numS 3) (numS 5)))
              (opS mexpt (list (numS 2) (numS 3) (numS 4)))
              (opS sqrt (list (numS 81))))))
(test (parse expr05)
    (withS (list (binding 'a (numS 2)) (binding 'b (numS 3)) (binding 'c (numS 4))) 
          (opS + (list (idS 'a) (idS 'b) (idS 'c)))))
(test (parse expr06)
    (withS 
        (list 
            (binding 'a (opS + (list (numS 2) (numS 3)))) 
            (binding 'b (opS * (list (numS 7) (numS 4)))) 
            (binding 'c (opS max (list (numS 1) (numS 7) (numS 2)))))
        (opS sqrt (list (opS mmodulo (list (idS 'a) (idS 'b) (idS 'c)))))))
(test (parse expr07)
    (withS
        (list
            (binding 'a (numS 2))
            (binding 'b (numS 3))
            (binding 'c (opS + (list (numS 2) (numS 3))))
            (binding 'd (withS (list (binding 'e (numS 1))) (idS 'e))))
        (opS max (list (idS 'a) (idS 'b) (idS 'c) (idS 'd)))))
(test (parse expr08)
    (withS*
        (list
            (binding 'a (numS 2))
            (binding 'b (opS + (list (idS 'a) (idS 'a)))))
        (idS 'b)))
(test (parse expr09)
    (withS*
        (list
            (binding 'a (numS 2))
            (binding 'b (opS + (list (idS 'a) (idS 'a))))
            (binding 'c (opS * (list (idS 'a) (idS 'b)))))
        (withS*
            (list
                (binding 'd (idS 'c))
                (binding 'e (idS 'd)))
            (opS + (list (idS 'a) (idS 'b) (idS 'c) (idS 'd) (idS 'e))))))
(test (parse expr10)
    (withS*
        (list
            (binding 'a (idS 'x))
            (binding 'b (opS + (list (idS 'a) (idS 'a)))))
        (withS
            (list
                (binding 'x (idS 'b)))
            (idS 'x))))
(test (parse expr11) (boolS #f))
(test (parse expr12) (boolS #t))
(test (parse expr13)
    (opS
    not
     (list
      (opS
       mand
       (list
        (opS
         mor
         (list
          (opS < (list (numS 1) (numS 2) (numS 3)))
          (opS <= (list (numS 4) (numS 5) (numS 6)))
          (opS mequal? (list (numS 7) (numS 7) (numS 7)))))
        (opS
         mor
         (list
          (opS not-equal? (list (numS 8) (numS 9) (numS 10)))
          (opS > (list (numS 11) (numS 12) (numS 13)))
          (opS >= (list (numS 14) (numS 15) (numS 16))))))))))
(test (parse expr14) (funS '(x) (idS 'x)))
(test (parse expr15)
    (funS
     '(b h)
     (opS
      /
      (list (opS * (list (idS 'b) (idS 'h))) (numS 2)))))
(test (parse expr16)
    (funS
     '(a b c)
     (opS
      /
      (list
       (opS
        +
        (list
         (opS * (list (idS 'b) (numS -1)))
         (opS
          sqrt
          (list
           (opS
            -
            (list
             (opS mexpt (list (idS 'b) (numS 2)))
             (opS * (list (numS 4) (idS 'a) (idS 'c)))))))))
       (opS * (list (numS 2) (idS 'a)))))))
(test (parse expr17) (appS (funS '(x) (idS 'x)) (list (numS 1729))))
(test (parse expr18)
    (appS
     (funS
      '(b h)
      (opS
       /
       (list (opS * (list (idS 'b) (idS 'h))) (numS 2))))
     (list (numS 4) (numS 2))))
(test (parse expr19)
    (appS
     (funS
      '(a b c)
      (opS
       /
       (list
        (opS
         +
         (list
          (opS * (list (idS 'b) (numS -1)))
          (opS
           sqrt
           (list
            (opS
             -
             (list
              (opS mexpt (list (idS 'b) (numS 2)))
              (opS * (list (numS 4) (idS 'a) (idS 'c)))))))))
        (opS * (list (numS 2) (idS 'a))))))
     (list (numS 1) (numS 7) (numS 10))))
(test (parse expr20)
    (withS*
     (list
      (binding 'a (numS 2))
      (binding 'b (numS 3))
      (binding 'c (numS 4))
      (binding
       'foo
       (funS
        '(x y z)
        (opS
         +
         (list (idS 'a) (idS 'b) (idS 'c) (idS 'x) (idS 'y) (idS 'z))))))
     (withS
      (list (binding 'a (numS 5)) (binding 'b (numS 6)) (binding 'c (numS 7)))
      (appS (idS 'foo) (list (numS 10) (numS 20) (numS 30))))))
(test (parse expr21)
    (ifS (opS > (list (numS 10) (numS 2))) (boolS #t) (boolS #f)))
(test (parse expr22)
    (withS (list 
      (binding 'a (numS 2)) 
      (binding 'b (numS 4)) 
      (binding 'c (numS 3))) 
      (ifS (opS > (list (idS 'a) (numS 2))) 
           (ifS (opS > (list (idS 'a) (idS 'c))) (idS 'a) (idS 'c)) 
           (ifS (opS > (list (idS 'b) (idS 'c))) (idS 'b) (idS 'c)))))
(test (parse expr23)
    (condS 
      (list
        (condition (opS > (list (numS 3) (numS 5))) (boolS #t))
        (condition (opS < (list (numS 5) (numS 3))) (boolS #t))
        (else-cond (boolS #f)))))
(test (parse expr24)
    (withS* (list
      (binding 'disc 
               (funS '(a b c) 
                     (opS - (list 
                          (opS mexpt (list (idS 'b) (numS 2))) 
                          (opS * (list (numS 4) (idS 'a) (idS 'c)))))))
      (binding 'valida
               (funS '(x y z)
                     (withS* (list 
                        (binding 'd (appS (idS 'disc) (list (idS 'x) (idS 'y) (idS 'z)))))
                        (condS (list
                            (condition (opS > (list (idS 'd) (numS 0))) (numS 1))
                            (condition (opS mequal? (list (idS 'd) (numS 0))) (numS 2))
                            (else-cond (numS 3))))))))
      (appS (idS 'valida) (list (numS 5) (numS 8) (numS 7)))))
(test (parse expr25)
    (withS (list
      (binding 'a (numS 3))
      (binding 'b (opS / (list (numS 10) (numS 0)))))
      (idS 'a)))

;; Pruebas para el ejercicio 5.2

(test (desugar (parse expr01)) (id 'foo))
(test (desugar (parse expr02)) (num 1729))
(test (desugar (parse expr03)) (num 12.46))
(test (desugar (parse expr04))
    (op
     +
     (list
      (op - (list (num 1) (num 2) (num 3)))
      (op * (list (num 4) (num 5) (num 6)))
      (op / (list (num 20) (num 2) (num 2)))
      (op mmodulo (list (num 20) (num 2) (num 3)))
      (op min (list (num 1) (num 7) (num 2) (num 9)))
      (op max (list (num 1) (num 8) (num 3) (num 5)))
      (op mexpt (list (num 2) (num 3) (num 4)))
      (op sqrt (list (num 81))))))
(test (desugar (parse expr05))
    (app
     (fun '(a b c) (op + (list (id 'a) (id 'b) (id 'c))))
     (list (num 2) (num 3) (num 4))))
(test (desugar (parse expr06))
    (app
     (fun
      '(a b c)
      (op
       sqrt
       (list (op mmodulo (list (id 'a) (id 'b) (id 'c))))))
     (list
      (op + (list (num 2) (num 3)))
      (op * (list (num 7) (num 4)))
      (op max (list (num 1) (num 7) (num 2))))))
(test (desugar (parse expr07))
    (app
     (fun '(a b c d) (op max (list (id 'a) (id 'b) (id 'c) (id 'd))))
     (list
      (num 2)
      (num 3)
      (op + (list (num 2) (num 3)))
      (app (fun '(e) (id 'e)) (list (num 1))))))
(test (desugar (parse expr08))
    (app
     (fun
      '(a)
      (app (fun '(b) (id 'b)) (list (op + (list (id 'a) (id 'a))))))
     (list (num 2))))
(test (desugar (parse expr09))
    (app
     (fun
      '(a)
      (app
       (fun
        '(b)
        (app
         (fun
          '(c)
          (app
           (fun
            '(d)
            (app
             (fun
              '(e)
              (op + (list (id 'a) (id 'b) (id 'c) (id 'd) (id 'e))))
             (list (id 'd))))
           (list (id 'c))))
         (list (op * (list (id 'a) (id 'b))))))
       (list (op + (list (id 'a) (id 'a))))))
     (list (num 2))))
(test (desugar (parse expr10))
    (app
     (fun
      '(a)
      (app
       (fun '(b) (app (fun '(x) (id 'x)) (list (id 'b))))
       (list (op + (list (id 'a) (id 'a))))))
     (list (id 'x))))
(test (desugar (parse expr11)) (bool #f))
(test (desugar (parse expr12)) (bool #t))
(test (desugar (parse expr13))
    (op
     not
     (list
      (op
       mand
       (list
        (op
         mor
         (list
          (op < (list (num 1) (num 2) (num 3)))
          (op <= (list (num 4) (num 5) (num 6)))
          (op mequal? (list (num 7) (num 7) (num 7)))))
        (op
         mor
         (list
          (op not-equal? (list (num 8) (num 9) (num 10)))
          (op > (list (num 11) (num 12) (num 13)))
          (op >= (list (num 14) (num 15) (num 16))))))))))
(test (desugar (parse expr14)) (fun '(x) (id 'x)))
(test (desugar (parse expr15))
    (fun
     '(b h)
     (op / (list (op * (list (id 'b) (id 'h))) (num 2)))))
(test (desugar (parse expr16))
    (fun
     '(a b c)
     (op
      /
      (list
       (op
        +
        (list
         (op * (list (id 'b) (num -1)))
         (op
          sqrt
          (list
           (op
            -
            (list
             (op mexpt (list (id 'b) (num 2)))
             (op * (list (num 4) (id 'a) (id 'c)))))))))
       (op * (list (num 2) (id 'a)))))))
(test (desugar (parse expr17)) (app (fun '(x) (id 'x)) (list (num 1729))))
(test (desugar (parse expr18))
    (app
     (fun
      '(b h)
      (op
       /
       (list (op * (list (id 'b) (id 'h))) (num 2))))
     (list (num 4) (num 2))))
(test (desugar (parse expr19))
    (app
     (fun
      '(a b c)
      (op
       /
       (list
        (op
         +
         (list
          (op * (list (id 'b) (num -1)))
          (op
           sqrt
           (list
            (op
             -
             (list
              (op mexpt (list (id 'b) (num 2)))
              (op * (list (num 4) (id 'a) (id 'c)))))))))
        (op * (list (num 2) (id 'a))))))
     (list (num 1) (num 7) (num 10))))
(test (desugar (parse expr20))
    (app
     (fun
      '(a)
      (app
       (fun
        '(b)
        (app
         (fun
          '(c)
          (app
           (fun
            '(foo)
            (app
             (fun '(a b c) (app (id 'foo) (list (num 10) (num 20) (num 30))))
             (list (num 5) (num 6) (num 7))))
           (list
            (fun
             '(x y z)
             (op
              +
              (list (id 'a) (id 'b) (id 'c) (id 'x) (id 'y) (id 'z)))))))
         (list (num 4))))
       (list (num 3))))
     (list (num 2))))
(test (desugar (parse expr21))
    (iF (op > (list (num 10) (num 2))) (bool #t) (bool #f)))
(test (desugar (parse expr22))
    (app 
        (fun '(a b c) 
             (iF (op > (list (id 'a) (num 2))) 
                 (iF (op > (list (id 'a) (id 'c))) (id 'a) (id 'c)) 
                 (iF (op > (list (id 'b) (id 'c))) (id 'b) (id 'c))))
        (list (num 2) (num 4) (num 3))))
(test (desugar (parse expr23))
    (iF (op > (list (num 3) (num 5)))
        (bool #t)
        (iF (op < (list (num 5) (num 3))) (bool #t) (bool #f))))
(test (desugar (parse expr24))
    (app
     (fun
        '(disc)
        (app
         (fun '(valida) (app (id 'valida) (list (num 5) (num 8) (num 7))))
         (list
          (fun
           '(x y z)
           (app
            (fun
             '(d)
             (iF
              (op > (list (id 'd) (num 0)))
              (num 1)
              (iF (op mequal? (list (id 'd) (num 0))) (num 2) (num 3))))
            (list (app (id 'disc) (list (id 'x) (id 'y) (id 'z)))))))))
       (list
        (fun
         '(a b c)
         (op
          -
          (list
           (op mexpt (list (id 'b) (num 2)))
           (op * (list (num 4) (id 'a) (id 'c)))))))))
(test (desugar (parse expr25))
    (app
      (fun '(a b) (id 'a))
      (list (num 3) (op / (list (num 10) (num 0))))))

;; Pruebas para el ejercicio 5.3

(test/exn (interp (desugar (parse expr01)) (mtSub)) "Identificador libre")
(test (interp (desugar (parse expr02)) (mtSub)) (numV 1729))
(test (interp (desugar (parse expr03)) (mtSub)) (numV 12.46))
(test (interp (desugar (parse expr04)) (mtSub)) (numV 4235))
(test (interp (desugar (parse expr05)) (mtSub)) (numV 9))
(test (interp (desugar (parse expr06)) (mtSub)) (numV 2.23606797749979))
(test (interp (desugar (parse expr07)) (mtSub)) (numV 5))
(test (interp (desugar (parse expr08)) (mtSub)) 
    (exprV (op + (list (id 'a) (id 'a))) (aSub 'a (exprV (num 2) (mtSub)) (mtSub))))
(test (interp (desugar (parse expr09)) (mtSub)) (numV 30))
(test (interp (desugar (parse expr10)) (mtSub))
    (exprV (id 'b) 
           (aSub 'b 
                 (exprV (op + (list (id 'a) (id 'a))) 
                 (aSub 'a 
                       (exprV (id 'x) (mtSub)) (mtSub))) 
                       (aSub 'a (exprV (id 'x) (mtSub)) (mtSub)))))
(test (interp (desugar (parse expr11)) (mtSub)) (boolV #f))
(test (interp (desugar (parse expr12)) (mtSub)) (boolV #t))
(test (interp (desugar (parse expr13)) (mtSub)) (boolV #f))
(test (interp (desugar (parse expr14)) (mtSub)) (closureV '(x) (id 'x) (mtSub)))
(test (interp (desugar (parse expr15)) (mtSub))
    (closureV
     '(b h)
     (op / (list (op * (list (id 'b) (id 'h))) (num 2)))
     (mtSub)))
(test (interp (desugar (parse expr16)) (mtSub))
    (closureV
     '(a b c)
     (op
      /
      (list
       (op
        +
        (list
         (op * (list (id 'b) (num -1)))
         (op
          sqrt
          (list
           (op
            -
            (list
             (op mexpt (list (id 'b) (num 2)))
             (op * (list (num 4) (id 'a) (id 'c)))))))))
       (op * (list (num 2) (id 'a)))))
     (mtSub)))
(test (interp (desugar (parse expr17)) (mtSub)) (exprV (num 1729) (mtSub)))
(test (interp (desugar (parse expr18)) (mtSub)) (numV 4))
(test (interp (desugar (parse expr19)) (mtSub)) (numV -2))
(test (interp (desugar (parse expr20)) (mtSub)) (numV 69))
(test (interp (desugar (parse expr21)) (mtSub)) (boolV #t))
(test (interp (desugar (parse expr22)) (mtSub)) (exprV (num 4) (mtSub)))
(test (interp (desugar (parse expr23)) (mtSub)) (boolV #f))
(test (interp (desugar (parse expr24)) (mtSub)) (numV 3))
(test (interp (desugar (parse expr25)) (mtSub)) (exprV (num 3) (mtSub)))
