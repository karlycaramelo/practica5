#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")
(require "interp.rkt")

;; Función encargada de ejecutar el intérprete para que el usuario interactúe con el lenguaje. Para
;; diferenciar el prompt de Racket del nuestro, usamos "(λ)". Aprovechamos los aspectos imperativos
;; del lenguaje para esta función.
;; ejecuta: void
(define (ejecuta)
    (begin
        (display "(λ) ")
        (define lexemas (read))
        (cond 
            [(equal? lexemas '{exit}) (display "")]
            [(equal? (car lexemas) '!)
                (begin 
                    (with-handlers ([exn:fail? (lambda (exn) (displayln (exn-message exn)))])
                        (let ([resultado (strict (interp (desugar (parse lexemas)) (mtSub)))])
                            (match resultado
                                [(numV n) (displayln n)]
                                [(boolV #t) (displayln "true")]
                                [(boolV #f) (displayln "false")]
                                [(closureV _ _ _) (displayln "#<function>")])))
                        (ejecuta))]
            [else
                (begin 
                    (with-handlers ([exn:fail? (lambda (exn) (displayln (exn-message exn)))])
                        (let ([resultado (interp (desugar (parse lexemas)) (mtSub))])
                            (match resultado
                                [(numV n) (displayln n)]
                                [(boolV #t) (displayln "true")]
                                [(boolV #f) (displayln "false")]
                                [(closureV _ _ _) (displayln "#<function>")]
                                [(exprV expr _) (displayln "#<promise>")])))
                        (ejecuta))])))

;; Llamada a la función
(ejecuta)
