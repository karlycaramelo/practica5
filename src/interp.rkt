#lang plai

(require "grammars.rkt")

;; Analizador semántico.
;; Interpreta el árbol de sintaxis abstracta generado por el desendulzador.
;; interp: CFBAE/L -> CFBAE/L-Value
(define (interp expr env)
    #| Aquí va su código. |#)

;; Busca el valor de un identificador en el ambiente.
;; Si el identificador no se encuentra, se genera el error "Identificador libre".
;; lookup: symbol Env -> FBAE-Value
(define (lookup id env)
    (match env
        [(mtSub) (error 'interp "Identificador libre")]
        [(aSub name value rest-env) (if (symbol=? name id) value (lookup id rest-env))]))

;; Fuerza la evaluación de un punto estricto.
;; strict: CFBAE/L-Value -> CFBAE/L-Value
(define (strict expr)
    (match expr
        [(exprV expr env) (strict (interp expr env))]
        [else expr]))
