#lang plai

(require "grammars.rkt")

;; Analizador sintáctico.
;; Regresa el árbol de sintaxis abstracto asociado a la sintaxis concreta.
;; parse: s-expression -> CFWBAE/L.
(define (parse sexp)
   #| Aquí va su código. |#)

;; Realiza un mapeo entre las operaciones del lenguaje anfitrión y el lenguaje objetivo.
;; elige: symbol -> procedure
(define (elige s)
   (match s
      ['+    +]
      ['-    -]
      ['*    *]
      ['/    /]
      ['%    mmodulo]
      ['min  min]
      ['max  max]
      ['pow  mexpt]
      ['sqrt sqrt]
      ['<    <]
      ['<=   <=]
      ['=    mequal?]
      ['/=   not-equal?]
      ['>    >]
      ['>=   >=]
      ['not  not]
      ['and  mand]
      ['or   mor]))
